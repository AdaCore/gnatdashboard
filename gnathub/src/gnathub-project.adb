------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers;                      use Ada.Containers;
with Ada.Exceptions;                      use Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Equal_Case_Insensitive;

with GNAThub.Database;                    use GNAThub.Database;
with Orm;                                 use Orm;

package body GNAThub.Project is

   package Project_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Detached_Resource,
      Hash         => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   use Project_Map;
   --  ???

   procedure Save_Project_Sources
     (Project     : Project_Type;
      Project_Orm : Detached_Resource);
   --  ???

   -----------------------
   -- Load_Project_Tree --
   -----------------------

   procedure Load_Project_Tree
     (Path   : GNAT.Strings.String_Access;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record)
   is
      Project_File : constant Virtual_File := GNATCOLL.VFS.Create (+Path.all);
   begin
      Kernel.Registry.Tree.Load
        (Root_Project_Path => Project_File,
         Env => Kernel.Registry.Environment);
   exception
      when E : Invalid_Project =>
         raise Error with "Unable to load project file: " & Path.all & ": " &
                          Exception_Information (E);
   end Load_Project_Tree;

   --------------------------
   -- Save_Project_Sources --
   --------------------------

   procedure Save_Project_Sources
     (Project     : Project_Type;
      Project_Orm : Detached_Resource)
   is
      Directory_Orm : Detached_Resource;
      File_Orm      : Detached_Resource;
      Files         : File_Array_Access;

   begin
      --  Initialisation
      Files := Project.Source_Files;
      Directory_Orm := No_Detached_Resource;

      for F in Files'Range loop

         --  Save source directory
         if (not (Directory_Orm = No_Detached_Resource)
             and then not Ada.Strings.Equal_Case_Insensitive
               (Directory_Orm.Name, Files (F).Display_Dir_Name))
           or else
             Directory_Orm = No_Detached_Resource
         then
            --  Save source directory and tree.
            Directory_Orm := Create_And_Save_Resource
              (Name => Files (F).Display_Dir_Name,
               Kind => Kind_Directory);
            Save_Resource_Tree (Directory_Orm, Project_Orm);
         end if;

         --  Save source file, and source tree
         File_Orm := Create_And_Save_Resource
           (Name => Files (F).Display_Full_Name,
            Kind => Kind_File);
         Save_Resource_Tree (File_Orm, Directory_Orm);
      end loop;

   end Save_Project_Sources;

   ------------------
   -- Load_Project --
   ------------------

   procedure Save_Project_Tree (Root_Project : Project_Type) is
      Project_Orm_By_Name : Project_Map.Map;
      Iterator            : Project_Iterator;
      Project             : Project_Type;
      Project_Orm         : Detached_Resource;
      Parent_Iterator     : Project_Iterator;
      Parent_Project      : Project_Type;
      Parent_Project_Orm  : Detached_Resource;

      function Get_Project_Orm
        (Project : Project_Type) return Detached_Resource;
      --  ???

      ---------------------
      -- Get_Project_Orm --
      ---------------------

      function Get_Project_Orm
        (Project : Project_Type) return Detached_Resource
      is
         Orm                 : Detached_Resource;
         Project_Cursor      : Project_Map.Cursor;
      begin
         Project_Cursor := Project_Orm_By_Name.Find
           (To_Unbounded_String (Project.Name));

         --  Check if project was in the Map, if not create it and
         --  add it to the map
         if Project_Cursor = Project_Map.No_Element then
            Orm := Create_And_Save_Resource (Name => Project.Name,
                                             Kind => Kind_Project);
            Project_Orm_By_Name.Insert
              (Key => To_Unbounded_String (Project.Name),
               New_Item => Orm);
         else
            Orm := Project_Orm_By_Name (Project_Cursor);
         end if;

         return Orm;
      end Get_Project_Orm;

   begin
      --  All projects
      Iterator := Root_Project.Start;
      loop
         Project := Current (Iterator);
         exit when Project = No_Project;

         --  Retreive Project DB object representation
         Project_Orm := Get_Project_Orm (Project);

         --  All parents' current project
         Parent_Iterator := Project.Find_All_Projects_Importing
           (Direct_Only => True);

         --  Save Root project, with no parent
         if Current (Parent_Iterator) = No_Project then
            Save_Resource_Tree (Child => Project_Orm,
                                Parent => No_Detached_Resource);
         else

            loop
               Parent_Project := Current (Parent_Iterator);
               exit when Parent_Project = No_Project;

               Parent_Project_Orm := Get_Project_Orm (Parent_Project);
               Save_Resource_Tree (Project_Orm, Parent_Project_Orm);

               Next (Parent_Iterator);
            end loop;

         end if;

         --  Save Project's source directories and files
         Save_Project_Sources (Project, Project_Orm);

         Next (Iterator);
      end loop;

   end Save_Project_Tree;

end GNAThub.Project;
