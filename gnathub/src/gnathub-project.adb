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

with GNATCOLL.VFS_Utils;                  use GNATCOLL.VFS_Utils;

with GNAThub.Database;                    use GNAThub.Database;
with Orm;                                 use Orm;

package body GNAThub.Project is

   Package_Name : constant String := "dashboard";
   --  Package_Name MUST be in lower case to avoid Prj error "cannot
   --  register a package with a non unique name"

   Project_Tree : GNATCOLL.Projects.Project_Tree;
   Project_Env  : Project_Environment_Access;
   --  GNATCOLL.Projects specificities

   package Project_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Detached_Resource,
      Hash         => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   use Project_Map;

   Is_Initialized    : Boolean := False;
   --  False until Initialize is called

   Is_Project_Loaded : Boolean := False;
   --  False until Load_Project_Tree is called

   procedure Register_Custom_Attributes;
   --  Register GNATdashboard-specific attributes

   procedure Save_Project_Sources
     (Project     : Project_Type;
      Project_Orm : Detached_Resource);
   --  ???

   ------------
   -- Loaded --
   ------------

   function Loaded return Boolean is
   begin
      return Is_Project_Loaded;
   end Loaded;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Is_Initialized;
   end Initialized;

   ----------
   -- Name --
   ----------

   function Name return String is
   begin
      return Project_Tree.Root_Project.Name;
   end Name;

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir return Virtual_File is
   begin
      return Project_Tree.Root_Project.Object_Dir;
   end Object_Dir;

   ----------
   -- Path --
   ----------

   function Path return Virtual_File is
   begin
      return Project_Tree.Root_Project.Project_Path;
   end Path;

   ------------------------
   -- Property_As_String --
   ------------------------

   function Property_As_String (Property : String) return String is
   begin
      return Project_Tree.Root_Project.Attribute_Value
               (Attribute_Pkg_String'(Build (Package_Name, Property)),
                Default => "");
   end Property_As_String;

   ----------------------
   -- Property_As_List --
   ----------------------

   function Property_As_List (Property : String) return String_List_Access is
   begin
      return Project_Tree.Root_Project.Attribute_Value
               (Attribute_Pkg_List'(Build (Package_Name, Property)));
   end Property_As_List;

   -------------------------------
   -- Register_Extra_Attributes --
   -------------------------------

   procedure Register_Custom_Attributes is
      procedure Internal_Register (Key : String; Is_List : Boolean := False);
      --  Register an attribute in the Dashboard package

      procedure Internal_Register (Key : String; Is_List : Boolean := False) is
         Ret : constant String :=
                 Register_New_Attribute
                   (Name    => Key,
                    Pkg     => Package_Name,
                    Is_List => Is_List);
      begin
         if Ret /= "" then
            raise Project_Error with
              "Failed to register custom attribute " & Key;
         end if;
      end Internal_Register;

   begin
      Internal_Register ("Project_Name");
      Internal_Register ("Project_Version");
      Internal_Register ("Project_Key");

      Internal_Register ("Source_Encoding");

      Internal_Register ("Plugins", Is_List => True);
      Internal_Register ("Specific_Plugins", Is_List => True);
   end Register_Custom_Attributes;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      GNATCOLL.Projects.Initialize (Project_Env);
      Register_Custom_Attributes;

      Is_Initialized := True;
   end Initialize;

   ---------
   -- Load--
   ---------

   procedure Load (Path : GNAT.Strings.String_Access)
   is
      Project_File : constant Virtual_File := GNATCOLL.VFS.Create (+Path.all);
   begin
      Project_Tree.Load
        (Root_Project_Path => Project_File,
         Env               => Project_Env);

      Is_Project_Loaded := True;

   exception
      when E : Invalid_Project =>
         raise Error with "Unable to load project file: " & Path.all & ": " &
                          Exception_Information (E);

   end Load;

   ----------------
   -- Update_Env --
   ----------------

   procedure Update_Env (Key, Value : String) is
   begin
      Project_Env.Change_Environment (Key, Value);
   end Update_Env;

   ----------
   -- File --
   ----------

   function File (Name : String) return Virtual_File
   is
      File : constant Virtual_File := Project_Tree.Create (Base_Name (+Name));
   begin
      if File = No_File then
         return Create (Full_Filename => +Name);
      end if;

      return File;
   end File;

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

   procedure Save_Project_Tree is
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
      Iterator := Project_Tree.Root_Project.Start;

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

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Loaded then
         Project_Tree.Unload;
         Is_Project_Loaded := False;

         GNATCOLL.Projects.Finalize;
      end if;
   end Finalize;

end GNAThub.Project;
