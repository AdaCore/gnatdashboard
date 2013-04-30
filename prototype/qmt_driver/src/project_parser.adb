--
--  QMT Driver$
--  Copyright (C) 2012-2013, AdaCore
with Orm;                        use Orm;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with Database_Interface;         use Database_Interface;
with Ada.Containers.Hashed_Maps;
with Ada.Strings;
with GNAT.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;

package body Project_Parser is

   package Project_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Detached_Resource,
      Hash         => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   use Project_Map;
   --  ???

   procedure Save_Project_Sources (Project     : Project_Type;
                                   Project_Orm : Detached_Resource);
   --  ???

   function Load_Project_Tree (Project_File : Virtual_File) return Project_Tree
   is
      Version : GNAT.Strings.String_Access;
      Env     : Project_Environment_Access;
      Tree    : Project_Tree;
   begin
      Initialize (Env);
      Env.Set_Path_From_Gnatls ("gnatls", Version);
      Tree.Load (Project_File, Env);
      return Tree;

   exception
      when E : Invalid_Project =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Exception_Information (E));
         return Tree;

   end Load_Project_Tree;

   --------------------------
   -- Save_Project_Sources --
   --------------------------

   procedure Save_Project_Sources (Project     : Project_Type;
                                   Project_Orm : Detached_Resource)
   is
      Source_Dir          : Detached_Resource;
      Resource            : Detached_Resource;
      pragma Unreferenced (Resource);
      Files               : File_Array_Access;
   begin

      --  Initialisation
      Files := Project.Source_Files;
      Source_Dir := No_Detached_Resource;

      for F in Files'Range loop

         --  Save dource directory
         if (not (Source_Dir = No_Detached_Resource)
             and then not Ada.Strings.Equal_Case_Insensitive
               (Source_Dir.Name, Files (F).Display_Dir_Name))
           or else
             Source_Dir = No_Detached_Resource
         then

            Source_Dir := Save_Resource_And_Tree
              (Name => Files (F).Display_Dir_Name,
               Kind    => 1,
               Parent => Project_Orm);

         end if;

         --  Save source file
         Resource := Save_Resource_And_Tree
           (Name => Files (F).Display_Full_Name,
            Kind    => 2,
            Parent  => Source_Dir);
      end loop;

   end Save_Project_Sources;

   ------------------
   -- Load_Project --
   ------------------

   procedure Save_Project_Tree (Root_Project : Project_Type) is
      Project_By_Name     : Project_Map.Map;
      Iterator            : Project_Iterator;
      Project             : Project_Type;
      Project_Orm         : Detached_Resource;
      Parent_Iterator     : Project_Iterator;
      Parent_Project      : Project_Type;
      Parent_Project_Orm  : Detached_Resource;

      function Get_Project_Orm (Project : Project_Type)
                                return Detached_Resource;
      --  ???

      ---------------------
      -- Get_Project_Orm --
      ---------------------

      function Get_Project_Orm (Project : Project_Type)
                                return Detached_Resource
      is
         Orm                 : Detached_Resource;
         Project_Cursor      : Project_Map.Cursor;
      begin

         Project_Cursor := Project_By_Name.Find
           (To_Unbounded_String (Project.Name));
         if Project_Cursor = Project_Map.No_Element then
            Orm := Save_Resource (Name => Project.Name,
                                  Kind => 0);
            Project_By_Name.Insert (Key => To_Unbounded_String (Project.Name),
                                    New_Item => Orm);
         else
            Orm := Project_By_Name (Project_Cursor);
         end if;
         Put_Line ("   [DEBUG] Get Orm : " & Orm.Name);
         return Orm;
      end Get_Project_Orm;

   begin
      --  All projects
      Iterator := Root_Project.Start;
      loop
         Project := Current (Iterator);
         exit when Project = No_Project;

         Put_Line ("*** Project: " & Project.Name);

         Project_Orm := Get_Project_Orm (Project);

         --  All parents' current project
         Parent_Iterator := Project.Find_All_Projects_Importing
           (Direct_Only => True);

         if Current (Parent_Iterator) = No_Project then

            Save_Resource_Tree (Child => Project_Orm,
                                Parent => No_Detached_Resource);
         else

            loop
               Parent_Project := Current (Parent_Iterator);
               exit when Parent_Project = No_Project;

               Parent_Project_Orm := Get_Project_Orm (Parent_Project);
               Save_Resource_Tree (Project_Orm, Parent_Project_Orm);

               Put_Line ("     ->  " & Parent_Project.Name);

               Next (Parent_Iterator);
            end loop;

         end if;

         Save_Project_Sources (Project, Project_Orm);

         Next (Iterator);
      end loop;

   end Save_Project_Tree;

end Project_Parser;
