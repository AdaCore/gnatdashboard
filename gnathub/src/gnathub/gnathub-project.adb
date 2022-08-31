------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2022, AdaCore                     --
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
with Ada.Strings;
with Ada.Strings.Unbounded.Hash;

with Database.Orm;                        use Database.Orm;

with GNAT.Source_Info;

with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.VFS_Utils;                  use GNATCOLL.VFS_Utils;

with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;

with GNAThub.Configuration;
with GNAThub.Database;                    use GNAThub.Database;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;

with GPR2.Project.Source.Set;

with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Source_Info;

with GPR2_GNATCOLL_Projects;

package body GNAThub.Project is

   package GCP renames GPR2_GNATCOLL_Projects;

   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);

   Project_Tree : GPR2.Project.Tree.Object;
   Project_Env  : GPR2.Context.Object;
   --  GPR2 specificities

   Scenario_Variables : Scenario_Variables_Vector.Vector;
   --  The scenario variables passed on the command line

   Codepeer_Package   : constant Package_Id := +"codepeer";
   Database_Directory : constant Attribute_Id := +"database_directory";
   Output_Directory   : constant Attribute_Id := +"output_directory";

   Codepeer_Output_Directory : constant GPR2.Q_Attribute_Id :=
     (Codepeer_Package, Output_Directory);

   Codepeer_Database_Directory : constant GPR2.Q_Attribute_Id :=
     (Codepeer_Package, Database_Directory);

   package Project_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Detached_Resource,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   use Project_Map;

   Is_Initialized    : Boolean := False;
   --  False until Initialize is called

   Is_Project_Loaded : Boolean := False;
   --  False until Load_Project_Tree is called

   procedure Register_Custom_Attributes;
   --  Register GNATdashboard-specific attributes

   procedure Save_Project_Sources
     (Project     : GPR2.Project.View.Object;
      Project_Orm : Detached_Resource;
      Session     : Session_Type);
   --  Save source files and directories associated with a project.
   --  Session: The database session in which the requests will be made.

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
      return String (Project_Tree.Root_Project.Name);
   end Name;

   -------------------
   -- Artifacts_Dir --
   -------------------

   function Artifacts_Dir return Virtual_File is
   begin
      return Object_Dir;
   end Artifacts_Dir;

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir return Virtual_File is
      Project : GPR2.Project.View.Object renames Project_Tree.Root_Project;
   begin
      if Project.Kind not in K_Configuration | K_Abstract then
         return GPR2.Path_Name.Virtual_File
                 (Project_Tree.Root_Project.Object_Directory);
      else
         return GNATCOLL.VFS.Create (".");
      end if;
   end Object_Dir;

   ----------
   -- Path --
   ----------

   function Path return Virtual_File is
   begin
      return GPR2.Path_Name.Virtual_File
        (Project_Tree.Root_Project.Path_Name);
   end Path;

   -----------------
   -- Project_Dir --
   -----------------

   function Project_Dir return Virtual_File is
   begin
      return Path.Dir;
   end Project_Dir;

   ------------------------------
   -- Project_Output_Directory --
   ------------------------------

   function Project_Output_Directory return Virtual_File is
      Prj        : constant GPR2.Project.View.Object :=
                     Project_Tree.Root_Project;
      Output_Dir : Virtual_File := No_File;
   begin
      if Prj.Has_Package (Codepeer_Package) and then
        Prj.Has_Attribute (Codepeer_Output_Directory)
      then
         declare
            Dir : constant Filesystem_String :=
              Filesystem_String
                (Prj.Attribute (Codepeer_Output_Directory).Value.Text);
         begin
            Output_Dir := Create_From_Base (Dir, Project_Dir.Full_Name.all);
         end;
      end if;
      return Output_Dir;
   end Project_Output_Directory;

   --------------------------------
   -- Project_Database_Directory --
   --------------------------------

   function Project_Database_Directory return Virtual_File is
      Prj : constant GPR2.Project.View.Object :=  Project_Tree.Root_Project;
      DB_Dir : Virtual_File := No_File;
   begin
      if Prj.Has_Package (Codepeer_Package) and then
        Prj.Has_Attribute (Codepeer_Database_Directory)
      then
         declare
            Dir : constant Filesystem_String :=
              Filesystem_String
                (Prj.Attribute (Codepeer_Database_Directory).Value.Text);

         begin
            DB_Dir := Create_From_Base (Dir, Project_Dir.Full_Name.all);
         end;
      end if;

      return DB_Dir;
   end Project_Database_Directory;

   ------------
   -- Target --
   ------------

   function Target return String is
   begin
      if GNAThub.Configuration.Target /= "" then
         return GNAThub.Configuration.Target;
      end if;
      return GCP.Get_Target (Project_Tree);
   end Target;

   -------------
   -- Runtime --
   -------------

   function Runtime return String is
   begin
      if GNAThub.Configuration.Runtime /= "" then
         return GNAThub.Configuration.Runtime;
      else
         return String (Project_Tree.Root_Project.Tree.Runtime (Ada_Language));
      end if;
   end Runtime;

   ------------------------
   -- Property_As_String --
   ------------------------

   function Property_As_String
     (Property     : Attribute_Id;
      Package_Name : Package_Id := GNATdashboard_Package;
      Index        : String := "") return String is
   begin
      return GCP.Attribute_Value
        (Project_Tree.Root_Project, Property, Package_Name, Index);
   end Property_As_String;

   ----------------------
   -- Property_As_List --
   ----------------------

   function Property_As_List
     (Property     : Attribute_Id;
      Package_Name : Package_Id := GNATdashboard_Package;
      Index        : String := "") return String_List_Access is
   begin
      return GCP.Attribute_Value
        (Project_Tree.Root_Project, Property, Package_Name, Index);
   end Property_As_List;

   --------------------------------
   -- Register_Custom_Attributes --
   --------------------------------

   procedure Register_Custom_Attributes is
      procedure Internal_Register
        (Key : Attribute_Id;
         Pack : Package_Id := GNATdashboard_Package;
         Is_List : Boolean := False);
      --  Register an attribute in the Dashboard package

      procedure Internal_Register
        (Key : Attribute_Id;
         Pack : Package_Id := GNATdashboard_Package;
         Is_List : Boolean := False)
      is
      begin
         if not GPR2.Project.Registry.Pack.Exists (Pack) then
            GPR2.Project.Registry.Pack.Add
              (Pack, GPR2.Project.Registry.Pack.Everywhere);
         end if;

         GPR2.Project.Registry.Attribute.Add
           (Name                 => (Pack, Key),
            Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
            Value                =>
              (if Is_List
               then GPR2.Project.Registry.Attribute.List
               else GPR2.Project.Registry.Attribute.Single),
            Value_Case_Sensitive => False,
            Is_Allowed_In        =>
              GPR2.Project.Registry.Attribute.Everywhere);
      end Internal_Register;

      Project_Name       : constant GPR2.Attribute_Id := +"Project_Name";
      Project_Version    : constant GPR2.Attribute_Id := +"Project_Version";
      Project_Key        : constant GPR2.Attribute_Id := +"Project_Key";
      Source_Encoding    : constant GPR2.Attribute_Id := +"Source_Encoding";
      Local_Repository   : constant GPR2.Attribute_Id := +"Local_Repository";
      Plugins            : constant GPR2.Attribute_Id := +"Plugins";
      Plugins_Off        : constant GPR2.Attribute_Id := +"Plugins_Off";

   begin
      Internal_Register (Project_Name);
      Internal_Register (Project_Version);
      Internal_Register (Project_Key);

      Internal_Register (Source_Encoding);

      Internal_Register (Local_Repository);

      Internal_Register (Plugins, Is_List => True);
      Internal_Register (Plugins_Off, Is_List => True);

      Internal_Register (Output_Directory, Codepeer_Package);
      Internal_Register (Database_Directory, Codepeer_Package);
   end Register_Custom_Attributes;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Trace (Me, "Register custom GNATdashboard package attributes");
      Register_Custom_Attributes;

      Is_Initialized := True;
   end Initialize;

   ---------
   -- Load--
   ---------

   procedure Load (Path : String)
   is
      Project_File : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (GPR2.Project.Ensure_Extension
                            (GPR2.Filename_Optional (Path)));
   begin
      Trace (Me, "Load project file " & Path);

      Project_Tree.Load_Autoconf
        (Filename => Project_File,
         Context  => Project_Env,
         Subdirs  => GPR2.Optional_Name_Type (GNAThub.Configuration.Subdirs),
         Target   => GPR2.Optional_Name_Type (GNAThub.Configuration.Target));

      --  load all view's sources

      Project_Tree.Update_Sources (Backends => GPR2.Source_Info.No_Backends);

      GPR2.Log.Output_Messages
        (Log         => Project_Tree.Log_Messages.all,
         Information => not Configuration.Quiet,
         Warning     => Configuration.Verbose,
         Error       => True,
         Lint        => False);

      Is_Project_Loaded := True;

      Trace (Me, "Project """ & Name & """ loaded");
   exception
      when Project_Error =>
         GPR2.Log.Output_Messages
           (Log         => Project_Tree.Log_Messages.all,
            Information => not Configuration.Quiet,
            Warning     => Configuration.Verbose,
            Error       => True,
            Lint        => False);
         raise Fatal_Error with "Failed to load project file: " & Path;

   end Load;

   ----------------------------
   -- Get_Scenario_Variables --
   ----------------------------

   function Get_Scenario_Variables return Scenario_Variables_Vector.Vector is
   begin
      return Scenario_Variables;
   end Get_Scenario_Variables;

   ----------------
   -- Update_Env --
   ----------------

   procedure Update_Env (Key, Value : String) is
   begin
      Trace (Me, "Update project environment: " & Key & " = " & Value);
      Project_Env.Insert (GPR2.Name_Type (Key), Value);

      --  Store the scenario variable that was parsed
      Scenario_Variables.Append
        ((To_Unbounded_String (Key), To_Unbounded_String (Value)));
   end Update_Env;

   ----------
   -- File --
   ----------

   function File (Name : String) return Virtual_File
   is
      File : constant Virtual_File := GCP.Create
        (Project_Tree, Base_Name (+Name));
   begin
      if File = No_File then
         return Create (Full_Filename => +Name);
      end if;

      return File;
   end File;

   ------------------
   -- All_Projects --
   ------------------

   function All_Projects return GPR2.Project.View.Vector.Object is
   begin
      return Project_Tree.Ordered_Views;
   end All_Projects;

   --------------------------
   -- Save_Project_Sources --
   --------------------------

   procedure Save_Project_Sources
     (Project     : GPR2.Project.View.Object;
      Project_Orm : Detached_Resource;
      Session     : Session_Type)
   is
      Directory_Orm : Detached_Resource;
      File_Orm      : Detached_Resource;

      use GPR2.Project.Source.Set;
   begin
      if Project.Is_Defined then
         Trace (Me, "Save project sources: " & String (Project.Name));
      else
         Trace (Me, "Save project sources: undefined");
      end if;

      --  Initialisation
      Directory_Orm := No_Detached_Resource;

      for Source of Project.Sources loop
         --  Save source directory
         if (not (Directory_Orm = No_Detached_Resource)
             and then GPR2.Filename_Type (Directory_Orm.Name)
             /= GPR2.Filename_Type (Source.Path_Name.Dir_Name))
           or else
             Directory_Orm = No_Detached_Resource
         then
            --  Save source directory and tree.
            Directory_Orm := Create_And_Save_Resource
              (Name => String (Source.Path_Name.Dir_Name),
               Kind => Kind_Directory,
               Session => Session);
            Save_Resource_Tree (Directory_Orm, Project_Orm, Session);

            Trace (Me, "New source directory: " &
                     String (Source.Path_Name.Dir_Name));
         end if;

         --  Save source file, and source tree
         File_Orm := Create_And_Save_Resource
           (Name => String (Source.Path_Name.Value),
            Kind => Kind_File,
            Session => Session);
         Save_Resource_Tree (File_Orm, Directory_Orm, Session);

         Trace (Me, "New source file: " & String (Source.Path_Name.Value));
      end loop;
   end Save_Project_Sources;

   -----------------------
   -- Save_Project_Tree --
   -----------------------

   procedure Save_Project_Tree is
      Project_Orm_By_Name : Project_Map.Map;
      Project_Orm         : Detached_Resource;
      Parent_Project_Orm  : Detached_Resource;

      function Get_Project_Orm
        (Project : GPR2.Project.View.Object;
         Session : Session_Type) return Detached_Resource;

      ---------------------
      -- Get_Project_Orm --
      ---------------------

      function Get_Project_Orm
        (Project : GPR2.Project.View.Object;
         Session : Session_Type) return Detached_Resource
      is
         Orm            : Detached_Resource;
         Project_Cursor : Project_Map.Cursor;
      begin
         Project_Cursor := Project_Orm_By_Name.Find
           (To_Unbounded_String (String (Project.Name)));

         --  Check if project was in the Map, if not create it and
         --  add it to the map
         if Project_Cursor = Project_Map.No_Element then
            Orm := Create_And_Save_Resource (Name => String (Project.Name),
                                             Kind => Kind_Project,
                                             Session => Session);
            Project_Orm_By_Name.Insert
              (Key => To_Unbounded_String (String (Project.Name)),
               New_Item => Orm);
         else
            Orm := Project_Orm_By_Name (Project_Cursor);
         end if;

         return Orm;
      end Get_Project_Orm;

   begin
      declare
         Session : constant Session_Type := Get_New_Session;
      begin
         --  All projects
         for Project of Project_Tree.Ordered_Views loop
            --  Retrieve Project DB object representation
            Project_Orm := Get_Project_Orm (Project, Session);

            declare
               Parents : constant GPR2.Project.View.Set.Object
                           := GPR2_GNATCOLL_Projects
                             .Find_All_Projects_Importing
                               (Project      => Project,
                                Direct_Only  => True);

               --  All parents' current project
            begin
               if Parents.Is_Empty then
                  --  Save Root project, with no parent
                  Save_Resource_Tree
                    (Child => Project_Orm,
                     Parent => No_Detached_Resource,
                     Session => Session);
               else
                  --  All parents' current project
                  for Parent_Project of Parents loop
                     Parent_Project_Orm := Get_Project_Orm
                       (Parent_Project, Session);
                     Save_Resource_Tree
                       (Project_Orm, Parent_Project_Orm, Session);
                  end loop;
               end if;
            end;
            --  Save Project's source directories and files
            Save_Project_Sources (Project, Project_Orm, Session);
         end loop;

         Session.Commit;
      exception
         when E : others =>
            --  Make sure to close the session.
            Session.Commit;
            Trace (Me, E);
      end;
   end Save_Project_Tree;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Loaded then
         Project_Tree.Unload;
         Is_Project_Loaded := False;

      end if;
   end Finalize;

end GNAThub.Project;
