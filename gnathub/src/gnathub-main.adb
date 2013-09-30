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

with Ada.Command_Line;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GPS.CLI_Kernels;            use GPS.CLI_Kernels;
with GPS.CLI_Utils;

with GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with GNAThub.Constants;          use GNAThub.Constants;
with GNAThub.Database;
with GNAThub.Project;
with GNAThub.Configuration;      use GNAThub.Configuration;
with GNAThub.Python_Api;
with GNAThub.Scripts;

   -------------
   -- GNAThub --
   -------------

function GNAThub.Main return Ada.Command_Line.Exit_Status is

   procedure Load_Plugin_Runner;
   --  Loads the plugin runner that executes every GNAThub plugin (both core
   --  and user plugins).

   procedure Load_Custom_Project_Attributes;
   --  Teaches the project file engine the GNATdashboard-specific attributes it
   --  needs to handle. These attributes are declared in an XML format and
   --  loaded with the GPS.parse_xml Python binding.

   procedure Create_Project_Directory_Env;
   --  Creates the following directories hierarchy, when needed:
   --
   --  <project_object_dir>/
   --   └── gnathub/
   --       ├── logs/
   --       └── sonar/
   --           └── sonar-project.properties

   procedure Finalize;
   --  Dispose of every allocated object.

   Kernel : constant GPS.CLI_Kernels.CLI_Kernel :=
              new GPS.CLI_Kernels.CLI_Kernel_Record;

   Config : GNAThub.Configuration.Command_Line;

   ------------------------------------
   -- Load_Custom_Project_Attributes --
   ------------------------------------

   procedure Load_Custom_Project_Attributes is
      Had_Errors : Boolean;
   begin
      Execute_File
        (Script       => Kernel.Scripts.Lookup_Scripting_Language ("python"),
         Filename     => Custom_Attributes_Definition_File.Display_Full_Name,
         Show_Command => False,
         Errors       => Had_Errors);

      if Had_Errors then
         raise Error with "Error creating GNAThub-specific project attributes";
      end if;
   end Load_Custom_Project_Attributes;

   ----------------------------------
   -- Create_Project_Directory_Env --
   ----------------------------------

   procedure Create_Project_Directory_Env
   is
      Object_Directory : constant Virtual_File :=
                           Kernel.Registry.Tree.Root_Project.Object_Dir;

      Logs_Directory   : constant Virtual_File :=
                           Create_From_Dir
                             (Object_Directory, Logs_Dir.Full_Name);

   begin
      --  Check whether the project object directory exists

      if Object_Directory = No_File then
         raise Error
           with "Project file forces no object directory, execution aborted";
      end if;

      --  Create both root and log directories

      if Is_Regular_File (Logs_Directory) then
         if not Is_Directory (Logs_Directory) then
            raise Error
              with Logs_Directory.Display_Full_Name & " is not a directory";
         end if;
      else
         Make_Dir (Logs_Directory, Recursive => True);
      end if;

   exception
      when E : Directory_Error =>
         raise Error with "Unexpected error while initializing environment: " &
                          Object_Directory.Display_Full_Name & ": " &
                          Exception_Information (E);

   end Create_Project_Directory_Env;

   ------------------------
   -- Load_Plugin_Runner --
   ------------------------

   procedure Load_Plugin_Runner is
      Had_Errors : Boolean;

      Python : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("python");

      Python_Path_List : constant array (1 .. 4) of Virtual_File :=
        (Core_Dir, Core_Plugin_Dir, User_Plugin_Dir, Core_Lib_Dir);

   begin
      --  Add the default search path for GNAThub plugins
      --  and GNAThub python API

      Log.Debug ("Loading plugins from: ");

      declare
         Python_Path : Unbounded_String :=
                         To_Unbounded_String ("sys.path = [");
      begin
         for Dir of Python_Path_List loop
            Log.Debug ("  - " & Dir.Display_Full_Name);
            Python_Path := Python_Path &
              To_Unbounded_String ("r'" & Dir.Display_Full_Name & "',");
         end loop;

         Python_Path := Python_Path & To_Unbounded_String ("] + sys.path");

         Execute_Command
           (Python,
            GNATCOLL.Arg_Lists.Create (To_String (Python_Path)),
            Show_Command => False,
            Hide_Output  => True,
            Errors       => Had_Errors);
      end;

      if Had_Errors then
         Log.Warn ("Could not set the search path for python");
      end if;

      --  Execute the pythn script that runs all the plugins

      Log.Info ("Running tools analysis");
      Log.Debug ("Executing plugin: " & Plugin_Runner.Display_Base_Name);

      return;

      Execute_File
        (Python,
         Filename     => Plugin_Runner.Display_Full_Name,
         Hide_Output  => False,
         Show_Command => False,
         Errors       => Had_Errors);

      if Had_Errors then
         raise Error with "Unexpected plug-in error";
      end if;
   end Load_Plugin_Runner;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      --  Finalize
      GPS.CLI_Utils.Destroy_Kernel_Context (Kernel);

      Config.Finalize;
      GNATCOLL.Traces.Finalize;
   end Finalize;

begin
   --  Initialization

   GNATCOLL.Traces.Parse_Config_File;

   GPS.CLI_Utils.Create_Kernel_Context
      (Kernel, Install_Semantic_Parser => False);

   Load_Custom_Project_Attributes;

   GNAThub.Python_Api.Initialize;

   --  Error output messages are handled by Parse function, according
   --  to the case of failure.

   Config.Initialize;
   Config.Parse (Kernel);

   --  Load project
   Log.Info ("Loading project: " & Config.Project_Name);
   GNAThub.Project.Load_Project_Tree (Config.Project_Name, Kernel);

   --  Create GNAThub directories in project object directory
   Log.Info ("Creating environment...");
   Create_Project_Directory_Env;

   --  Create Database
   Log.Info ("Creating application database...");

   GNAThub.Database.Initialize
      (Create_From_Dir
         (Kernel.Registry.Tree.Root_Project.Object_Dir, DB_File.Full_Name),
       Database_Schema_File);

   --  Save project tree to DB

   Log.Debug ("Writing project in dabatase");
   GNAThub.Project.Save_Project_Tree (Kernel.Registry.Tree.Root_Project);

   --  Run GNAThub plugins

   Load_Plugin_Runner;

   --  Run script if present on command line

   if Config.Script_Arg  /= "" then
      Log.Info ("Executing: " & Config.Script_Arg);
      GNAThub.Scripts.Execute_Script (Kernel, Config.Script_Arg);
   end if;

   Finalize;

   --  Log end statistics
   Log.Info ("Execution completed");

   return Ada.Command_Line.Success;

exception
   when Silent_Error =>
      Finalize;
      return Ada.Command_Line.Failure;

   when E : Error =>
      Finalize;
      Log.Fatal (Exception_Message (E));
      return Ada.Command_Line.Failure;

   when E : Fatal_Error =>
      Finalize;
      Log.Fatal (Exception_Information (E));
      return Ada.Command_Line.Failure;

end GNAThub.Main;
