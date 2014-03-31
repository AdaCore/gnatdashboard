------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2014, AdaCore                     --
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
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Source_Info;

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with GNAThub.Constants;          use GNAThub.Constants;
with GNAThub.Database;
with GNAThub.Project;
with GNAThub.Configuration;      use GNAThub.Configuration;
with GNAThub.Python;

   -------------
   -- GNAThub --
   -------------

function GNAThub.Main return Ada.Command_Line.Exit_Status is
   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);

   function Interpreter_Mode return Boolean;
   --  Whether to run GNAThub in interpreter mode (--exec) or normal mode.

   procedure Create_Or_Override_Database;
   --  Creates the GNAThub database and initializes it with the project
   --  information. Override any existing database.

   procedure Execute_Plugin_Runner;
   --  Loads the plugin runner that executes every GNAThub plugin (both core
   --  and user plugins).

   procedure Execute_User_Script;
   --  Loads and executes the script provided by the user throught the --exec
   --  switch from the command-line.

   procedure Create_Project_Directory_Env;
   --  Creates the following directories hierarchy, when needed:
   --
   --  <project_object_dir>/
   --   └── gnathub/
   --       ├── logs/
   --       └── sonar/
   --           └── sonar-project.properties

   procedure Initialize_Application;
   --  Initialize every module that needs initialization.

   procedure Finalize_Application;
   --  Dispose of every allocated object.

   ----------------------------------
   -- Create_Project_Directory_Env --
   ----------------------------------

   procedure Create_Project_Directory_Env
   is
      Object_Directory : constant Virtual_File :=
                           GNAThub.Project.Object_Dir;

      Logs_Directory   : constant Virtual_File :=
                           Create_From_Dir
                             (Object_Directory, Logs_Dir.Full_Name);

   begin
      --  Check whether the project object directory exists

      if Object_Directory = No_File then
         raise Fatal_Error
           with "Project file forces no object directory, execution aborted";
      end if;

      --  Create both root and log directories

      if Is_Regular_File (Logs_Directory) then
         if not Is_Directory (Logs_Directory) then
            raise Fatal_Error
              with Logs_Directory.Display_Full_Name & " is not a directory";
         end if;
      else
         Make_Dir (Logs_Directory, Recursive => True);
      end if;

   exception
      when E : Directory_Error =>
         raise Fatal_Error with
           "Cannot initialize environment: " &
           Object_Directory.Display_Full_Name & ": " &
           Exception_Information (E);

   end Create_Project_Directory_Env;

   ----------------------
   -- Interpreter_Mode --
   ----------------------

   function Interpreter_Mode return Boolean is
   begin
      return GNAThub.Configuration.Script /= "";
   end Interpreter_Mode;

   ---------------------------
   -- Execute_Plugin_Runner --
   ---------------------------

   procedure Execute_Plugin_Runner is
      Had_Errors : Boolean;

      Python_Path_List : constant array (1 .. 2) of Virtual_File :=
        (Core_Plugins_Dir, Extra_Plugins_Dir);

   begin
      --  Add the default search path for GNAThub plugins
      --  and GNAThub python API

      Log.Info (Me, "Loading plugins from: ");

      declare
         Python_Path : Unbounded_String :=
                         To_Unbounded_String ("sys.path.extend([");
      begin
         for Dir of Python_Path_List loop
            Log.Info (Me, "  + " & Dir.Display_Full_Name);
            Python_Path := Python_Path &
              To_Unbounded_String ("r'" & Dir.Display_Full_Name & "',");
         end loop;

         Python_Path := Python_Path & To_Unbounded_String ("])");

         GNAThub.Python.Execute (To_String (Python_Path), Had_Errors);
      end;

      if Had_Errors then
         Warn ("Failed to update sys.path");
      end if;

      --  Execute the python script that runs all the plugins

      Log.Info (Me, "Executing plugin: " & Plugin_Runner.Display_Base_Name);

      GNAThub.Python.Execute_File
        (Plugin_Runner.Display_Full_Name, Had_Errors);

      if Had_Errors then
         raise Fatal_Error with "Unexpected plug-in error";
      end if;
   end Execute_Plugin_Runner;

   -------------------------
   -- Execute_User_Script --
   -------------------------

   procedure Execute_User_Script is
      Script : constant String := GNAThub.Configuration.Script;
      Errors : Boolean := False;
   begin
      if Script /= "" then
         Info ("gnathub.python.execute " & Script);

         GNAThub.Python.Execute_File (Script, Errors);

         if Errors then
            raise Fatal_Error with "execution failed: " & Script;
         end if;
      end if;
   end Execute_User_Script;

   ---------------------------------
   -- Create_Or_Override_Database --
   ---------------------------------

   procedure Create_Or_Override_Database is
   begin
      Info ("gnathub.sql.create " & Database_File.Display_Full_Name);
      GNAThub.Database.Initialize
         (Create_From_Dir
            (GNAThub.Project.Object_Dir, Database_File.Full_Name),
         Remove_Previous_Database => True);

      Info ("gnathub.sql.store " & GNAThub.Configuration.Project);
      GNAThub.Project.Save_Project_Tree;
   end Create_Or_Override_Database;

   ----------------------------
   -- Initialize_Application --
   ----------------------------

   procedure Initialize_Application is
   begin
      --  Load the logging module
      GNAThub.Initialize_Logging;

      --  Configure GNAThub (through the command line). In particular, set the
      --  output verbosity (with the switches --quiet or --verbose) after the
      --  logging module initialization.
      GNAThub.Configuration.Initialize;

      --  Configure the Python VM that will run the plug-ins and user scripts
      GNAThub.Python.Initialize;

      --  Load the user's project file and store the configuration in memory
      GNAThub.Project.Initialize;
   end Initialize_Application;

   --------------------------
   -- Finalize_Application --
   --------------------------

   procedure Finalize_Application is
   begin
      GNAThub.Project.Finalize;
      GNAThub.Database.Finalize;
      GNAThub.Python.Finalize;
      GNAThub.Configuration.Finalize;

      GNATCOLL.Traces.Finalize;
   end Finalize_Application;

begin
   Initialize_Application;

   Log.Info (Me, "Loading project: " & GNAThub.Configuration.Project);
   GNAThub.Project.Load (GNAThub.Configuration.Project);

   Log.Info (Me, "Creating execution environment...");
   Create_Project_Directory_Env;

   if Interpreter_Mode then
      Log.Info (Me, "Connecting to existing database...");
      GNAThub.Database.Initialize
        (Create_From_Dir
           (GNAThub.Project.Object_Dir, Database_File.Full_Name),
        Remove_Previous_Database => False);

      Log.Info (Me, "Executing user script: " & Script);
      Execute_User_Script;

   else
      Log.Info (Me, "Creating local application database...");
      Create_Or_Override_Database;

      Log.Info (Me, "Executing Plugin Runner");
      Execute_Plugin_Runner;
   end if;

   Log.Info (Me, "Execution completed");
   Finalize_Application;

   return Ada.Command_Line.Success;

exception
   when Silent_Error =>
      Finalize_Application;

      return Ada.Command_Line.Failure;

   when E : Command_Line_Error =>
      Log.Exception_Raised (Me, E);

      --  NOTE: For the moment, GNAT.Command_Line.Try_Help is not available in
      --  stable-gnat (only in nightlies). To avoid the dependency to the daily
      --  gnat build, we currently manually display the Try_Help message.
      --
      --  This will need to be updated when stable-gnat's version is bumped and
      --  it provides the required symbol.
      --
      --  GNAT.Command_Line.Try_Help;
      Put_Line ("try ""gnathub --help"" for more information.");
      Finalize_Application;

      return Ada.Command_Line.Failure;

   when E : Fatal_Error =>
      Log.Exception_Raised (Me, E);
      Finalize_Application;

      return Ada.Command_Line.Failure;

end GNAThub.Main;
