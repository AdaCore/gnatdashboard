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

with GNAT.Command_Line;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
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

         GNAThub.Python.Execute (To_String (Python_Path), Had_Errors);
      end;

      if Had_Errors then
         Log.Warn ("Could not set the search path for python");
      end if;

      --  Execute the python script that runs all the plugins

      Log.Debug ("Executing plugin: " & Plugin_Runner.Display_Base_Name);

      GNAThub.Python.Execute_File
        (Plugin_Runner.Display_Full_Name, Had_Errors);

      if Had_Errors then
         raise Error with "Unexpected plug-in error";
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
         Log.Info ("gnathub.python.execute " & Script);

         GNAThub.Python.Execute_File (Script, Errors);

         if Errors then
            raise Error with "execution failed: " & Script;
         end if;
      end if;
   end Execute_User_Script;

   ---------------------------------
   -- Create_Or_Override_Database --
   ---------------------------------

   procedure Create_Or_Override_Database is
   begin
      Log.Info ("gnathub.sql.create " & Database_File.Display_Full_Name);
      GNAThub.Database.Initialize
         (Create_From_Dir
            (GNAThub.Project.Object_Dir, Database_File.Full_Name),
         Remove_Previous_Database => True);

      Log.Info ("gnathub.sql.save(" & GNAThub.Configuration.Project & ")");
      GNAThub.Project.Save_Project_Tree;
   end Create_Or_Override_Database;

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
   GNATCOLL.Traces.Parse_Config_File;

   GNAThub.Python.Initialize;
   GNAThub.Project.Initialize;
   GNAThub.Configuration.Initialize;

   Log.Debug ("Loading project: " & GNAThub.Configuration.Project);
   GNAThub.Project.Load (GNAThub.Configuration.Project);

   Log.Debug ("Creating execution environment...");
   Create_Project_Directory_Env;

   if Interpreter_Mode then
      Log.Debug ("Connecting to existing database...");
      GNAThub.Database.Initialize
        (Create_From_Dir
           (GNAThub.Project.Object_Dir, Database_File.Full_Name),
        Remove_Previous_Database => False);

      Log.Debug ("Executing user script: " & Script);
      Execute_User_Script;

   else
      Log.Debug ("Creating local application database...");
      Create_Or_Override_Database;

      Log.Debug ("Executing Plugin Runner");
      Execute_Plugin_Runner;
   end if;

   Log.Debug ("Execution completed");
   Finalize_Application;

   return Ada.Command_Line.Success;

exception
   when Silent_Error =>
      Finalize_Application;

      return Ada.Command_Line.Failure;

   when E : Command_Line_Error =>
      Log.Fatal (Exception_Message (E));
      GNAT.Command_Line.Try_Help;
      Finalize_Application;

      return Ada.Command_Line.Failure;

   when E : Error =>
      Log.Fatal (Exception_Message (E));
      Finalize_Application;

      return Ada.Command_Line.Failure;

   when E : Fatal_Error =>
      Log.Fatal (Exception_Information (E));
      Finalize_Application;

      return Ada.Command_Line.Failure;

end GNAThub.Main;
