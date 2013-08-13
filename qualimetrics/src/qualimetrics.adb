------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

--  Organization of the qualimetrics install directory:
--
--   <install_dir>
--     /bin/
--       qualimetrics             --  the main executable
--     /share/
--       qualimetrics/
--         plug-ins/              --  user-defined plugins go here
--         core/
--           plugins_runner.py    --  this is the main python entry point
--                                --  that users can use
--           plug-ins/            --  AdaCore-defined plugins go here

with GPS.CLI_Utils;
with GPS.CLI_Kernels;       use GPS.CLI_Kernels;

with Database_Interface;    use Database_Interface;
with Qmt_Command_Line;      use Qmt_Command_Line;
with Project_Parser;        use Project_Parser;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Arg_Lists;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Core_Properties;
with Qmt_Python_Api;
with Utils;
with Logger;
with Ada.Calendar;          use Ada.Calendar;
with GNAT.OS_Lib;
with GNAT.IO; use GNAT.IO;

procedure Qualimetrics is

   procedure Main;
   --  Program entry point

   function Load_Plugin_Runner return Boolean;
   --  Load the plugin runner that executes every qualimetrics plugin:
   --  both core and user plugins.

   function Create_Project_Directory_Env return Boolean;
   --  Check the existance of Project object directory and create direcotries
   --  needed for qualimetrics driver, such as:
   --  <project_root_dir>
   --             /object_dir/
   --                  qualimetrics/    --  qualimetrics deposit dir
   --                       logs/       --  directory for plugin log

   function Create_Specific_Project_Attributes return Boolean;

   Kernel : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;

   function Create_Specific_Project_Attributes return Boolean
   is
      Errors : Boolean;
   begin
      Execute_File
        (Script   => Kernel.Scripts.Lookup_Scripting_Language ("python"),
         Filename => GNAT.OS_Lib.Normalize_Pathname
           ("project_attributes.py",
            Core_Properties.Qmt_Core_Dir.Display_Full_Name),
         Show_Command => False,
         Errors   => Errors);

      if Errors then
         return Utils.Return_On_Failure ("Error while creating Qualimetrics " &
                                    "specific project attributes.");
      end if;

      return True;
   end Create_Specific_Project_Attributes;

   ----------------------------------
   -- Create_Project_Directory_Env --
   ----------------------------------

   function Create_Project_Directory_Env return Boolean
   is
      Object_Directory : constant Virtual_File
        := Kernel.Registry.Tree.Root_Project.Object_Dir;
      Logs_Directory   : constant Virtual_File := Create_From_Dir
        (Object_Directory, Core_Properties.Project_Log_Dir_Name.Full_Name);

   begin
      --  Check exitance of project object directory
      if Object_Directory = No_File then
         return False;
      end if;

      --  Create both root and log directories
      if not Is_Regular_File (Logs_Directory) then
         Make_Dir (Dir       => Logs_Directory,
                   Recursive => True);
      end if;

      return True;

   exception
      when E : Directory_Error =>
         return Utils.Return_On_Failure
           (Exception_Name (E) & ": Unable to initialise qualimetrics" &
              " object directory environment in: " &
              Object_Directory.Display_Full_Name);
   end Create_Project_Directory_Env;

   ------------------------
   -- Load_Plugin_Runner --
   ------------------------

   function Load_Plugin_Runner return Boolean is
      Plugin_Runner : constant Virtual_File := Create_From_Dir
           (Core_Properties.Qmt_Core_Dir, +"plugins_runner.py");
      Errors : Boolean;
      Python : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("python");
      Python_Path : constant String :=
        "sys.path=[r'" &
        Core_Properties.Qmt_Core_Dir.Display_Full_Name & "', '" &
        Core_Properties.Qmt_Core_Plugin_Dir.Display_Full_Name & "', '" &
        Core_Properties.Qmt_User_Plugin_Dir.Display_Full_Name & "', '" &
        Core_Properties.Qmt_Core_Lib_Dir.Display_Full_Name
        & "']+sys.path";

   begin
      --  Add the default search path for qualimetrics plugins
      --  and qualimetrics python API
      Trace (Logger.Debug_Trace, "  Loading plugins with python path: " &
                                Python_Path);
      Execute_Command
        (Python,
         GNATCOLL.Arg_Lists.Create (Python_Path),
         Show_Command => False,
         Hide_Output  => True,
         Errors       => Errors);

      if Errors then
         Trace (Logger.Warn_Trace, "Could not set the search path for python");
      end if;

      --  Execute the pythn script that runs all the plugins
      Logger.Trace_Title ("Running tools analysis");
      Put_Line (" ");
      Trace (Logger.Debug_Trace, "Executing plugin runner script: "
             & Plugin_Runner.Display_Full_Name);
      Execute_File
        (Python,
         Filename => Plugin_Runner.Display_Full_Name,
         Hide_Output => False,
         Show_Command => False,
         Errors   => Errors);

      if Errors then
         return Utils.Return_On_Failure
           ("/!\ Error during the execution of the tool runner /!\");
      end if;

      return True;

   end Load_Plugin_Runner;

   ----------
   -- Main --
   ----------

   procedure Main is
      Command_Line : Qualimetrics_Command_Line;
   begin
      Logger.Program_Starting_Time := Ada.Calendar.Clock;

      --  Initialisation
      GNATCOLL.Traces.Parse_Config_File;
      Trace (Logger.Debug_Trace, "Initializing Qualimetrics kernel context");
      GPS.CLI_Utils.Create_Kernel_Context
        (Kernel,
         Install_Semantic_Parser => False);
      Qmt_Python_Api.Initialise (Kernel);
      Command_Line.Configure;
      if not Create_Specific_Project_Attributes then
         return;
      end if;

      --  Error output messages are handled by Parse function, according
      --  to the case of failure.
      if not Command_Line.Parse (Kernel) then
         return;
      end if;

      --  Load project
      Trace (Logger.Info_Trace, "Loading project tree from: " &
                               Command_Line.Get_Project_Name.all);
      if not Load_Project_Tree (Command_Line.Get_Project_Name, Kernel) then
         return;
      end if;

      --  Create qualimetrics directories in project object directory
      Trace (Logger.Info_Trace, "Initializing environement...");
      if not Create_Project_Directory_Env then
         return;
      end if;

      --  Create Database
      Trace (Logger.Info_Trace, "Initializing DB...");
      if not Initialise_DB
        (Create_From_Dir (Kernel.Registry.Tree.Root_Project.Object_Dir,
                          Core_Properties.DB_File_Relative_Path.Full_Name),
         Create_From_Dir (Core_Properties.Qmt_Core_Dir, "dbschema.txt"))
      then
         return;
      end if;
      Trace (Logger.Debug_Trace, "DB Created");

      --  Save project tree to DB
      Trace (Logger.Debug_Trace, "Saving project tree in DB...");
      Save_Project_Tree (Kernel.Registry.Tree.Root_Project);

      --  Run qualimetrics plugins
      if not Load_Plugin_Runner then
         return;
      end if;

      --  Run script if present on command line
      if Command_Line.Get_Script_Arg.all  /= "" then
         Trace (Logger.Info_Trace, "=====  Running script from command line");
         if not Utils.Execute_Script (Kernel, Command_Line.Get_Script_Arg) then
            return;
         end if;
         Trace (Logger.Debug_Trace, "Script successfully loaded");
      end if;

      --  Log end statistics
      Logger.Trace_End_Success;

      --  Finilise
      GPS.CLI_Utils.Destroy_Kernel_Context (Kernel);
      Command_Line.Destroy;
      GNATCOLL.Traces.Finalize;

   end Main;

begin

   Main;

end Qualimetrics;
