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
--           qualimetrics_api.py  --  this is the main API entry point
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

procedure Qualimetrics is

   Main_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.MAIN", Off);

   Error_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.Errors", On);

   procedure Main;
   --  Program entry point

   procedure Load_Plugin_Runner;
   --  Load the plugin runner that executes every qualimetrics plugin:
   --  both core and user plugins.

   function Create_Project_Directory_Env return Boolean;
   --  Check the existance of Project object directory and create direcotries
   --  needed for qualimetrics driver, such as:
   --  <project_root_dir>
   --             /object_dir/
   --                  qualimetrics/    --  qualimetrics deposit dir
   --                       logs/       --  directory for plugin log

   Kernel : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;

   ----------------------------------
   -- Create_Project_Directory_Env --
   ----------------------------------

   function Create_Project_Directory_Env return Boolean
   is
      Object_Directory : constant Virtual_File
        := Kernel.Registry.Tree.Root_Project.Object_Dir;
      Root_Directory : constant Virtual_File := Create_From_Dir
        (Object_Directory, Core_Properties.Project_Qmt_Dir_Name);
      Logs_Directory : constant Virtual_File := Create_From_Dir
        (Root_Directory, Core_Properties.Project_Log_Dir_Name);

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
         Trace (Error_Trace, Exception_Information (E), Red_Fg);
         return False;
   end Create_Project_Directory_Env;

   ------------------------
   -- Load_Plugin_Runner --
   ------------------------

   procedure Load_Plugin_Runner is
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
      Trace (Main_Trace, Python_Path);
      Execute_Command
        (Python,
         GNATCOLL.Arg_Lists.Create (Python_Path),
         Show_Command => False,
         Hide_Output  => True,
         Errors       => Errors);

      if Errors then
         Trace (Main_Trace, "Could not set the search path for python");
      end if;

      --  Execute the main plugin
      Execute_File
        (Python,
         Filename =>
         +Create_From_Dir
           (Core_Properties.Qmt_Core_Dir,
            +"plugins_runner.py").Full_Name,
         Hide_Output => False,
         Show_Command => False,
         Errors   => Errors);

      if Errors then
         Trace (Main_Trace, "Errors during python script execution");
      end if;
   end Load_Plugin_Runner;

   ----------
   -- Main --
   ----------

   procedure Main is
      Command_Line : Qualimetrics_Command_Line;
   begin
      --  Initialisation
      GNATCOLL.Traces.Parse_Config_File;
      GPS.CLI_Utils.Create_Kernel_Context (Kernel);
      Qmt_Python_Api.Initialise (Kernel);
      Command_Line.Configure;

      --  Error output messages are handled by Parse function, according
      --  to the case of failure.
      if not Command_Line.Parse (Kernel) then
         return;
      end if;

      --  Load project
      if not Load_Project_Tree (Command_Line.Get_Project_Name,
                                Kernel)
      then
         Trace (Error_Trace, "Could not load the project", Red_Fg);
         return;
      end if;

      --  Create qualimetrics directories in project object directory
      if not Create_Project_Directory_Env then
         Trace (Main_Trace, "Unable to initialise project qualimetrics"
                & "object directory environment", Red_Fg);
      end if;

      --  Create Database
      if not Initialise_DB
        (Create_From_Dir (Kernel.Registry.Tree.Root_Project.Object_Dir,
                          Core_Properties.DB_File_Name),
         Create_From_Dir (Core_Properties.Qmt_Core_Dir, "dbschema.txt"))
      then
         Trace (Error_Trace, "Could not initialize the database", Red_Fg);
         return;
      end if;

      Trace (Main_Trace, "DB Created");

      --  Save project tree to DB
      Save_Project_Tree (Kernel.Registry.Tree.Root_Project);

      Trace (Main_Trace, "Project information entered in the database");

      --  Run qualimetrics plugins
      Load_Plugin_Runner;

      --  Run script if present on command line
      if Command_Line.Get_Script_Arg.all  /= "" then
         if Utils.Execute_Script (Kernel, Command_Line.Get_Script_Arg) then
            Trace (Main_Trace, "[Debug] - script load successfully");
         end if;
      end if;

      --  Finilise
      GPS.CLI_Utils.Destroy_Kernel_Context (Kernel);
      Command_Line.Destroy;
      GNATCOLL.Traces.Finalize;

   end Main;

begin

   Main;

end Qualimetrics;
