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
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Arg_Lists;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

procedure Qualimetrics is

   Main_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.MAIN", Off);

   Error_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.Errors", On);

   procedure Main;
   --  Program entry point

   procedure Load_Main_Plugin;
   --  Load the main plugin

   function Create_Project_Directory_Env return Boolean;
   --  Check the existance of Project object directory and create direcotries
   --  needed for qualimetrics driver, such as:
   --  <project_root_dir>
   --             /object_dir/
   --                  qualimetrics/    --  qualimetrics deposit dir
   --                       logs/       --  directory for plugin log

   Kernel     : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;
   Prefix_Dir : constant Virtual_File := Create (+Executable_Location);
   Core_Dir   : constant Virtual_File := Create_From_Dir
     (Prefix_Dir, "share/qualimetrics/core");
   Core_Plugin_Dir : constant Virtual_File := Create_From_Dir
   (Core_Dir, "plug-ins");
   User_Plugin_Dir : constant Virtual_File := Create_From_Dir
     (Prefix_Dir, "share/qualimetrics/plug-ins");
   Python_Path : constant String := "sys.path=[r'" &
           (+Core_Dir.Full_Name.all) & "', '" &
           (+Core_Plugin_Dir.Full_Name.all) & "', '" &
           (+User_Plugin_Dir.Full_Name.all)
            & "']+sys.path";

   ----------------------------------
   -- Create_Project_Directory_Env --
   ----------------------------------

   function Create_Project_Directory_Env return Boolean
   is
      Object_Directory : constant Virtual_File
        := Kernel.Registry.Tree.Root_Project.Object_Dir;
      Logs_Directory : Virtual_File;
      Root_Directory : Virtual_File;

   begin
      --  Check exitance of project object directory
      if Object_Directory = No_File then
         return False;
      end if;

      Root_Directory := Create_From_Dir (Object_Directory, "qualimetrics");
      Logs_Directory := Create_From_Dir (Root_Directory, "logs");

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

   ----------------------
   -- Load_Main_Plugin --
   ----------------------

   procedure Load_Main_Plugin is
      Python : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("python");

      Errors : Boolean;
   begin
      --  /!\  Plugin management  /!\

      --  Add the default search path for qualimetrics plugins
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
           (Core_Dir,
            +"plugins_executor.py").Full_Name,
         Hide_Output => False,
         Show_Command => False,
         Errors   => Errors);

      if Errors then
         Trace (Main_Trace, "Errors during python script execution");
      end if;
   end Load_Main_Plugin;

   ----------
   -- Main --
   ----------

   procedure Main is
      Command_Line : Qualimetrics_Command_Line;
   begin
      GNATCOLL.Traces.Parse_Config_File;

      GPS.CLI_Utils.Create_Kernel_Context (Kernel);
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
      if not Initialize_DB
        (Kernel.Registry.Tree.Root_Project.Object_Dir,
         Create_From_Dir (Core_Dir, "dbschema.txt"))
      then
         Trace (Error_Trace, "Could not initialize the database", Red_Fg);
         return;
      end if;

      Trace (Main_Trace, "DB Created");

      --  Save project tree to DB
      Save_Project_Tree (Kernel.Registry.Tree.Root_Project);

      Trace (Main_Trace, "Project information entered in the database");

      Load_Main_Plugin;

      --  Save project tree to DB
      Save_Project_Tree (Kernel.Registry.Tree.Root_Project);

      GPS.CLI_Utils.Destroy_Kernel_Context (Kernel);
      Command_Line.Destroy;

      GNATCOLL.Traces.Finalize;

   end Main;

begin

   Main;

end Qualimetrics;
