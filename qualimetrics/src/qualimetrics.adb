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

with Utils;
with Database_Interface;    use Database_Interface;
with Qmt_Command_Line;      use Qmt_Command_Line;
with Project_Parser;        use Project_Parser;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Arg_Lists;

procedure Qualimetrics is

   Main_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.MAIN", Off);

   Error_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("QUALIMETRICS.Errors", On);

   procedure Main;
   --  Program entry point

   procedure Load_Main_Plugin;
   --  Load the main plugin

   Kernel     : constant GPS.CLI_Kernels.CLI_Kernel :=
     new GPS.CLI_Kernels.CLI_Kernel_Record;
   Prefix_Dir : constant Virtual_File := Create (+Executable_Location);
   Core_Dir   : constant Virtual_File := Create_From_Dir
     (Prefix_Dir, "share/qualimetrics/core");

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

      Execute_Command
        (Python,
         GNATCOLL.Arg_Lists.Create ("sys.path=[r'" &
           (+Core_Dir.Full_Name.all)
           & "']+sys.path"),
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
            +"plug-ins/gnatmetric.py").Full_Name,
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
      Deposit_Dir  : Virtual_File;
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

      --  Set Qualimetrics deposit directory
      Deposit_Dir := Utils.Get_Deposit_Directory
        (Kernel.Registry.Tree.Root_Project);

      --  Create Database
      if not Initialize_DB
        (Deposit_Dir,
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
