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

with Ada.Command_Line;                    use Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers;                      use Ada.Containers;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

with GNAT.Command_Line;                   use GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with GNAT.Strings;

with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

with GNAThub.Project;
with GNAThub.Version;
with GNAThub.Python;
with GNAThub.Constants; use GNAThub.Constants;

with GPR2;
with GPR2.Path_Name;
with GPR2.Project;

package body GNAThub.Configuration is
   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);
   Tool_Args_Section : constant String := "--targs:";

   Config : GNAT.Command_Line.Command_Line_Configuration;

   Project_Arg          : aliased GNAT.Strings.String_Access;
   Script_Arg           : aliased GNAT.Strings.String_Access;
   Target_Arg           : aliased GNAT.Strings.String_Access;
   Subdirs_Arg          : aliased GNAT.Strings.String_Access;
   Sonar_Work_Dir_Arg   : aliased GNAT.Strings.String_Access;
   Runtime_Arg          : aliased GNAT.Strings.String_Access;
   Jobs_Arg             : aliased Integer;
   Dry_Run_Arg          : aliased Boolean;
   Version_Arg          : aliased Boolean;
   Quiet_Arg            : aliased Boolean;
   Verbose_Arg          : aliased Boolean;
   Incremental_Arg      : aliased Boolean;
   Hide_Exempted_Arg    : aliased Boolean;
   Runners_Only_Arg     : aliased Boolean;
   Reporters_Only_Arg   : aliased Boolean;
   Display_Progress_Arg : aliased Boolean;

   --  Switch -U switch implementation
   U_Process_All_Arg    : aliased Boolean;

   --  Keeping this for later -U main_file switchimplementation
   --  U_Main_Arg           : aliased GNAT.Strings.String_Access;

   All_Plugins : Unbounded_String := Null_Unbounded_String;
   --  Store all plugins provided with --plugins

   package Tool_Arg_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Tool_Arg_Vectors.Vector,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Tool_Arg_Vectors."=");
   use Tool_Arg_Maps;

   Tool_Arg_Map : Tool_Arg_Maps.Map;
   --  Store all tools extra command line switches provided with -targs:

   procedure Parse_Command_Line (Parser : Opt_Parser);
   --  Parse the command line and handle -X switches

   procedure Evaluate_Command_Line;
   --  Invoke Parse_Command_Line and ensure consistency

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      EOL    : constant String :=
        "" & Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
      Spaces : constant String (1 .. 19) := (others => ' ');
   begin
      --  Declare the switches

      Define_Switch
        (Config       => Config,
         Switch       => "-X:",
         Help         => "Specify an external reference in the project");

      Define_Switch
        (Config      => Config,
         Output      => Project_Arg'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Run on the given project (mandatory)");

      Define_Switch
        (Config      => Config,
         Output      => U_Process_All_Arg'Access,
         Long_Switch => "-U",
         Help        => "Process all sources in all projects,"
         & " except for projects that" & EOL & Spaces
         & "have the ""Externally_Built"" attribute set to ""true""");

      --  Keeping this for later implemntation of -U main switch
      --        Define_Switch
      --          (Config      => Config,
      --           Output      => U_Main_Arg'Access,
      --           Switch      => "-U:",
      --           Help        =>
      --             "Process the closure of units rooted at the unit"
      --             & " contained in ARG");

      Define_Switch
        (Config      => Config,
         Long_Switch => "--plugins=",
         Help        => "Comma separated list of plugins to execute");

      Define_Switch
        (Config      => Config,
         Output      => Display_Progress_Arg'Access,
         Long_Switch => "-d",
         Help        => "Whether to display a progress bar for IDEs");

      Define_Switch
        (Config      => Config,
         Output      => Script_Arg'Access,
         Long_Switch => "--exec=",
         Help        => "Python script to execute (implies --incremental)");

      Define_Switch
        (Config      => Config,
         Output      => Target_Arg'Access,
         Long_Switch => "--target=",
         Help        => "Specify a target for cross platforms");

      Define_Switch
        (Config      => Config,
         Output      => Subdirs_Arg'Access,
         Long_Switch => "--subdirs=",
         Help        =>
           "Specify the location of the database from the object directory");

      Define_Switch
        (Config      => Config,
         Output      => Sonar_Work_Dir_Arg'Access,
         Long_Switch => "--sonar-work-dir=",
         Help        =>
           "Specify the directory where the data needed by the " &
           "sonnar-scanner will be collected");

      Define_Switch
        (Config      => Config,
         Output      => Runtime_Arg'Access,
         Long_Switch => "--RTS=",
         Help        => "Specify a runtime for the language Ada");

      Define_Switch
        (Config      => Config,
         Output      => Jobs_Arg'Access,
         Switch      => "-j:",
         Long_Switch => "--jobs=",
         Help        => "Number of jobs to run in parallel",
         Default     => 1);

      Define_Switch
        (Config      => Config,
         Output      => Verbose_Arg'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Toggle verbose mode on");

      Define_Switch
        (Config      => Config,
         Output      => Quiet_Arg'Access,
         Switch      => "-q",
         Long_Switch => "--quiet",
         Help        => "Toggle quiet mode on");

      Define_Switch
        (Config      => Config,
         Output      => Dry_Run_Arg'Access,
         Switch      => "-n",
         Long_Switch => "--dry-run",
         Help        => "Show plugins without executing them");

      Define_Switch
        (Config      => Config,
         Output      => Runners_Only_Arg'Access,
         Long_Switch => "--runners-only",
         Help        => "Execute only plugins implementing GNAThub.Runner");

      Define_Switch
        (Config      => Config,
         Output      => Reporters_Only_Arg'Access,
         Long_Switch => "--reporters-only",
         Help        => "Execute only plugins implementing GNAThub.Reporter");

      Define_Switch
        (Config      => Config,
         Output      => Version_Arg'Access,
         Switch      => "-V",
         Long_Switch => "--version",
         Help        => "Print the version and exit");

      Define_Switch
        (Config      => Config,
         Output      => Incremental_Arg'Access,
         Switch      => "-i",
         Long_Switch => "--incremental",
         Help        => "Do not remove database if exists");

      Define_Switch
        (Config      => Config,
         Output      => Hide_Exempted_Arg'Access,
         Long_Switch => "--gnatcheck-hide-exempted",
         Help        => "Hide GNATcheck exempted violations");
      --  Usage

      Set_Usage
        (Config => Config,
         Usage  => "[-vq] -P PROJECT [--plugins PLUGINS | --exec SCRIPT]" &
                   ASCII.LF & ASCII.HT & "       " &
                   "[--targs:<tool-name> SWITCHES [--]]",
         Help   => "GNAThub, driver & formatter for GNAT tool suite.");

      --  Parse the command line

      Evaluate_Command_Line;
   end Initialize;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Parser : Opt_Parser) is

      procedure Local_Parse_Command_Line (Switch, Param, Section : String);
      --  Allow to manage every occurrence of --plugins switch

      procedure Handle_Command_Line_Scenario_Variables (Index, Inc : Natural);
      --  Allow to manage every occurrence of -X switch for scenario variables

      ------------------------------
      -- Local_Parse_Command_Line --
      ------------------------------

      procedure Local_Parse_Command_Line (Switch, Param, Section : String)
      is
         pragma Unreferenced (Section);
      begin

         if Switch = "--plugins" then
            if All_Plugins = Null_Unbounded_String then
               All_Plugins := To_Unbounded_String (Param);
            else
               Append (All_Plugins, "," & Param);
            end if;
         end if;
      end Local_Parse_Command_Line;

      --------------------------------------------
      -- Handle_Command_Line_Scenario_Variables --
      --------------------------------------------

      procedure Handle_Command_Line_Scenario_Variables (Index, Inc : Natural)
      is
         Param : constant String  := Argument (Index);
         Pos   : constant Natural := Ada.Strings.Fixed.Index (Param, "=");
      begin
         if Pos /= 0 then
            declare
               Key   : constant String := Param (Param'First + Inc .. Pos - 1);
               Value : constant String := Param (Pos + 1 .. Param'Last);
            begin
               GNAThub.Project.Update_Env (Key, Value);
            end;
         else
            Warn ("Unexpected argument for -X:");
            Warn ("Expected ""key=value"", got " & Param);
         end if;
      end Handle_Command_Line_Scenario_Variables;

      Count          : constant Natural := Argument_Count;
      Index          : Natural;
   begin
      Getopt (Config, Local_Parse_Command_Line'Unrestricted_Access, Parser);

      Index := 1;
      if Count > 0 then
         while Index <= Count loop
            declare
               Str : constant String := Argument (Index);
            begin

               --  Parse -P argument and set as project file path
               if Str'Length >= 2 and then Str (1 .. 2) = "-P" then
                  if Str'Length = 2 then
                     Index := Index + 1;

                     if Index <= Count and then Argument (Index) /= "" then
                        Project_Arg := new String'(Argument (Index));
                     end if;
                  else
                     Project_Arg := new String'(Str (3 .. Str'Last));
                  end if;
               end if;

               --  Parse -X arguments (multiple occurrences should be handled)
               if Str'Length >= 2 and then Str (1 .. 2) = "-X" then
                  if Str'Length = 2 then
                     Index := Index + 1;

                     if Index <= Count and then Argument (Index) /= "" then
                        Handle_Command_Line_Scenario_Variables (Index, 0);
                     end if;
                  else
                     Handle_Command_Line_Scenario_Variables (Index, 2);
                  end if;
               end if;

            end;
            Index := Index + 1;
         end loop;

      end if;

   end Parse_Command_Line;

   ---------------------------
   -- Evaluate_Command_Line --
   ---------------------------

   procedure Evaluate_Command_Line
   is
      use Ada.Text_IO;
      use GNAT.Strings; --  Bring the '=' operator in the scope

      GNAThub_Command_Line : Unbounded_String := Null_Unbounded_String;
      --  The reconstructed command line that will be fed to GNAThub's command
      --  line parser once tool arguments sections are extracted from the
      --  original command line.

      procedure Save_Tool_Argument (Tool_Name, Option : String);
      --  Store the tool argument for later use

      function Starts_With_Tool_Args_Section (Option : String) return Boolean;
      --  Return True if the given option starts with (or is equal to – the
      --  error processing is done in the main routine) the --targs: prefix,
      --  False otherwise.

      procedure Append_To_GNAThub_Command_Line (Option : String);
      --  Append the option to the reconstructed command line for later
      --  processing by GNAThub's parser.

      ------------------------
      -- Save_Tool_Argument --
      ------------------------

      procedure Save_Tool_Argument (Tool_Name, Option : String) is
         C : constant Tool_Arg_Maps.Cursor := Tool_Arg_Map.Find (Tool_Name);
         V : Tool_Arg_Vectors.Vector := (if Has_Element (C) then
               Element (C) else Tool_Arg_Vectors.Empty_Vector);
      begin
         V.Append (Option);

         if Has_Element (C) then
            Tool_Arg_Map.Replace (Tool_Name, V);
         else
            Tool_Arg_Map.Insert (Tool_Name, V);
         end if;
      end Save_Tool_Argument;

      -----------------------------------
      -- Starts_With_Tool_Args_Section --
      -----------------------------------

      function Starts_With_Tool_Args_Section (Option : String) return Boolean
      is
         Idx : constant Natural := Option'First + Tool_Args_Section'Length;
      begin
         return Option'Length >= Tool_Args_Section'Length
            and then Option (Option'First .. Idx - 1) = Tool_Args_Section;
      end Starts_With_Tool_Args_Section;

      ------------------------------------
      -- Append_To_GNAThub_Command_Line --
      ------------------------------------

      procedure Append_To_GNAThub_Command_Line (Option : String) is

         procedure Update_GNAThub_Command_Line (Opt : String);

         -----------------------------------
         --  Update_GNAThub_Command_Line  --
         -----------------------------------

         procedure Update_GNAThub_Command_Line (Opt : String) is
         begin
            if GNAThub_Command_Line = Null_Unbounded_String then
               GNAThub_Command_Line := To_Unbounded_String (Opt);
            else
               Append (GNAThub_Command_Line, " " & Opt);
            end if;
         end Update_GNAThub_Command_Line;

         Idx : constant Natural := Option'First;

      begin

         --  In order to avoid execution errors when the scenario variables
         --  contains the sequence of " - " or " -" adding '"' delimiters
         --  surrounding these variables into the reconstructed command line
         --  parsed by the GNAThub's command line parser.

         if Option'Length >= 2 and then Option (Idx .. Idx + 1) = "-X" then
            Update_GNAThub_Command_Line ('"' & Option & '"');
         else
            Update_GNAThub_Command_Line (Option);
         end if;
      end Append_To_GNAThub_Command_Line;

      --  Start of processing for Evaluate_Command_Line

      J : Positive := 1;

   begin
      --  Extract tool argument switches prior to parser the command line

      loop
         exit when J > Argument_Count;

         declare
            Arg  : constant String  := Argument (J);
            Idx  : constant Natural := Arg'First + Tool_Args_Section'Length;
            Tool : constant String  := Arg (Idx .. Arg'Last);
         begin
            if Starts_With_Tool_Args_Section (Arg) then
               --  Handle the case where --targs: is used without argument, eg.
               --
               --    $ gnathub --targs: […]

               if Tool = "" then
                  raise Command_Line_Error
                    with "missing argument for " & Tool_Args_Section;
               end if;

               J := J + 1;

               if J > Argument_Count then
                  --  Handle the case where --targs:<tool> is used without
                  --  parameter, eg.
                  --    $ gnathub […] --targs:codepeer

                  --  ignore when missing tool switches and warn users
                  Warn ("ignoring --targs:" & Tool
                        & " since tool switches are missing!");

               else
                  --  Loop over the following switches and save them until
                  --  either the end of the command line is reached, the
                  --  special sentinel "--" is found or another
                  --  --targs: parameter is next.

                  loop
                     Save_Tool_Argument (Tool, Argument (J));

                     --  Exit if the next switch starts with --targs:
                     exit when J < Argument_Count
                       and then Starts_With_Tool_Args_Section
                         (Argument (J + 1));

                     J := J + 1;

                     --  Exit at the end of the command line or if the next
                     --  switch is the special sentinel "--".
                     exit when J > Argument_Count or else Argument (J) = "--";
                  end loop;
               end if;
            else
               --  Defer the parsing of other switches to GNAThub's command
               --  line parser.

               Append_To_GNAThub_Command_Line (Argument (J));
            end if;
         end;

         J := J + 1;
      end loop;

      --  Parse the reconstructed command line now that all tool arguments have
      --  been extracted from the original command line.

      declare
         GNAThub_Parser : Opt_Parser;
         GNAThub_Argv   : constant GNAT.OS_Lib.Argument_List_Access :=
                            GNAT.OS_Lib.Argument_String_To_List
                              (To_String (GNAThub_Command_Line));
      begin
         --  Parse the remaining switches, ie. all switches that are not
         --  supposed to be passed to tools, ie. GNAThub switches.

         --  Initialize the command line engine with the command line subset

         Initialize_Option_Scan (GNAThub_Parser, GNAThub_Argv);

         --  Manage -X (scenario vars) switches and call getopt

         Parse_Command_Line (GNAThub_Parser);

         --  Free allocated resources required for the processing of the
         --  command line.

         Free (GNAThub_Parser);
      end;

      --  Sanity check the result of the parsing of the command line and apply
      --  modifiers ie. check for mutual exclusions, set output verbosity, …

      Assert (Me, Project_Arg /= null, "unexpected null project");
      Assert (Me, Script_Arg /= null, "unexpected null script argument");

      --  Print the version and exit if --version is supplied

      if Version_Arg then
         Put_Line ("GNAThub " & GNAThub.Version.Version & " " &
                   GNAThub.Version.Date & " for GNATdashboard Suite");
         Put_Line ("Copyright (C) 2013-" & GNAThub.Version.Year & ", AdaCore");

         raise GNAT.Command_Line.Exit_From_Command_Line;
      end if;

      --  Ensure consistency of use for --quiet and --verbose and set the
      --  logging level accordingly

      if Quiet_Arg and then Verbose_Arg then
         raise Command_Line_Error
           with "--verbose and --quiet are mutually exclusive.";
      end if;

      if Runners_Only_Arg and then Reporters_Only_Arg then
         raise Command_Line_Error
           with "--runners-only and --reporters-only are mutually exclusive.";
      end if;

      if Quiet_Arg then
         GNAThub.Set_Verbosity (Quiet);
      elsif Verbose_Arg then
         GNAThub.Set_Verbosity (Verbose);
      end if;

      GNAThub.Set_IDE_Progress_Bar (Display_Progress_Arg);

      --  Ensure consistency of use for --plugins and --exec

      if All_Plugins /= Null_Unbounded_String
         and then Script_Arg.all /= ""
      then
         raise Command_Line_Error
           with "--plugins and --exec are mutually exclusive.";
      end if;

      --  Check that job number is in the correct range
      if Jobs_Arg not in Natural'Range then
         raise Command_Line_Error
           with "invalid jobs value: " & Integer'Image (Jobs_Arg);
      end if;

      if Dry_Run_Arg and then Project_Arg.all = "" then
         --  When no project file is provided in gnathub command line with
         --  dry-run switch list all auto-discovered plugins
         declare
            Had_Errors : Boolean := False;
         begin
            Trace (Me, "Execute plugin-runner: --dry-run without project");
            GNAThub.Python.Execute_File
              (Plugin_Runner.Display_Full_Name, Had_Errors);
            raise GNAT.Command_Line.Exit_From_Command_Line;
         end;
      end if;

      --  Check that project file path has been specified on command line
      if Project_Arg.all = "" then
         raise Command_Line_Error with "no project file specified";
      end if;

      --  Check existence of the given paths on disk

      declare
         Project_File : constant GPR2.Path_Name.Object :=
                          GPR2.Path_Name.Create_File
                            (GPR2.Project.Ensure_Extension
                               (GPR2.Filename_Optional (Project_Arg.all)));
      begin
         if not Project_File.Exists then
            raise Fatal_Error
              with Project_Arg.all & ": no such file or directory";
         end if;
      end;

      Trace (Me, "Use project file " & Project_Arg.all);

      if Script_Arg.all /= "" and then Incremental_Arg then
         Warn ("--incremental implied by --exec");
      end if;

   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         --  used by --version switch
         raise Exit_Success;

      when GNAT.Command_Line.Invalid_Switch =>
         --  Error message is already displayed
         raise Silent_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         raise Command_Line_Error
           with "-" & GNAT.Command_Line.Full_Switch & ": invalid parameter";

   end Evaluate_Command_Line;

   -------------
   -- Project --
   -------------

   function Project return String is
   begin
      return Project_Arg.all;
   end Project;

   --  Keeping this for -U main switch implementation
   --     ------------
   --     -- U_Main --
   --     ------------
   --
   --     function U_Main return String is
   --     begin
   --        return U_Main_Arg.all;
   --     end U_Main;

   -------------------
   -- U_Process_All --
   -------------------

   function U_Process_All return Boolean is
   begin
      return U_Process_All_Arg;
   end U_Process_All;

   -------------
   -- Plugins --
   -------------

   function Plugins return String is
   begin
      return To_String (All_Plugins);
   end Plugins;

   ------------
   -- Script --
   ------------

   function Script return String is
   begin
      return Script_Arg.all;
   end Script;

   -------------
   -- Subdirs --
   -------------

   function Subdirs return String is
   begin
      return Subdirs_Arg.all;
   end Subdirs;

   --------------------
   -- Sonar_Work_Dir --
   --------------------

   function Sonar_Work_Dir return String is
   begin
      return Sonar_Work_Dir_Arg.all;
   end Sonar_Work_Dir;

   ------------
   -- Target --
   ------------

   function Target return String is
   begin
      return Target_Arg.all;
   end Target;

   -------------
   -- Runtime --
   -------------

   function Runtime return String is
   begin
      return Runtime_Arg.all;
   end Runtime;

   ----------
   -- Jobs --
   ----------

   function Jobs return Natural is
   begin
      return Jobs_Arg;
   end Jobs;

   -------------
   -- Verbose --
   -------------

   function Verbose return Boolean is
   begin
      return Verbose_Arg;
   end Verbose;

   -----------
   -- Quiet --
   -----------

   function Quiet return Boolean is
   begin
      return Quiet_Arg;
   end Quiet;

   ----------------------
   -- Interpreter_Mode --
   ----------------------

   function Interpreter_Mode return Boolean is
   begin
      return Script_Arg.all /= "";
   end Interpreter_Mode;

   -----------------
   -- Incremental --
   -----------------

   function Incremental return Boolean is
   begin
      return Interpreter_Mode or else Incremental_Arg;
   end Incremental;

   --------------------
   --  Hide_Exempted --
   --------------------

   function Hide_Exempted return Boolean is
   begin
      return Hide_Exempted_Arg;
   end Hide_Exempted;

   ----------------------
   -- Display_Progress --
   ----------------------

   function Display_Progress return Boolean is
   begin
      return Display_Progress_Arg;
   end Display_Progress;

   -------------
   -- Dry_Run --
   -------------

   function Dry_Run return Boolean is
   begin
      return Dry_Run_Arg;
   end Dry_Run;

   -----------------------------
   -- Dry_Run_Without_Project --
   -----------------------------

   function Dry_Run_Without_Project return Boolean is
   begin
      return Dry_Run and (Project_Arg.all = "");
   end Dry_Run_Without_Project;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      GNAT.Command_Line.Free (Config);
   end Finalize;

   ---------------
   -- Tool_Args --
   ---------------

   function Tool_Args (Tool_Name : String) return Tool_Arg_Vectors.Vector
   is
      Found : constant Tool_Arg_Maps.Cursor := Tool_Arg_Map.Find (Tool_Name);
   begin
      if not Has_Element (Found) then
         return Tool_Arg_Vectors.Empty_Vector;
      end if;

      return Element (Found);
   end Tool_Args;

   ------------------
   -- Runners_Only --
   ------------------

   function Runners_Only return Boolean is
   begin
      return Runners_Only_Arg;
   end Runners_Only;

   --------------------
   -- Reporters_Only --
   --------------------

   function Reporters_Only return Boolean is
   begin
      return Reporters_Only_Arg;
   end Reporters_Only;

end GNAThub.Configuration;
