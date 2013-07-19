------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GPS.CLI_Utils;

with Utils;
with Logger;
with Version;

package body Qmt_Command_Line is

   ----------------------------
   -- Configure_Command_Line --
   ----------------------------

   procedure Configure (Self : in out Qualimetrics_Command_Line) is
   begin
      --  Usage
      Set_Usage
        (Config => Self.Command_Line,
         Help   => "Qualimetrics, driver & formatter for GNAT tool suite");

      --  Switches
      Define_Switch
        (Self.Command_Line,
         Output      => Self.Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Load the given project (mandatory)");
      Define_Switch
        (Self.Command_Line,
         Switch       => "-X:",
         Help         => "Specify an external reference in the project");
      Define_Switch
        (Self.Command_Line,
         Output      => Self.Script_Arg'Access,
         Switch      => "-l:",
         Long_Switch => "--load=",
         Help        => "Execute an external file written in the" &
                         " language lang");
      Define_Switch
        (Self.Command_Line,
         Output      => Self.Version'Access,
         Long_Switch => "--version",
         Help        => "Version of Qualimetrics driver",
         Value       => True);
      Define_Switch
        (Self.Command_Line,
         Output      => Self.Quiet'Access,
         Switch      => "-q",
         Long_Switch => "--quiet",
         Help        => "Quiet mode, no log displayed on the" &
                        " standard outpout",
         Value       => True);
      Define_Switch
        (Self.Command_Line,
         Output      => Self.Verbose'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Verbose mode, display additional information on" &
                        " the standard outpout",
         Value       => True);
   end Configure;

   ----------------------
   -- Get_Command_Line --
   ----------------------

   function Parse
     (Self   : in out Qualimetrics_Command_Line;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) return Boolean is
   begin
      --  Manage -X (scenario vars) switch and call getopt
      GPS.CLI_Utils.Parse_Command_Line (Self.Command_Line, Kernel);

      --  Trace level
      if Self.Quiet then
         --  Set quiet mode if mentionned,
         --   priority is given to the quiet mode if verbose + quiet is
         --  mentionned on the command line
         Logger.Set_Quiet_Traces;
      elsif Self.Verbose then
         --  Set verbose mode if mentionned
         Logger.Set_Verbose_Traces;
      end if;

      --  Version option
      if Self.Version then
         Put_Line ("Qualimetrics driver " & Version.VERSION);
         return False;
      end if;

      --  Check that project file path has been specified on command line
      if not GPS.CLI_Utils.Is_Project_Path_Specified (Self.Project_Name) then
         return Utils.Return_On_Failure ("No project file specified");
      end if;

      --  Check existance of the given path on disk
      if not GPS.CLI_Utils.Project_File_Path_Exists (Self.Project_Name) then
         return Utils.Return_On_Failure ("No such file: " &
                                           Self.Project_Name.all);
      end if;

      return True;
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         --  Help is already displayed
         return False;
      when GNAT.Command_Line.Invalid_Switch =>
         --  Error message is already displayed
         return False;
      when GNAT.Command_Line.Invalid_Parameter =>
         return Utils.Return_On_Failure
           ("Invalid parameter for switch: " & GNAT.Command_Line.Full_Switch);
   end Parse;

   ----------------------
   -- Get_Project_Name --
   ----------------------

   function Get_Project_Name
     (Self : Qualimetrics_Command_Line) return GNAT.Strings.String_Access is
   begin
      return Self.Project_Name;
   end Get_Project_Name;

   --------------------
   -- Get_Script_Arg --
   --------------------

   function Get_Script_Arg
     (Self : Qualimetrics_Command_Line) return GNAT.Strings.String_Access is
   begin
      return Self.Script_Arg;
   end Get_Script_Arg;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Qualimetrics_Command_Line) is
   begin
      GNAT.Command_Line.Free (Self.Command_Line);
   end Destroy;

end Qmt_Command_Line;
