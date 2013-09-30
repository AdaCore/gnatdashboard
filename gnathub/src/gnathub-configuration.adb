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

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GPS.CLI_Utils;

with GNAThub.Scripts;
with GNAThub.Version;

package body GNAThub.Configuration is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Command_Line) is
   begin
      --  Declare the switches

      Define_Switch
        (Config       => Self.Command_Line,
         Switch       => "-X:",
         Help         => "Specify an external reference in the project");

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Run on the given project (mandatory)");

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Plugins'Access,
         Long_Switch => "--plugins=",
         Help        => "Comma separated list of plugins to execute");

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Script_Arg'Access,
         Switch      => "-l:",
         Long_Switch => "--load=",
         Help        => "Execute external script written (lang:path)");

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Verbose'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Toggle verbose mode on",
         Value       => True);

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Quiet'Access,
         Switch      => "-q",
         Long_Switch => "--quiet",
         Help        => "Toggle quiet mode on",
         Value       => True);

      Define_Switch
        (Config      => Self.Command_Line,
         Output      => Self.Version'Access,
         Switch      => "-V",
         Long_Switch => "--version",
         Help        => "Print the version and exit",
         Value       => True);

      --  Usage

      Set_Usage
        (Config => Self.Command_Line,
         Usage  => "[-vq] -P PROJECT [-plugins PLUGINS] [-X ARG [-X ARG]]",
         Help   => "GNAThub, driver & formatter for GNAT tool suite");

   end Initialize;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self   : in out Command_Line;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) is
   begin
      --  Manage -X (scenario vars) switch and call getopt
      GPS.CLI_Utils.Parse_Command_Line (Self.Command_Line, Kernel);

      --  Trace level
      if Self.Quiet and then Self.Verbose then
         raise Error
           with "options --verbose and --quiet are mutually exclusive";
      end if;

      if Self.Quiet then
         Log.Set_Verbosity (Log.Quiet);
      end if;

      if Self.Verbose then
         Log.Set_Verbosity (Log.Verbose);
      end if;

      --  Version option
      if Self.Version then
         Put_Line ("GNAThub driver " & GNAThub.Version.Version);
         raise GNAT.Command_Line.Exit_From_Command_Line;
      end if;

      --  Check that project file path has been specified on command line
      if not GPS.CLI_Utils.Is_Project_Path_Specified (Self.Project_Name) then
         raise Error with "No project file specified, see --help for usage.";
      end if;

      --  Check existance of the given path on disk
      if not GPS.CLI_Utils.Project_File_Path_Exists (Self.Project_Name) then
         raise Error with "No such file: " & Self.Project_Name.all;
      end if;

      GNAThub.Scripts.Plugins_To_Execute := Self.Plugins;

   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         --  Help is already displayed
         raise Silent_Error;

      when GNAT.Command_Line.Invalid_Switch =>
         --  Error message is already displayed
         raise Silent_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         raise Error with "Invalid parameter for switch: " &
                          GNAT.Command_Line.Full_Switch;
   end Parse;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Self : Command_Line) return String is
   begin
      return Self.Project_Name.all;
   end Project_Name;

   ----------------
   -- Script_Arg --
   ----------------

   function Script_Arg (Self : Command_Line) return String is
   begin
      return Self.Script_Arg.all;
   end Script_Arg;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name
     (Self : Command_Line) return GNAT.Strings.String_Access is
   begin
      return Self.Project_Name;
   end Project_Name;

   ----------------
   -- Script_Arg --
   ----------------

   function Script_Arg
     (Self : Command_Line) return GNAT.Strings.String_Access is
   begin
      return Self.Script_Arg;
   end Script_Arg;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Command_Line) is
   begin
      GNAT.Command_Line.Free (Self.Command_Line);
   end Finalize;

end GNAThub.Configuration;
