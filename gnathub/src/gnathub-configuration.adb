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

with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line;       use GNAT.Command_Line;

with GNATCOLL.VFS;            use GNATCOLL.VFS;

with GPS.CLI_Utils;

with GNAThub.Version;

package body GNAThub.Configuration is

   Config      : GNAT.Command_Line.Command_Line_Configuration;

   Project_Arg : aliased GNAT.Strings.String_Access;
   Script_Arg  : aliased GNAT.Strings.String_Access;
   Plugins_Arg : aliased GNAT.Strings.String_Access;
   Version     : aliased Boolean;
   Quiet       : aliased Boolean;
   Verbose     : aliased Boolean;

   procedure Parse (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Parses the command line.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) is
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
         Output      => Plugins_Arg'Access,
         Long_Switch => "--plugins=",
         Help        => "Comma separated list of plugins to execute");

      Define_Switch
        (Config      => Config,
         Output      => Script_Arg'Access,
         Switch      => "-l:",
         Long_Switch => "--load=",
         Help        => "Execute external script written (lang:path)");

      Define_Switch
        (Config      => Config,
         Output      => Verbose'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Toggle verbose mode on",
         Value       => True);

      Define_Switch
        (Config      => Config,
         Output      => Quiet'Access,
         Switch      => "-q",
         Long_Switch => "--quiet",
         Help        => "Toggle quiet mode on",
         Value       => True);

      Define_Switch
        (Config      => Config,
         Output      => Version'Access,
         Switch      => "-V",
         Long_Switch => "--version",
         Help        => "Print the version and exit",
         Value       => True);

      --  Usage

      Set_Usage
        (Config => Config,
         Usage  => "[-vq] -P PROJECT [-plugins PLUGINS] [-X ARG [-X ARG]]",
         Help   => "GNAThub, driver & formatter for GNAT tool suite.");

      --  Parse the command line
      Parse (Kernel);
   end Initialize;

   -----------
   -- Parse --
   -----------

   procedure Parse (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record)
   is
      use GNAT.Strings;
      --  Bring the '=' operator in the scope

   begin
      --  Manage -X (scenario vars) switches and call getopt

      GPS.CLI_Utils.Parse_Command_Line (Config, Kernel);

      --  Print the version and exit if --version is supplied

      if Version then
         Put_Line ("GNAThub driver " & GNAThub.Version.Version);
         raise GNAT.Command_Line.Exit_From_Command_Line;
      end if;

      --  Ensure consistency of use for --quiet and --verbose and set the
      --  logging level accordingly

      if Quiet and then Verbose then
         raise Error with "--verbose and --quiet are mutually exclusive.";
      end if;

      if Quiet then
         Log.Set_Verbosity (Log.Quiet);
      end if;

      if Verbose then
         Log.Set_Verbosity (Log.Verbose);
      end if;

      --  Check that project file path has been specified on command line

      if not GPS.CLI_Utils.Is_Project_Path_Specified (Project_Arg) then
         raise Error with "No project file specified, see --help for usage.";
      end if;

      --  Check existance of the given path on disk

      if not GPS.CLI_Utils.Project_File_Path_Exists (Project_Arg) then
         raise Error with "No such file: " & Project;
      end if;

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

   -------------
   -- Project --
   -------------

   function Project return String is
   begin
      return Project_Arg.all;
   end Project;

   ------------
   -- Script --
   ------------

   function Script return String is
   begin
      return Script_Arg.all;
   end Script;

   -------------
   -- Project --
   -------------

   function Project return GNAT.Strings.String_Access is
   begin
      return Project_Arg;
   end Project;

   ------------
   -- Script --
   ------------

   function Script return GNAT.Strings.String_Access is
   begin
      return Script_Arg;
   end Script;

   -------------
   -- Plugins --
   -------------

   function Plugins return String is
   begin
      return Plugins_Arg.all;
   end Plugins;

   -------------
   -- Plugins --
   -------------

   function Plugins return GNAT.Strings.String_Access is
   begin
      return Plugins_Arg;
   end Plugins;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      GNAT.Command_Line.Free (Config);
   end Finalize;

end GNAThub.Configuration;
