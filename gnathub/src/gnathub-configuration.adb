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

with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT.Command_Line;       use GNAT.Command_Line;

with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;      use GNATCOLL.VFS_Utils;

with GNAThub.Project;
with GNAThub.Version;

package body GNAThub.Configuration is

   Config      : GNAT.Command_Line.Command_Line_Configuration;

   Project_Arg : aliased GNAT.Strings.String_Access;
   Plugins_Arg : aliased GNAT.Strings.String_Access;
   Script_Arg  : aliased GNAT.Strings.String_Access;
   Version     : aliased Boolean;
   Quiet       : aliased Boolean;
   Verbose     : aliased Boolean;

   procedure Parse_Command_Line;
   --  Parse the command line and handle -X switches

   procedure Evaluate_Command_Line;
   --  Invoke Parse_Command_Line and ensure consistency

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
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
         Long_Switch => "--load=",
         Help        => "Execute an external Python script before exiting");

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

      Evaluate_Command_Line;
   end Initialize;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      procedure Local_Parse_Command_Line (Switch, Parameter, Section : String);
      --  Allow to manage every occurance of -X switch for scenario variable

      ------------------------------
      -- Local_Parse_Command_Line --
      ------------------------------

      procedure Local_Parse_Command_Line (Switch, Parameter, Section : String)
      is
         pragma Unreferenced (Section);
         Equal : Natural;
      begin
         if Switch = "-X" then
            Equal := Ada.Strings.Fixed.Index (Parameter, "=");

            if Equal /= 0 then
               GNAThub.Project.Update_Env
                 (Key   => Parameter (Parameter'First .. Equal - 1),
                  Value => Parameter (Equal + 1 .. Parameter'Last));

            else
               Log.Warn
                 ("Ignoring switch -X, missing name or/and value for: " &
                  Switch & Parameter);
            end if;
         end if;

      end Local_Parse_Command_Line;

   begin
      Getopt (Config, Local_Parse_Command_Line'Unrestricted_Access);
   end Parse_Command_Line;

   ---------------------------
   -- Evaluate_Command_Line --
   ---------------------------

   procedure Evaluate_Command_Line
   is
      use GNAT.Strings;
      --  Bring the '=' operator in the scope

   begin
      --  Manage -X (scenario vars) switches and call getopt

      Parse_Command_Line;

      --  Print the version and exit if --version is supplied

      if Version then
         Log.Info
           ("GNAThub v" & GNAThub.Version.Version & " (" &
            GNAThub.Version.Date & ")");

         Log.Info ("Copyright (C) " & GNAThub.Version.Year & ", AdaCore");

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

      if Project_Arg = null or else Project_Arg.all = "" then
         raise Error with "No project file specified, see --help for usage.";
      end if;

      --  Check existance of the given path on disk

      declare
         Project : constant String := Project_Arg.all;
         Ext     : constant String := +Project_File_Extension;

      begin
         if not Ends_With (Project, Ext) then
            Free (Project_Arg);
            Project_Arg := new String'(Project & Ext);
         end if;

         if not Is_Regular_File (Filesystem_String (Project_Arg.all)) then
            raise Error with "No such file: " & Project;
         end if;
      end;

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

   end Evaluate_Command_Line;

   -------------
   -- Project --
   -------------

   function Project return String is
   begin
      return Project_Arg.all;
   end Project;

   function Project return GNAT.Strings.String_Access is
   begin
      return Project_Arg;
   end Project;

   -------------
   -- Plugins --
   -------------

   function Plugins return String is
   begin
      return Plugins_Arg.all;
   end Plugins;

   function Plugins return GNAT.Strings.String_Access is
   begin
      return Plugins_Arg;
   end Plugins;

   ------------
   -- Script --
   ------------

   function Script return String is
   begin
      return Script_Arg.all;
   end Script;

   function Script return GNAT.Strings.String_Access is
   begin
      return Script_Arg;
   end Script;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      GNAT.Command_Line.Free (Config);
   end Finalize;

end GNAThub.Configuration;
