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

with GNAT.Command_Line;
with GNAT.Strings;

with GPS.CLI_Kernels;
with GPS;

package GNAThub.Configuration is

   type Command_Line is tagged private;
   --  Command line for qualimetrics

   procedure Initialize (Self : in out Command_Line);
   --  Define switches handle by qualimetrics

   procedure Parse
     (Self : in out Command_Line;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Parses the command line.

   function Project_Name (Self : Command_Line) return String;
   --  Returns Project name given on the command line.

   function Script_Arg (Self : Command_Line) return String;
   --  Returns value for --load switch
   --  Example: --load=python:example.py: return 'python:example.py'

   function Project_Name
     (Self : Command_Line) return GNAT.Strings.String_Access;
   --  Same as Project_Name but returns an access type.

   function Script_Arg
     (Self : Command_Line) return GNAT.Strings.String_Access;
   --  Same as Project_Name but returns an access type.

   procedure Finalize (Self : in out Command_Line);
   --  Frees allocated memory.

private

   type Command_Line is tagged record
      Command_Line : GNAT.Command_Line.Command_Line_Configuration;
      Project_Name : aliased GNAT.Strings.String_Access;
      Script_Arg   : aliased GNAT.Strings.String_Access;
      Plugins      : aliased GNAT.Strings.String_Access;
      Version      : aliased Boolean;
      Quiet        : aliased Boolean;
      Verbose      : aliased Boolean;
   end record;

end GNAThub.Configuration;
