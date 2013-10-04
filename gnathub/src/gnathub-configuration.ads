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

with GNAT.Strings;

with GPS.CLI_Kernels;

package GNAThub.Configuration is

   procedure Initialize;
   --  Defines switches handled by GNAThub.

   procedure Parse (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Parses the command line.

   function Project return String;
   --  Returns Project name given on the command line.

   function Project return GNAT.Strings.String_Access;
   --  Same as Project but returns an access type.

   function Script return String;
   --  Returns value for --load switch
   --  Example: --load=python:example.py: return 'python:example.py'

   function Script return GNAT.Strings.String_Access;
   --  Same as Script but returns an access type.

   function Plugins return String;
   --  Returns the list of plugins to execute, as a comma separated string.

   function Plugins return GNAT.Strings.String_Access;
   --  Same as Plugins but returns an access type.

   procedure Finalize;
   --  Frees allocated memory.

end GNAThub.Configuration;
