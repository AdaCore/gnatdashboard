------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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

package GNAThub.Configuration is

   Command_Line_Error : exception;
   --  Raised on invalid input on the command line

   procedure Initialize;
   --  Initialize and parse the command line

   function Project return String;
   function Project return GNAT.Strings.String_Access;
   --  Return the Project name given on the command line

   function Plugins return String;
   function Plugins return GNAT.Strings.String_Access;
   --  Return the list of plugins to execute, as a comma-separated string

   function Script return String;
   function Script return GNAT.Strings.String_Access;
   --  Return the Script to execute if given on the command line. Return the
   --  empty string otherwise.

   function Target return String;
   function Target return GNAT.Strings.String_Access;
   --  Return the Target attribute if given on the command line. Return the
   --  empty string otherwise.

   function Runtime return String;
   function Runtime return GNAT.Strings.String_Access;
   --  Return the Runtime attribute if given on the command line. Return the
   --  empty string otherwise. This concerns only the runtime for Ada.

   function Jobs return Natural;
   --  Return the number of jobs to execute in parallel

   function Quiet return Boolean;
   --  Informative messages can be retained in quiet mode

   function Verbose return Boolean;
   --  Additional information can be displayed in verbose mode

   function Interpreter_Mode return Boolean;
   --  Whether to run GNAThub in interpreter mode (--exec) or normal mode

   function Incremental return Boolean;
   --  Whether the run should be incremental (not removing the DB) or not

   procedure Finalize;
   --  Free allocated memory

end GNAThub.Configuration;
