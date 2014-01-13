------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2014, AdaCore                     --
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

package GNAThub is

   Error        : exception;
   Fatal_Error  : exception;
   Silent_Error : exception;

   package Log is
      type Verbosity_Level is (Quiet, Default, Verbose);
      --  The three possible threshold of verbosity for this application.

      procedure Info (Message : String);
      --  Prints an informative message. Activated at default verbosity output.

      procedure Warn (Message : String);
      --  Prints a warning message. Activated at default verbosity output.

      procedure Error (Message : String);
      --  Prints an error message. Always activated.

      procedure Fatal (Message : String);
      --  Prints a fatal message. Always activated.

      procedure Debug (Message : String);
      --  Prints a debug message. Activated at higher verbosity level.

      procedure Progress
        (Current : Natural; Total : Positive; New_Line : Boolean := False);
      --  Prints a progess message. Activated at default verbosity level.
      --  If New_Line is True, then terminate the line with a ASCII.LF
      --  character. Defaults to False.

      procedure Ellapsed;
      --  Prints the ellapsed time since the beginning of the application
      --  execution.

      procedure Set_Verbosity (Verbosity : Verbosity_Level);
      --  Sets the output verbosity of the GNAThub application.
   end Log;

end GNAThub;
