------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with Ada.Exceptions;

package GNAThub is

   Fatal_Error    : exception;
   Silent_Error   : exception;
   Exit_Success   : exception;
   Global_Run_Error : exception;

   Console_Prefix : constant String := "gnathub";

   Global_Error : Boolean := False;
   --  This is a global boolean variable which is set at the end of plugin
   --  runner execution if one or more plugins are failing during GNAThub run.
   --  It's default value is set at False.
   --  If its value is set at True a Global_Run_Error will be raised and a
   --  non-zero error code is returned at the end of the GNAThub run.

   type Verbosity_Level is (Quiet, Default, Verbose);
   --  The three possible thresholds of verbosity for this application

   procedure Initialize_Logging;
   --  Setup the logger and load the configuration file if available

   procedure Info
     (Message      : String;
      New_Line     : Boolean         := True;
      Prefix       : String          := Console_Prefix;
      Availability : Verbosity_Level := Default);
   --  Print an informative message. Activated at default verbosity output

   procedure Warn (Message : String; Prefix : String := Console_Prefix);
   --  Print a warning message. Always activated

   procedure Error (Message : String; Prefix : String := Console_Prefix);
   --  Print an error message. Always activated

   procedure Error
     (E      : Ada.Exceptions.Exception_Occurrence;
      Prefix : String := Console_Prefix);
   --  Print the error message and log the exception. Always activated.

   procedure Fail (Message : String; Prefix : String := Console_Prefix);
   --  Print a fatal message and exit. Always activated

   procedure Set_Verbosity (Verbosity : Verbosity_Level);
   --  Set the output verbosity of the GNAThub application

   procedure Set_IDE_Progress_Bar (Progress : Boolean);
   --  Set whether a progress bar for IDEs should be displayed

   procedure Progress
     (Current  : Natural;
      Total    : Positive;
      New_Line : Boolean := False);
   --  Print a progess message. Activated at default verbosity level.
   --  If New_Line is True, then terminate the line with a ASCII.LF
   --  character. Defaults to False.

end GNAThub;
