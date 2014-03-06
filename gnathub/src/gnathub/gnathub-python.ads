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

package GNAThub.Python is

   Python_Error : exception;
   --  Exception raised upon Python-related error (e.g. call to unexisting
   --  function/method).

   function Initialized return Boolean;
   --  Return whether the Python engine has been initialized or not

   procedure Initialize;
   --  Register the GNAThub Python interface

   procedure Finalize
      with Pre => Initialized;
   --  Dispose of internal resources

   procedure Execute_File (Script_Filename : String; Errors : out Boolean)
      with Pre => Initialized;
   --  Execute the given Python script

   procedure Execute (Cmd : String; Errors : out Boolean)
      with Pre => Initialized;
   --  Execute the given command

end GNAThub.Python;
