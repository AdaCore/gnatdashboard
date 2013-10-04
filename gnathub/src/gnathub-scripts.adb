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

with Ada.Strings.Fixed;             use Ada.Strings.Fixed;

with GPS.CLI_Utils;

package body GNAThub.Scripts is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String)
   is
      Colon : constant Natural := Index (Script_Arg.all, ":");
   begin
      if Colon = 0 then
         raise Error with "No lang in --load=" & Script_Arg.all;
      end if;

      Log.Debug ("Script: " & Script_Arg (Colon + 1 .. Script_Arg'Last));

      if not GPS.CLI_Utils.Execute_Batch
               (Kernel,
                Lang_Name   => Script_Arg (Script_Arg'First .. Colon - 1),
                Script_Name => Script_Arg (Colon + 1 .. Script_Arg'Last))
      then
         raise Error with "Invalid script provided to --load: "
                          & Script_Arg (Script_Arg'First .. Colon - 1);
      end if;
   end Execute;

end GNAThub.Scripts;
