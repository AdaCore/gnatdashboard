------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GPS.CLI_Kernels;
with GPS;             use GPS;

package Utils is

   function Return_On_Failure (Error_Message : String) return Boolean;
   --  Trace the error message with the error traces handler.
   --  Set exit code to failure exit code.
   --  Return false.

   function Execute_Script
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String) return Boolean;
   --  ??

end Utils;
