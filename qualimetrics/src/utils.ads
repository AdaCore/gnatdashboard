------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GPS;             use GPS;
with GPS.CLI_Kernels;

package Utils is

   function Return_On_Failure (Message : String) return Boolean;
   --  ???

   function Execute_Script
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String) return Boolean;
   --  ???

end Utils;
