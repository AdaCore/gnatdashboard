------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Command_Line;
with GNAT.IO;          use GNAT.IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with GPS.CLI_Utils;

package body Utils is

   -----------------------
   -- Return_On_Failure --
   -----------------------

   function Return_On_Failure (Message : String) return Boolean is
   begin
      --  /!\ Replace with Traces
      Put_Line (Message);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return False;
   end Return_On_Failure;

   function Execute_Script
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String) return Boolean
   is
      Colon : constant Natural :=
        Ada.Strings.Fixed.Index (Script_Arg.all, ":");
   begin
      if Colon /= 0 then

         if not GPS.CLI_Utils.Execute_Batch
           (Kernel,
            Lang_Name   => Script_Arg (Script_Arg'First .. Colon - 1),
            Script_Name => Script_Arg (Colon + 1 .. Script_Arg'Last))
         then
            return Return_On_Failure
              ("Language unknown for --load command line switch: "
               & Script_Arg (Script_Arg'First .. Colon - 1));
         else
            return True;
         end if;

      else
         return Return_On_Failure
           ("No lang in --load=" & Script_Arg.all);
      end if;
   end Execute_Script;

end Utils;
