------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;
with GPS.CLI_Utils;
with Ada.Calendar.Formatting;
package body Utils is

   Traces_Separator : constant String := "---------------------------------" &
                                    "---------------------------------------";

   procedure Trace_End_Stats;
   --  ??

   -----------------------
   --  Trace_End_Stats  --
   -----------------------

   procedure Trace_End_Stats
   is
      Total_Time : Duration;
   begin
      Total_Time := Ada.Calendar.Clock - Program_Starting_Time;
      Trace (Utils.Info_Trace, "Total time: " &
               Ada.Calendar.Formatting.Image (Total_Time, True));
      Trace (Utils.Info_Trace, "Finished at : "
             & Ada.Calendar.Formatting.Image (Clock));
      Utils.Trace_Separator;
   end Trace_End_Stats;

   -----------------------
   --  Trace_Separator  --
   -----------------------

   procedure Trace_Separator is
   begin
      Trace (Info_Trace, Traces_Separator);
   end Trace_Separator;

   -------------------------
   --  Trace_End_Success  --
   -------------------------

   procedure Trace_End_Success is
   begin
      Trace_Separator;
      Trace (Info_Trace, "QUALIMETRICS SUCCESS");
      Trace_Separator;
      Trace_End_Stats;
   end Trace_End_Success;

   -------------------------
   --  Trace_End_Failure  --
   -------------------------

   procedure Trace_End_Failure is
   begin
      Trace_Separator;
      Trace (Info_Trace, "QUALIMETRICS FAILURE");
      Trace_Separator;
      Trace_End_Stats;
   end Trace_End_Failure;

   ------------------------
   --  Set_Quiet_Traces  --
   ------------------------

   procedure Set_Quiet_Traces is
   begin
      --  Only error traces remain active
      GNATCOLL.Traces.Set_Active (Info_Trace, False);
      GNATCOLL.Traces.Set_Active (Warn_Trace, False);
      GNATCOLL.Traces.Set_Active (Debug_Trace, False);
   end Set_Quiet_Traces;

   --------------------------
   --  Set_Verbose_Traces  --
   --------------------------

   procedure Set_Verbose_Traces is
   begin
      --  Default setting is:
      --  INFO: On / WARNING: On / ERROR: On / DEBUG: Off.
      GNATCOLL.Traces.Set_Active (Debug_Trace, True);
   end Set_Verbose_Traces;

   -----------------------
   -- Return_On_Failure --
   -----------------------

   function Return_On_Failure (Error_Message : String) return Boolean is
   begin
      GNATCOLL.Traces.Trace (Error_Trace, Error_Message);
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      Trace_End_Failure;
      return False;
   end Return_On_Failure;

   --------------------
   -- Execute_Script --
   --------------------

   function Execute_Script
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String) return Boolean
   is
      Colon : constant Natural :=
        Ada.Strings.Fixed.Index (Script_Arg.all, ":");
   begin
      if Colon /= 0 then

         Trace (Utils.Debug_Trace, "  Script: "
                & Script_Arg (Colon + 1 .. Script_Arg'Last));
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
