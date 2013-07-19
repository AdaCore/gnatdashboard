------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;

package body Logger is

   Traces_Separator : constant String := "---------------------------------" &
                                   "---------------------------------------";

   procedure Trace_End_Stats;
   --  Trace total execution time and end date of the program.

   -----------------------
   --  Trace_End_Stats  --
   -----------------------

   procedure Trace_End_Stats
   is
      Total_Time : Duration;
   begin
      Total_Time := Ada.Calendar.Clock - Program_Starting_Time;
      Trace (Info_Trace, "Total time: " &
               Ada.Calendar.Formatting.Image (Total_Time, True));
      Trace (Info_Trace, "Finished at : "
             & Ada.Calendar.Formatting.Image (Clock));
      Trace_Separator;
   end Trace_End_Stats;

   -----------------------
   --  Trace_Separator  --
   -----------------------

   procedure Trace_Separator is
   begin
      Trace (Info_Trace, Traces_Separator);
   end Trace_Separator;

   -------------------
   --  Trace_Title  --
   -------------------

   procedure Trace_Title (Title : String) is
   begin
      Trace_Separator;
      Trace (Info_Trace, Title);
      Trace_Separator;
   end Trace_Title;

   -------------------------
   --  Trace_End_Success  --
   -------------------------

   procedure Trace_End_Success is
   begin
      Trace_Title ("QUALIMETRICS SUCCESS");
      Trace_End_Stats;
   end Trace_End_Success;

   -------------------------
   --  Trace_End_Failure  --
   -------------------------

   procedure Trace_End_Failure is
   begin
      Trace (Info_Trace, "QUALIMETRICS FAILURE");
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
      Logger.Log_Level := 40;
   end Set_Quiet_Traces;

   --------------------------
   --  Set_Verbose_Traces  --
   --------------------------

   procedure Set_Verbose_Traces is
   begin
      --  Default setting is:
      --  INFO: On / WARNING: On / ERROR: On / DEBUG: Off.
      GNATCOLL.Traces.Set_Active (Debug_Trace, True);
      Logger.Log_Level := 10;
   end Set_Verbose_Traces;

end Logger;
