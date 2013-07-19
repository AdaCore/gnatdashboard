------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Traces; use GNATCOLL.Traces;
with Ada.Calendar; use Ada.Calendar;

--  Package that gathered definition of Trace for qualimetrics
--  Qualimetrics handles 4 type of Trace: INFO, WARN, ERROR, DEBUG
--  and 3 level of log verbosity:
--    - quiet  : deactives all Trace type except ERROR
--    - default: actives INFO, WARNING, ERROR
--    - verbose: actives all Traces
package Logger is

   --  Level for logging
   --  Used to synchronize logging level between the Ada part and Python part
   --  of qualimetrics: 40 -> quiet, 20 -> default, 10 -> verbose
   --  (Those value correspond to python logging module level value)
   --  This variable is updated through Set_Quiet_Traces and
   --  Set_Verbose_Traces procedures
   Log_Level : Natural := 20;

   Program_Starting_Time : Time;

   Info_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("INFO", On);

   Warn_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("WARNING", On);

   Error_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("ERROR", On);

   Debug_Trace : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("DEBUG", Off);

   procedure Trace_Separator;
   --  Output 80 columns dashed line separator, for trace readability

   procedure Trace_Title (Title : String);
   --  Trace title surrounded by separator

   procedure Trace_End_Success;
   --  ???

   procedure Trace_End_Failure;
   --  ???

   procedure Set_Quiet_Traces;
   --  Deactive all traces except error traces

   procedure Set_Verbose_Traces;
   --  Active debug traces, so all traces are active

end Logger;
