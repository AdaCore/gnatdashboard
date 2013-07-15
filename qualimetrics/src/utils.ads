------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GPS.CLI_Kernels;
with GPS;             use GPS;
with Ada.Calendar; use Ada.Calendar;

package Utils is

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
   procedure Trace_End_Success;
   procedure Trace_End_Failure;

   procedure Set_Quiet_Traces;
   --  Deactive all traces except error traces

   procedure Set_Verbose_Traces;
   --  Active debug traces, so all traces are active

   function Return_On_Failure (Error_Message : String) return Boolean;
   --  Trace the error message with the error traces handler.
   --  Set exit code to failure exit code.
   --  Return false.

   function Execute_Script
     (Kernel     : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Script_Arg : access String) return Boolean;
   --  ??

end Utils;
