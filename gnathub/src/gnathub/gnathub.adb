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

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;                   use Ada.Text_IO;

package body GNAThub is
   Me : constant Trace_Handle := Create ("GNATHUB.OUTPUT");
   --  The handle to use to log messages displayed with Info, Warn, Error and
   --  Fail procedures.

   Application_Start_Time : constant Time := Clock;
   --  The start time of the application, used by Ellapsed

   Output_Verbosity : Verbosity_Level := Default;
   --  Verbosity of the program

   ------------------------
   -- Initialize_Logging --
   ------------------------

   procedure Initialize_Logging is
   begin
      GNATCOLL.Traces.Parse_Config_File;

      Set_Active (Create ("DEBUG.ABSOLUTE_TIME"), True);
      Set_Active (Create ("DEBUG.COLORS"), True);
   end Initialize_Logging;

   ----------
   -- Info --
   ----------

   procedure Info
     (Message      : String;
      Availability : Verbosity_Level := Default)
   is
      Output : constant String := "gnathub: " & Message;
   begin
      if Output_Verbosity >= Availability then
         Put_Line (Output);
      end if;

      Log.Info (Me, Output);
   end Info;

   ----------
   -- Warn --
   ----------

   procedure Warn (Message : String)
   is
      Output : constant String := "warning: " & Message;
   begin
      Put_Line (Standard_Error, Output);
      Log.Warn (Me, Output);
   end Warn;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String)
   is
      Output : constant String := "error: " & Message;
   begin
      Put_Line (Standard_Error, Output);
      Log.Error (Me, Output);
   end Error;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String)
   is
      Output : constant String := "fatal: " & Message;
   begin
      Put_Line (Standard_Error, Output);
      Log.Fatal (Me, Output);

      raise Silent_Error;
   end Fail;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (Verbosity : Verbosity_Level) is
   begin
      Output_Verbosity := Verbosity;
   end Set_Verbosity;

   --------------
   -- Progress --
   --------------

   procedure Progress
     (Current : Natural; Total : Positive; New_Line : Boolean := False)
   is
      function Image (Number : Integer) return String;
      --  Prints the image of a number w/o the leading space

      -----------
      -- Image --
      -----------

      function Image (Number : Integer) return String is
         Img : constant String := Integer'Image (Number);
      begin
         return Img (Img'First + 1 .. Img'Last);
      end Image;

      Percent : constant Integer := Current * 100 / Total;
      Message : constant String  := "... completed " & Image (Current) &
                                    " out of " & Image (Total) &
                                    " (" & Image (Percent) & "%)";

   begin
      if Output_Verbosity >= Default then
         Put (Message & ASCII.CR);

         if New_Line then
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end Progress;

   --------------
   -- Ellapsed --
   --------------

   procedure Ellapsed is
      Total : constant Duration := Clock - Application_Start_Time;
   begin
      Info ("Ellapsed time: " & Ada.Calendar.Formatting.Image (Total));
   end Ellapsed;

   -----------------
   -- Package Log --
   -----------------

   package body Log is
      Log_Verbosity : Log_Level := Log_All;
      --  Verbosity of the trace engine

      procedure Log_If_At_Least
        (Handle  : Trace_Handle;
         Message : String;
         Level   : Log_Level := Log_Info);
      --  Log the message if Output_Verbosity matches at least Level

      ---------------------
      -- Log_If_At_Least --
      ---------------------

      procedure Log_If_At_Least
        (Handle  : Trace_Handle;
         Message : String;
         Level   : Log_Level := Log_Info)
      is
      begin
         if Log_Verbosity >= Level then
            Trace (Handle, Message);
         end if;
      end Log_If_At_Least;

      -------------------
      -- Set_Log_Level --
      -------------------

      procedure Set_Log_Level (Level : Log_Level) is
      begin
         Log_Verbosity := Level;
      end Set_Log_Level;

      -----------
      -- Debug --
      -----------

      procedure Debug (Handle : Trace_Handle; Message : String) is
      begin
         Log_If_At_Least (Handle, Message, Log_Debug);
      end Debug;

      ----------
      -- Info --
      ----------

      procedure Info (Handle : Trace_Handle; Message : String) is
      begin
         Log_If_At_Least (Handle, Message, Log_Info);
      end Info;

      ----------
      -- Warn --
      ----------

      procedure Warn (Handle : Trace_Handle; Message : String) is
      begin
         Log_If_At_Least (Handle, Message, Log_Warn);
      end Warn;

      -----------
      -- Error --
      -----------

      procedure Error (Handle : Trace_Handle; Message : String) is
      begin
         Log_If_At_Least (Handle, Message, Log_Error);
      end Error;

      -----------
      -- Fatal --
      -----------

      procedure Fatal (Handle : Trace_Handle; Message : String) is
      begin
         Log_If_At_Least (Handle, Message, Log_Fatal);
      end Fatal;

      ----------------------
      -- Exception_Raised --
      ----------------------

      procedure Exception_Raised
        (Handle : Trace_Handle;
         E      : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Log_Verbosity > Log_None then
            Trace (Handle, E);
         end if;
      end Exception_Raised;

   end Log;

end GNAThub;
