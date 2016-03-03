------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

   function Format_Message
     (Message : String; Prefix : String := "") return String;
   --  Format the message with the prefix if given

   ------------------------
   -- Initialize_Logging --
   ------------------------

   procedure Initialize_Logging is
   begin
      GNATCOLL.Traces.Parse_Config_File;
   end Initialize_Logging;

   --------------------
   -- Format_Message --
   --------------------

   function Format_Message
     (Message : String; Prefix : String := "") return String
   is
      Msg : constant String :=
              (if Prefix /= "" then Prefix & ": " else "") & Message;
   begin
      return Msg;
   end Format_Message;

   ----------
   -- Info --
   ----------

   procedure Info
     (Message      : String;
      New_Line     : Boolean         := True;
      Prefix       : String          := Console_Prefix;
      Availability : Verbosity_Level := Default)
   is
      Output : constant String := Format_Message (Message, Prefix);
   begin
      if Output_Verbosity >= Availability then
         Put (Output);

         if New_Line then
            Ada.Text_IO.New_Line;
         end if;
      end if;

      Log.Info (Me, Output);
   end Info;

   ----------
   -- Warn --
   ----------

   procedure Warn (Message : String; Prefix : String := Console_Prefix)
   is
      Output : constant String := Format_Message
                                    ("warning: " & Message, Prefix);
   begin
      Put_Line (Standard_Error, Output);
      Log.Warn (Me, Output);
   end Warn;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String; Prefix : String := Console_Prefix)
   is
      Output : constant String := Format_Message ("error: " & Message, Prefix);
   begin
      Put_Line (Standard_Error, Output);
      Log.Error (Me, Output);
   end Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (E      : Ada.Exceptions.Exception_Occurrence;
      Prefix : String := Console_Prefix) is
   begin
      Error (Ada.Exceptions.Exception_Message (E), Prefix);
      Log.Exception_Raised (Me, E);
   end Error;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String; Prefix : String := Console_Prefix)
   is
      Output : constant String := Format_Message ("fatal: " & Message, Prefix);
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
      Log.Info (Me,
         "Output_Verbosity set to " & Verbosity_Level'Image (Verbosity));
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

      Log.Info (Me, Message);
   end Progress;

   -------------
   -- Elapsed --
   -------------

   procedure Elapsed is
      Total : constant Duration := Clock - Application_Start_Time;
   begin
      Info ("Elapsed time: " & Ada.Calendar.Formatting.Image (Total));
   end Elapsed;

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
         Log.Info (Me, "Log_Level set to " & Log_Level'Image (Level));
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
