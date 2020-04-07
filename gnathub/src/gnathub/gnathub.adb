------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with Ada.Text_IO;                   use Ada.Text_IO;

with GNATCOLL.Traces;               use GNATCOLL.Traces;

package body GNAThub is
   Me : constant Trace_Handle := Create ("GNATHUB.OUTPUT");
   --  The handle to use to log messages displayed with Info, Warn, Error and
   --  Fail procedures.

   Output_Verbosity : Verbosity_Level := Default;
   --  Verbosity of the program

   IDE_Progress_Bar : Boolean := False;
   --  Whether a progress bar for IDEs should be displayed

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

      Trace (Me, Output);
   end Info;

   ----------
   -- Warn --
   ----------

   procedure Warn (Message : String; Prefix : String := Console_Prefix) is
      Output : constant String := Format_Message
                                    ("warning: " & Message, Prefix);
   begin
      Put_Line (Standard_Error, Output);
      Trace (Me, Output);
   end Warn;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String; Prefix : String := Console_Prefix) is
      Output : constant String := Format_Message ("error: " & Message, Prefix);
   begin
      Global_Error :=
        Message = "GNAThub error: one or more plugins failed to run!";

      Put_Line (Standard_Error, Output);
      Trace (Me, Output);
   end Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (E      : Ada.Exceptions.Exception_Occurrence;
      Prefix : String := Console_Prefix) is
   begin
      Error (Ada.Exceptions.Exception_Message (E), Prefix);
      Trace (Me, E);
   end Error;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String; Prefix : String := Console_Prefix) is
      Output : constant String := Format_Message ("fatal: " & Message, Prefix);
   begin
      Put_Line (Standard_Error, Output);
      Trace (Me, Output);

      raise Silent_Error;
   end Fail;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (Verbosity : Verbosity_Level) is
   begin
      Output_Verbosity := Verbosity;
      Trace (Me, "Output_Verbosity = " & Verbosity_Level'Image (Verbosity));
   end Set_Verbosity;

   --------------------------
   -- Set_IDE_Progress_Bar --
   --------------------------

   procedure Set_IDE_Progress_Bar (Progress : Boolean) is
   begin
      IDE_Progress_Bar := Progress;
   end Set_IDE_Progress_Bar;

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
      Message : constant String  := "completed " & Image (Current) &
                                    " out of " & Image (Total) &
                                    " (" & Image (Percent) & "%)";

   begin
      if Output_Verbosity >= Default then
         if IDE_Progress_Bar then
            Put_Line (Message & "...");
         else
            Put ("... " & Message & ASCII.CR);

            if New_Line then
               Ada.Text_IO.New_Line;
            end if;
         end if;
      end if;

      Trace (Me, Message);
   end Progress;

end GNAThub;
