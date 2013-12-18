------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GNATCOLL.Traces;               use GNATCOLL.Traces;

package body GNAThub is

   package body Log is
      Application_Start_Time : constant Time := Clock;

      Info_Handle    : constant Trace_Handle := Create ("INFO", On);
      Warning_Handle : constant Trace_Handle := Create ("WARN", On);
      Error_Handle   : constant Trace_Handle := Create ("ERROR", On);
      Fatal_Handle   : constant Trace_Handle := Create ("FATAL", On);
      Debug_Handle   : constant Trace_Handle := Create ("TRACE", Off);
      --  Trace handles

      Ada_Verbosity    : Verbosity_Level := Default;
      --  Verbosity of the Ada Trace engine

      ----------
      -- Info --
      ----------

      procedure Info (Message : String) is
      begin
         if Ada_Verbosity = Default then
            Put_Line (Message);
         elsif Ada_Verbosity = Verbose then
            Trace (Info_Handle, Message);
         end if;
      end Info;

      ----------
      -- Warn --
      ----------

      procedure Warn (Message : String) is
      begin
         if Ada_Verbosity = Default then
            Put_Line ("warning: " & Message);
         elsif Ada_Verbosity = Verbose then
            Trace (Warning_Handle, Message);
         end if;
      end Warn;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         if Ada_Verbosity = Default then
            Put_Line ("error: " & Message);
         elsif Ada_Verbosity = Verbose then
            Trace (Error_Handle, Message);
         end if;
      end Error;

      -----------
      -- Fatal --
      -----------

      procedure Fatal (Message : String) is
      begin
         Trace (Fatal_Handle, Message);
      end Fatal;

      -----------
      -- Debug --
      -----------

      procedure Debug (Message : String) is
      begin
         Trace (Debug_Handle, Message);
      end Debug;

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
           " out of " & Image (Total) & " (" & Image (Percent) & "%)";

      begin
         if Ada_Verbosity = Default then
            Put (Message & ASCII.CR);

            if New_Line then
               Ada.Text_IO.New_Line;
            end if;

         elsif Ada_Verbosity = Verbose then
            Trace (Info_Handle, Message);
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

      -------------------
      -- Set_Verbosity --
      -------------------

      procedure Set_Verbosity (Verbosity : Verbosity_Level) is
      begin
         Ada_Verbosity := Verbosity;

         Set_Active (Fatal_Handle, True);
         Set_Active (Error_Handle, True);

         Set_Active (Info_Handle, Ada_Verbosity > Quiet);
         Set_Active (Warning_Handle, Ada_Verbosity > Quiet);

         Set_Active (Debug_Handle, Ada_Verbosity = Verbose);

         Set_Active (Create ("DEBUG.ABSOLUTE_TIME"), Ada_Verbosity = Verbose);
         Set_Active (Create ("DEBUG.COLORS"), Ada_Verbosity = Verbose);
      end Set_Verbosity;

   end Log;

end GNAThub;
