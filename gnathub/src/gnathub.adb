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

with GNATCOLL.Traces;               use GNATCOLL.Traces;

package body GNAThub is

   package body Log is

      Application_Start_Time : constant Time := Clock;

      Info_Handle    : constant Trace_Handle := Create ("INFO", On);
      Warning_Handle : constant Trace_Handle := Create ("WARN", On);
      Fatal_Handle   : constant Trace_Handle := Create ("FATAL", On);
      Debug_Handle   : constant Trace_Handle := Create ("TRACE", Off);
      --  Trace handles

      Ada_Verbosity    : Verbosity_Level := Default;
      --  Verbosity of the Ada Trace engine.

      Python_Verbosity : Natural := 20;
      --  Verbosity of the Python Trace engine.
      --    Quiet   => 40
      --    Default => 20
      --    Verbose => 10

      ----------
      -- Info --
      ----------

      procedure Info (Message : String) is
      begin
         Trace (Info_Handle, Message);
      end Info;

      ----------
      -- Warn --
      ----------

      procedure Warn (Message : String) is
      begin
         Trace (Warning_Handle, Message);
      end Warn;

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

         case Verbosity is
            when Quiet =>
               Set_Active (Debug_Handle, False);
               Set_Active (Info_Handle, False);
               Set_Active (Warning_Handle, False);
               Set_Active (Fatal_Handle, True);

               Python_Verbosity := 40;

            when Default =>
               Set_Active (Debug_Handle, False);
               Set_Active (Info_Handle, True);
               Set_Active (Warning_Handle, True);
               Set_Active (Fatal_Handle, True);

               Python_Verbosity := 20;

            when Verbose =>
               Set_Active (Create ("DEBUG.ABSOLUTE_TIME"), True);

               Set_Active (Debug_Handle, True);
               Set_Active (Info_Handle, True);
               Set_Active (Warning_Handle, True);
               Set_Active (Fatal_Handle, True);

               Python_Verbosity := 10;

         end case;
      end Set_Verbosity;

      ---------------
      -- Verbosity --
      ---------------

      function Verbosity return Verbosity_Level is
      begin
         return Ada_Verbosity;
      end Verbosity;

      ---------------
      -- Verbosity --
      ---------------

      function Verbosity return Integer is
      begin
         return Python_Verbosity;
      end Verbosity;

   end Log;

end GNAThub;
