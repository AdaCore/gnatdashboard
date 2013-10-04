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

with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;       use GNATCOLL.Scripts.Python;

with GNAThub.Constants;             use GNAThub.Constants;
with GNAThub.Configuration;         use GNAThub.Configuration;

package body GNAThub.Python is

   Repository  : Scripts_Repository := null;

   Log_Info_Method  : aliased String := "info";
   Log_Warn_Method  : aliased String := "warn";
   Log_Fatal_Method : aliased String := "fatal";
   Log_Debug_Method : aliased String := "debug";

   Log_Class_Static_Methods : constant array (1 .. 4) of access String :=
      (Log_Info_Method'Access,
       Log_Warn_Method'Access,
       Log_Fatal_Method'Access,
       Log_Debug_Method'Access);

   Root_Function         : aliased String := "root";
   Logs_Function         : aliased String := "logs";
   Plugins_Function      : aliased String := "plugins";
   User_Plugins_Function : aliased String := "user_plugins";
   Core_Plugins_Function : aliased String := "core_plugins";
   Database_Function     : aliased String := "database";

   Root_Module_Functions : constant array (1 .. 6) of access String :=
      (Root_Function'Access,
       Logs_Function'Access,
       Plugins_Function'Access,
       User_Plugins_Function'Access,
       Core_Plugins_Function'Access,
       Database_Function'Access);

   procedure Root_Module_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Env class methods handler.

   procedure Log_Class_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Log class methods handler.

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Repository /= null;
   end Initialized;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Register_Core_Modules;
      --  Registers the core modules and functions of GNAThub Python module.

      ---------------------------
      -- Register_Core_Modules --
      ---------------------------

      procedure Register_Core_Modules
      is
         Log_Class : constant Class_Type := Repository.New_Class ("Log");
      begin
         --  GNAThub module

         for Command of Root_Module_Functions loop
            Repository.Register_Command
              (Command       => Command.all,
               Params        => No_Params,
               Handler       => Root_Module_Handler'Access);
         end loop;

         --  GNAThub.Log class

         for Command of Log_Class_Static_Methods loop
            Repository.Register_Command
              (Command       => Command.all,
               Params        => (1 .. 1 => Param ("message")),
               Handler       => Log_Class_Handler'Access,
               Class         => Log_Class,
               Static_Method => True);
         end loop;
      end Register_Core_Modules;

   begin
      if Repository = null then
         Repository := new Scripts_Repository_Record;

         Register_Python_Scripting
           (Repository, Python_Root_Module,
            Python_Home => Share_Dir.Display_Full_Name);

         Register_Standard_Classes
           (Repository,
            Console_Class_Name => "Console",
            Logger_Class_Name  => "Traces");

         Register_Core_Modules;
      end if;
   end Initialize;

   ---------------
   -- Finalize; --
   ---------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -----------------------
   -- Log_Class_Handler --
   -----------------------

   procedure Log_Class_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Message : constant String := Data.Nth_Arg (1);
   begin
      if Command = Log_Info_Method then
         Log.Info (Message);

      elsif Command = Log_Warn_Method then
         Log.Warn (Message);

      elsif Command = Log_Fatal_Method then
         Log.Fatal (Message);

      elsif Command = Log_Debug_Method then
         Log.Debug (Message);

      else
         raise Python_Error with "Unknown method GNAThub.Log." & Command;
      end if;
   end Log_Class_Handler;

   -------------------------
   -- Root_Module_Handler --
   -------------------------

   procedure Root_Module_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = User_Plugins_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.User_Plugin_Dir.Display_Full_Name);

      elsif Command = Core_Plugins_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Core_Plugin_Dir.Display_Full_Name);

      elsif Command = Root_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Root_Dir.Display_Full_Name);

      elsif Command = Database_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Database_File.Display_Full_Name);

      elsif Command = Logs_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Logs_Dir.Display_Full_Name);

      elsif Command = Plugins_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Plugins);

      else
         raise Python_Error with "Unknown method GNAThub.Env." & Command;
      end if;
   end Root_Module_Handler;

end GNAThub.Python;