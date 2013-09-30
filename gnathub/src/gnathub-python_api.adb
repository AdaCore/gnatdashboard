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

with GNAThub.Constants;       use GNAThub.Constants;
with GNAThub.Scripts;

with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

package body GNAThub.Python_Api is

   Repository : constant Scripts_Repository := new Scripts_Repository_Record;

   procedure Plugin_To_Execute
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Log_Level
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Exec_Dir
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Logs_Dir
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure DB_Relative_Path
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure User_Plugins_Dir
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Core_Plugins_Dir
     (Data    : in out Callback_Data'Class;
      Command : String);

   ---------------
   -- Log_Level --
   ---------------

   procedure Log_Level
     (Data    : in out Callback_Data'Class;
      Command : String)
      is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Integer'(Log.Verbosity));
   end Log_Level;

   -----------------------
   -- Plugin_To_Execute --
   -----------------------

   procedure Plugin_To_Execute
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      if GNAThub.Scripts.Plugins_To_Execute.all = "" then
         Set_Return_Value_As_List (Data);
      else
         Set_Return_Value (Data, GNAThub.Scripts.Plugins_To_Execute.all);
      end if;
   end Plugin_To_Execute;

   --------------
   -- Exec_Dir --
   --------------

   procedure Exec_Dir
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, GNAThub.Constants.Exec_Dir.Display_Full_Name);
   end Exec_Dir;

   --------------
   -- Logs_Dir --
   --------------

   procedure Logs_Dir
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, GNAThub.Constants.Logs_Dir.Display_Full_Name);
   end Logs_Dir;

   ----------------------
   -- DB_Relative_Path --
   ----------------------

   procedure DB_Relative_Path
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, DB_File.Display_Full_Name);
   end DB_Relative_Path;

   ----------------------
   -- Core_Plugins_Dir --
   ----------------------

   procedure Core_Plugins_Dir
     (Data    : in out Callback_Data'Class;
      Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, Core_Plugin_Dir.Display_Full_Name);
   end Core_Plugins_Dir;

   ----------------------
   -- User_Plugins_Dir --
   ----------------------

   procedure User_Plugins_Dir
     (Data    : in out Callback_Data'Class;
      Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, User_Plugin_Dir.Display_Full_Name);
   end User_Plugins_Dir;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Register python module

      GNATCOLL.Scripts.Python.Register_Python_Scripting
        (Repository, Python_Root_Module);
      Register_Standard_Classes (Repository, "Console");

      --  Register commands
      GNATCOLL.Scripts.Register_Command
        (Repository, "user_plugin_dir", 0, 0, User_Plugins_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "core_plugin_dir", 0, 0, Core_Plugins_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "db_relative_path", 0, 0, DB_Relative_Path'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "logs_dir", 0, 0, Logs_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "qmt_root_dir", 0, 0, Exec_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "log_level", 0, 0, Log_Level'Access);

      GNATCOLL.Scripts.Register_Command
        (Repository, "plugins_to_execute", 0, 0, Plugin_To_Execute'Access);
   end Initialize;

end GNAThub.Python_Api;
