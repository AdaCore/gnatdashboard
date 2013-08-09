------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with Core_Properties;
with Logger;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with Utils;

package body Qmt_Python_Api is

   procedure Plugin_To_Execute
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure Log_Level
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure Qmt_Root_Dir
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure Logs_Dir
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure DB_Relative_Path
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure User_Plugins_Dir
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure Core_Plugins_Dir
     (Data : in out Callback_Data'Class;
      Command : String);

   procedure Log_Level
     (Data : in out Callback_Data'Class;
      Command : String)
      is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Logger.Log_Level);
   end Log_Level;

   procedure Plugin_To_Execute
     (Data : in out Callback_Data'Class;
      Command : String)
    is
      pragma Unreferenced (Command);
   begin
      if Utils.Plugins_To_Execute.all = "" then
         Set_Return_Value_As_List (Data);
      else
         Set_Return_Value (Data, Utils.Plugins_To_Execute.all);
      end if;

   end Plugin_To_Execute;

   procedure Qmt_Root_Dir
     (Data : in out Callback_Data'Class;
      Command : String)
      is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.Project_Qmt_Dir_Name.Display_Full_Name);
   end Qmt_Root_Dir;

   procedure Logs_Dir
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.Project_Log_Dir_Name.Display_Full_Name);
   end Logs_Dir;

   procedure DB_Relative_Path
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.DB_File_Relative_Path.Display_Full_Name);
   end DB_Relative_Path;

   procedure Core_Plugins_Dir
     (Data : in out Callback_Data'Class;
      Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.Qmt_Core_Plugin_Dir.Display_Full_Name);
   end Core_Plugins_Dir;

   procedure User_Plugins_Dir
     (Data : in out Callback_Data'Class;
      Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.Qmt_User_Plugin_Dir.Display_Full_Name);
   end User_Plugins_Dir;

   procedure Initialise (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) is
      pragma Unreferenced (Kernel);
   begin
      --  Register python module

      GNATCOLL.Scripts.Python.Register_Python_Scripting
        (Repo, "Qmt");
      Register_Standard_Classes (Qmt_Python_Api.Repo, "Console");

      --  Register commands
      GNATCOLL.Scripts.Register_Command
        (Repo, "user_plugin_dir", 0, 0, User_Plugins_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "core_plugin_dir", 0, 0, Core_Plugins_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "db_relative_path", 0, 0, DB_Relative_Path'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "logs_dir", 0, 0, Logs_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "qmt_root_dir", 0, 0, Qmt_Root_Dir'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "log_level", 0, 0, Log_Level'Access);

      GNATCOLL.Scripts.Register_Command
        (Repo, "plugins_to_execute", 0, 0, Plugin_To_Execute'Access);
   end Initialise;

end Qmt_Python_Api;
