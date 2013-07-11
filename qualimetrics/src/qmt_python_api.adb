------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with Core_Properties;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Qmt_Python_Api is

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

   procedure Sonar_Work_Dir
     (Data : in out Callback_Data'Class;
      Command : String);

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

   procedure Sonar_Work_Dir
     (Data : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value
        (Data, Core_Properties.Project_Sonar_Dir.Display_Full_Name);
   end Sonar_Work_Dir;

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
        (Repo, "sonar_work_dir", 0, 0, Sonar_Work_Dir'Access);
   end Initialise;

end Qmt_Python_Api;
