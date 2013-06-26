------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with GNATCOLL.VFS;   use GNATCOLL.VFS;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package Core_Properties is

   --  Qualimetrics executable side environment properties
   Qmt_Prefix_Dir      : constant Virtual_File :=
     Create (+Executable_Location);
   Qmt_Core_Dir        : constant Virtual_File :=
     Create_From_Dir (Qmt_Prefix_Dir, "share/qualimetrics/core");
   Qmt_Core_Plugin_Dir : constant Virtual_File :=
     Create_From_Dir (Qmt_Prefix_Dir, "share/qualimetrics/core/plug-ins");
   Qmt_User_Plugin_Dir : constant Virtual_File :=
     Create_From_Dir (Qmt_Prefix_Dir, "share/qualimetrics/plug-ins");

   --  Qualimetrics project side environment properties, located in project
   --  object directory
   Project_Qmt_Dir_Name : constant Filesystem_String := "qualimetrics";
   Project_Log_Dir_Name : constant Filesystem_String := "logs";

   DB_File_Name : constant Filesystem_String := "qualimetrics.db";
   Qmt_Python_Module_Name : constant String := "Qmt";
end Core_Properties;
