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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                   use GNAT.OS_Lib;

with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;       use GNATCOLL.Scripts.Python;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GNAThub.Constants;             use GNAThub.Constants;
with GNAThub.Configuration;         use GNAThub.Configuration;
with GNAThub.Project;

package body GNAThub.Python is

   Repository : Scripts_Repository := null;
   Python     : Python_Scripting   := null;

   Log_Info_Method  : aliased String := "info";
   Log_Warn_Method  : aliased String := "warn";
   Log_Error_Method : aliased String := "error";
   Log_Fatal_Method : aliased String := "fatal";
   Log_Debug_Method : aliased String := "debug";

   Log_Progress_Method : constant String := "progress";

   Log_Class_Static_Methods : constant array (1 .. 5) of access String :=
      (Log_Info_Method'Access,
       Log_Warn_Method'Access,
       Log_Error_Method'Access,
       Log_Fatal_Method'Access,
       Log_Debug_Method'Access);

   Project_Name_Method               : constant String := "name";
   Project_Path_Method               : constant String := "path";
   Project_Object_Dir_Method         : constant String := "object_dir";
   Project_Source_File_Method        : constant String := "source_file";
   Project_Property_As_String_Method : constant String := "property_as_string";
   Project_Property_As_List_Method   : constant String := "property_as_list";

   Root_Function          : aliased String := "root";
   Logs_Function          : aliased String := "logs";
   Plugins_Function       : aliased String := "plugins";
   Core_Plugins_Function  : aliased String := "core_plugins";
   Extra_Plugins_Function : aliased String := "extra_plugins";
   Database_Function      : aliased String := "database";

   Root_Module_Functions : constant array (1 .. 6) of access String :=
      (Root_Function'Access,
       Logs_Function'Access,
       Plugins_Function'Access,
       Core_Plugins_Function'Access,
       Extra_Plugins_Function'Access,
       Database_Function'Access);

   procedure Root_Module_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Env class methods handler

   procedure Log_Class_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Log class methods handler

   procedure Log_Progress_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Log.progress class method handler

   procedure Project_Class_Accessors_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Project.* class method handler

   procedure Project_Class_Properties_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Project.property_as_* class method handler

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Repository /= null and then Python /= null;
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
         Log_Class     : constant Class_Type := Repository.New_Class ("Log");
         Project_Class : constant Class_Type :=
                           Repository.New_Class ("Project");

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

         Repository.Register_Command
           (Command       => Log_Progress_Method,
            Params        =>
              (Param ("current"),
               Param ("total"),
               Param ("new_line", Optional => True)),
            Handler       => Log_Progress_Handler'Access,
            Class         => Log_Class,
            Static_Method => True);

         --  GNAThub.Project class

         Repository.Register_Command
           (Command       => Project_Name_Method,
            Params        => No_Params,
            Handler       => Project_Class_Accessors_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);

         Repository.Register_Command
           (Command       => Project_Path_Method,
            Params        => No_Params,
            Handler       => Project_Class_Accessors_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);

         Repository.Register_Command
           (Command       => Project_Object_Dir_Method,
            Params        => No_Params,
            Handler       => Project_Class_Accessors_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);

         Repository.Register_Command
           (Command       => Project_Source_File_Method,
            Params        => (1 .. 1 => Param ("name")),
            Handler       => Project_Class_Accessors_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);

         Repository.Register_Command
           (Command       => Project_Property_As_String_Method,
            Params        => (1 .. 1 => Param ("property")),
            Handler       => Project_Class_Properties_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);

         Repository.Register_Command
           (Command       => Project_Property_As_List_Method,
            Params        => (1 .. 1 => Param ("property")),
            Handler       => Project_Class_Properties_Handler'Access,
            Class         => Project_Class,
            Static_Method => True);
      end Register_Core_Modules;

   begin
      if Repository = null then
         Repository := new Scripts_Repository_Record;

         Log.Debug
           ("Registering in Python environment: " &
            Python_Home.Display_Full_Name);

         Register_Python_Scripting
           (Repo        => Repository,
            Module      => Python_Root_Module,
            Python_Home => Python_Home.Display_Full_Name);

         Register_Standard_Classes
           (Repo               => Repository,
            Console_Class_Name => "Console",
            Logger_Class_Name  => "Traces");

         Python :=
           GNATCOLL.Scripts.Python.Python_Scripting
             (GNATCOLL.Scripts.Lookup_Scripting_Language
                (Repository, Python_Name));

         Log.Debug ("Registering GNAThub core modules");

         Register_Core_Modules;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

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

      elsif Command = Log_Error_Method then
         Log.Error (Message);

      elsif Command = Log_Fatal_Method then
         Log.Fatal (Message);

      elsif Command = Log_Debug_Method then
         Log.Debug (Message);

      else
         raise Python_Error with "Unknown method GNAThub.Log." & Command;
      end if;
   end Log_Class_Handler;

   --------------------------
   -- Log_Progress_Handler --
   --------------------------

   procedure Log_Progress_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Current  : constant Natural  := Data.Nth_Arg (1);
      Total    : constant Positive := Data.Nth_Arg (2);
      New_Line : constant Boolean  := Data.Nth_Arg (3, Default => False);

   begin
      pragma Assert (Command = Log_Progress_Method);
      Log.Progress (Current, Total, New_Line);
   end Log_Progress_Handler;

   -------------------------------------
   -- Project_Class_Accessors_Handler --
   -------------------------------------

   procedure Project_Class_Accessors_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Project_Name_Method then
         Set_Return_Value (Data, GNAThub.Project.Name);

      elsif Command = Project_Path_Method then
         Set_Return_Value (Data, GNAThub.Project.Path.Display_Full_Name);

      elsif Command = Project_Object_Dir_Method then
         Set_Return_Value (Data, GNAThub.Project.Object_Dir.Display_Full_Name);

      elsif Command = Project_Source_File_Method then
         declare
            Name : constant String := Data.Nth_Arg (1);
         begin
            Set_Return_Value
              (Data, GNAThub.Project.File (Name).Display_Full_Name);
         end;

      else
         raise Python_Error with "Unknown method GNAThub.Project." & Command;
      end if;
   end Project_Class_Accessors_Handler;

   --------------------------------------
   -- Project_Class_Properties_Handler --
   --------------------------------------

   procedure Project_Class_Properties_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Property : constant String := Data.Nth_Arg (1);
      Value    : constant String :=
                   GNAThub.Project.Property_As_String (Property);
      List     : String_List_Access :=
                   GNAThub.Project.Property_As_List (Property);

   begin
      if Command = Project_Property_As_String_Method then
         if Value = "" then
            declare
               Result : Unbounded_String;
            begin
               if List /= null then
                  for L in List'Range loop
                     Append (Result, List (L).all);

                     if L /= List'Last then
                        Append (Result, " ");
                     end if;
                  end loop;

                  Free (List);
               end if;

               Set_Return_Value (Data, To_String (Result));
            end;
         else
            Set_Return_Value (Data, Value);
         end if;

      elsif Command = Project_Property_As_List_Method then
         Set_Return_Value_As_List (Data);

         if List = null and then Value /= "" then
            Set_Return_Value (Data, Value);

         elsif List /= null then
            for L in List'Range loop
               Set_Return_Value (Data, List (L).all);
            end loop;
         end if;

         Free (List);

      else
         raise Python_Error with "Unknown method GNAThub.Project." & Command;
      end if;
   end Project_Class_Properties_Handler;

   -------------------------
   -- Root_Module_Handler --
   -------------------------

   procedure Root_Module_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Extra_Plugins_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Extra_Plugins_Dir.Display_Full_Name);

      elsif Command = Core_Plugins_Function then
         Set_Return_Value
           (Data, GNAThub.Constants.Core_Plugins_Dir.Display_Full_Name);

      elsif Command = Root_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (GNAThub.Project.Object_Dir,
               GNAThub.Constants.Root_Dir.Full_Name).Display_Full_Name);

      elsif Command = Database_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (GNAThub.Project.Object_Dir,
               GNAThub.Constants.Database_File.Full_Name).Display_Full_Name);

      elsif Command = Logs_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (GNAThub.Project.Object_Dir,
               GNAThub.Constants.Logs_Dir.Full_Name).Display_Full_Name);

      elsif Command = Plugins_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Plugins);

      else
         raise Python_Error with "Unknown method GNAThub." & Command;
      end if;
   end Root_Module_Handler;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File (Script_Filename : String; Errors : out Boolean) is
   begin
      Python.Execute_File
        (Filename     => Script_Filename,
         Show_Command => False,
         Errors       => Errors);
   end Execute_File;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : String; Errors : out Boolean)
   is
   begin
      Python.Execute_Command
        (Cmd,
         Show_Command => False,
         Hide_Output  => True,
         Errors       => Errors);
   end Execute;

end GNAThub.Python;
