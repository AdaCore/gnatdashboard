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
with GNAT.Source_Info;

with GNATCOLL.SQL;                  use GNATCOLL.SQL;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;       use GNATCOLL.Scripts.Python;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GNAThub.Constants;
with GNAThub.Configuration;         use GNAThub.Configuration;
with GNAThub.Project;
with GNAThub.Python.Database;       use GNAThub.Python.Database;

package body GNAThub.Python is
   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);

   Repository : Scripts_Repository := null;
   Python     : Python_Scripting   := null;

   Logger_Info_Method      : aliased constant String := "info";
   Logger_Warn_Method      : aliased constant String := "warn";
   Logger_Error_Method     : aliased constant String := "error";
   Logger_Fatal_Method     : aliased constant String := "fatal";
   Logger_Debug_Method     : aliased constant String := "debug";
   --  Logger_Exception_Method : aliased String := "exception";

   Logger_Class_Instance_Methods :
     constant array (1 .. 5) of access constant String :=
       (Logger_Info_Method'Access,
        Logger_Warn_Method'Access,
        Logger_Error_Method'Access,
        Logger_Fatal_Method'Access,
        Logger_Debug_Method'Access);

   Console_Info_Method     : aliased constant String := "info";
   Console_Warn_Method     : aliased constant String := "warn";
   Console_Error_Method    : aliased constant String := "error";
   Console_Progress_Method : aliased constant String := "progress";

   Console_Class_Instance_Methods :
     constant array (1 .. 3) of access constant String :=
       (Console_Info_Method'Access,
        Console_Warn_Method'Access,
        Console_Error_Method'Access);

   Project_Name_Method               : constant String := "name";
   Project_Path_Method               : constant String := "path";
   Project_Object_Dir_Method         : constant String := "object_dir";
   Project_Source_File_Method        : constant String := "source_file";
   Project_Source_Dirs_Method        : constant String := "source_dirs";
   Project_Property_As_String_Method : constant String := "property_as_string";
   Project_Property_As_List_Method   : constant String := "property_as_list";

   Root_Function          : aliased constant String := "root";
   Logs_Function          : aliased constant String := "logs";
   Jobs_Function          : aliased constant String := "jobs";
   Plugins_Function       : aliased constant String := "plugins";
   Database_Function      : aliased constant String := "database";
   Repositories_Function  : aliased constant String := "repositories";

   Root_Module_Functions :
     constant array (1 .. 6) of access constant String :=
       (Root_Function'Access,
        Logs_Function'Access,
        Jobs_Function'Access,
        Plugins_Function'Access,
        Database_Function'Access,
        Repositories_Function'Access);

   --------------------
   -- GNAThub Module --
   --------------------

   procedure Register_Core_Modules;
   --  Registers the core modules and functions of GNAThub Python module.

   procedure Root_Module_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Env class methods handler

   ---------------------------
   -- GNAThub.Console Class --
   ---------------------------

   procedure Console_Class_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Console class methods handler

   procedure Console_Progress_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Console.progress class method handler

   procedure Register_Console_Class (Console_Class : Class_Type);
   --  Load the GNAThub.Console class in the Python VM

   --------------------------
   -- GNAThub.Logger Class --
   --------------------------

   type Logger_Property is new Instance_Property_Record with record
      Handle : Trace_Handle;
   end record;

   procedure Set_Handle (Inst : Class_Instance; Name : String);
   --  Set the Trace_Handle held by this GNAThub.Logger instance

   function Get_Handle (Inst : Class_Instance) return Trace_Handle;
   --  Return the Trace_Handle held by this GNAThub.Logger instance

   procedure Logger_Instance_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Logger.* instance methods handler

   procedure Register_Logger_Class (Logger_Class : Class_Type);
   --  Load the GNAThub.Logger class in the Python VM

   ---------------------------
   -- GNAThub.Project Class --
   ---------------------------

   procedure Project_Class_Accessors_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Project.* class method handler

   procedure Project_Class_Properties_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Project.property_as_* class method handler

   procedure Register_Project_Class (Project_Class : Class_Type);
   --  Load the GNAThub.Project class in the Python VM

   ----------------------------
   -- Register_Console_Class --
   ----------------------------

   procedure Register_Console_Class (Console_Class : Class_Type) is
   begin
      for Command of Console_Class_Instance_Methods loop
         Repository.Register_Command
           (Command       => Command.all,
            Params        =>
              (Param ("message"),
               Param ("prefix", Optional => True)),
            Handler       => Console_Class_Method_Handler'Access,
            Class         => Console_Class,
            Static_Method => True);
      end loop;

      Repository.Register_Command
        (Command       => Console_Progress_Method,
         Params        =>
           (Param ("current"),
            Param ("total"),
            Param ("new_line", Optional => True)),
         Handler       => Console_Progress_Method_Handler'Access,
         Class         => Console_Class,
         Static_Method => True);
   end Register_Console_Class;

   ---------------------------
   -- Register_Logger_Class --
   ---------------------------

   procedure Register_Logger_Class (Logger_Class : Class_Type) is
   begin
      Repository.Register_Command
        (Command => Constructor_Method,
         Params  => (1 .. 1 => Param ("name")),
         Handler => Logger_Instance_Method_Handler'Access,
         Class   => Logger_Class);

      for Command of Logger_Class_Instance_Methods loop
         Repository.Register_Command
           (Command       => Command.all,
            Params        => (1 .. 1 => Param ("message")),
            Handler       => Logger_Instance_Method_Handler'Access,
            Class         => Logger_Class);
      end loop;
   end Register_Logger_Class;

   ----------------------------
   -- Register_Project_Class --
   ----------------------------

   procedure Register_Project_Class (Project_Class : Class_Type) is
   begin
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
        (Command       => Project_Source_Dirs_Method,
         Params        => No_Params,
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
   end Register_Project_Class;

   ---------------------------
   -- Register_Core_Modules --
   ---------------------------

   procedure Register_Core_Modules
   is
      Console_Class : constant Class_Type := Repository.New_Class ("Console");
      Logger_Class  : constant Class_Type := Repository.New_Class ("Logger");
      Project_Class : constant Class_Type := Repository.New_Class ("Project");

   begin
      --  Modules

      Log.Debug (Me, "  + GNAThub");

      for Command of Root_Module_Functions loop
         Repository.Register_Command
           (Command => Command.all,
            Params  => No_Params,
            Handler => Root_Module_Handler'Access);
      end loop;

      --  Classes

      Log.Debug (Me, "  + GNAThub.Console");
      Register_Console_Class (Console_Class);

      Log.Debug (Me, "  + GNAThub.Logger");
      Register_Logger_Class (Logger_Class);

      Log.Debug (Me, "  + GNAThub.Project");
      Register_Project_Class (Project_Class);
   end Register_Core_Modules;

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
      use GNAThub.Constants;
   begin
      if Repository /= null then
         Log.Warn (Me, "Python VM already initialized");
         return;
      end if;

      Log.Info (Me, "Initialize Python VM: " & Python_Home.Display_Full_Name);

      Repository := new Scripts_Repository_Record;

      Register_Python_Scripting
        (Repo        => Repository,
         Module      => Python_Root_Module,
         Python_Home => Python_Home.Display_Full_Name);

      Python := GNATCOLL.Scripts.Python.Python_Scripting
                  (GNATCOLL.Scripts.Lookup_Scripting_Language
                    (Repository, Python_Name));

      Log.Info (Me, "Register Python modules");
      Register_Core_Modules;

      Log.Debug (Me, "  + GNAThub.Database_API");
      Register_Database_Interaction_API (Repository);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   ----------------------------------
   -- Console_Class_Method_Handler --
   ----------------------------------

   procedure Console_Class_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Message : constant String := Data.Nth_Arg (1);
      Prefix  : constant String := Data.Nth_Arg (2, "");
   begin
      if Command = Console_Info_Method then
         Info (Message, Prefix => Prefix);

      elsif Command = Console_Warn_Method then
         Warn (Message, Prefix => Prefix);

      elsif Command = Console_Error_Method then
         Error (Message, Prefix => Prefix);

      else
         raise Python_Error with "Unknown method GNAThub.Console." & Command;
      end if;
   end Console_Class_Method_Handler;

   -------------------------------------
   -- Console_Progress_Method_Handler --
   -------------------------------------

   procedure Console_Progress_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Current  : constant Natural  := Data.Nth_Arg (1);
      Total    : constant Positive := Data.Nth_Arg (2);
      New_Line : constant Boolean  := Data.Nth_Arg (3, Default => False);

   begin
      pragma Assert (Command = Console_Progress_Method);
      Progress (Current, Total, New_Line);
   end Console_Progress_Method_Handler;

   ----------------
   -- Set_Handle --
   ----------------

   procedure Set_Handle (Inst : Class_Instance; Name : String) is
   begin
      GNATCOLL.Scripts.Set_Data
        (Inst, "_ada_logger",
         Logger_Property'(Handle => Create ("GNATHUB.PLUGIN." & Name)));
   end Set_Handle;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle (Inst : Class_Instance) return Trace_Handle is
   begin
      return Logger_Property (Get_Data (Inst, "_ada_logger").all).Handle;
   end Get_Handle;

   ------------------------------------
   -- Logger_Instance_Method_Handler --
   ------------------------------------

   procedure Logger_Instance_Method_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Inst : constant Class_Instance := Data.Nth_Arg (1);
   begin
      if Command = Constructor_Method then
         Set_Handle (Inst, Name => Data.Nth_Arg (2));

      elsif Command = Logger_Info_Method then
         Log.Info (Get_Handle (Inst), Data.Nth_Arg (2));

      elsif Command = Logger_Warn_Method then
         Log.Warn (Get_Handle (Inst), Data.Nth_Arg (2));

      elsif Command = Logger_Error_Method then
         Log.Error (Get_Handle (Inst), Data.Nth_Arg (2));

      elsif Command = Logger_Fatal_Method then
         Log.Fatal (Get_Handle (Inst), Data.Nth_Arg (2));

      elsif Command = Logger_Debug_Method then
         Log.Debug (Get_Handle (Inst), Data.Nth_Arg (2));

      --  elsif Command = Logger_Exception_Method then
      --     null;

      else
         raise Python_Error with "Unknown method GNAThub.Logger." & Command;
      end if;
   end Logger_Instance_Method_Handler;

   -------------------------------------
   -- Project_Class_Accessors_Handler --
   -------------------------------------

   procedure Project_Class_Accessors_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use GNAThub.Project;
   begin
      if Command = Project_Name_Method then
         Set_Return_Value (Data, Name);

      elsif Command = Project_Path_Method then
         Set_Return_Value (Data, Path.Display_Full_Name);

      elsif Command = Project_Object_Dir_Method then
         Set_Return_Value (Data, Object_Dir.Display_Full_Name);

      elsif Command = Project_Source_File_Method then
         declare
            Name : constant String := Data.Nth_Arg (1);
         begin
            Set_Return_Value (Data, File (Name).Display_Full_Name);
         end;

      elsif Command = Project_Source_Dirs_Method then
         for Project of GNAThub.Project.All_Projects loop
            Set_Return_Value_As_List (Data);

            for Dir of Project.Source_Dirs (Recursive => False) loop
               Set_Return_Value (Data, Dir.Display_Full_Name);
            end loop;

            Set_Return_Value_Key (Data, Project.Name);
         end loop;

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
      use GNAThub.Project;

      Property : constant String    := Data.Nth_Arg (1);
      Value    : constant String    := Property_As_String (Property);
      List     : String_List_Access := Property_As_List (Property);

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
      Command : String)
   is
      use GNAThub.Constants;
      use GNAThub.Project;
   begin
      if Command = Root_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (Object_Dir, Root_Dir.Full_Name).Display_Full_Name);

      elsif Command = Database_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (Object_Dir, Database_File.Full_Name).Display_Full_Name);

      elsif Command = Logs_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (Object_Dir, Logs_Dir.Full_Name).Display_Full_Name);

      elsif Command = Jobs_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Jobs);

      elsif Command = Plugins_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Plugins);

      elsif Command = Repositories_Function then
         --  Create a dictionary containing 3 keys: system, global, local
         --  Each key is associated with the path to the corresponding
         --  repository.

         Set_Return_Value (Data, Core_Plugins_Dir.Display_Full_Name);
         Set_Return_Value_Key (Data, "system");

         Set_Return_Value (Data, Extra_Plugins_Dir.Display_Full_Name);
         Set_Return_Value_Key (Data, "global");

         Set_Return_Value (Data, Property_As_String ("Local_Repository"));
         Set_Return_Value_Key (Data, "local");

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
