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
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                   use GNAT.OS_Lib;
with GNAT.Source_Info;

with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.SQL;                  use GNATCOLL.SQL;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;       use GNATCOLL.Scripts.Python;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GNAThub.Constants;
with GNAThub.Configuration;         use GNAThub.Configuration;
with GNAThub.Project;
with GNAThub.Python.Database;       use GNAThub.Python.Database;

package body GNAThub.Python is
   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);

   Repository : Scripts_Repository := null;
   Python     : Python_Scripting   := null;

   Logger_Log_Method : constant String := "log";

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
   Project_Target_Method             : constant String := "target";
   Project_Runtime_Method            : constant String := "runtime";
   Project_Object_Dir_Method         : constant String := "object_dir";
   Project_Object_Dirs_Method        : constant String := "object_dirs";
   Project_Source_Dirs_Method        : constant String := "source_dirs";
   Project_Source_File_Method        : constant String := "source_file";
   Project_Source_Files_Method       : constant String := "source_files";
   Project_Source_Suffixes_Method    : constant String := "source_suffixes";
   Project_Property_As_String_Method : constant String := "property_as_string";
   Project_Property_As_List_Method   : constant String := "property_as_list";
   Scenario_Switches_Method          : constant String := "scenario_switches";

   Root_Function              : aliased constant String := "root";
   Logs_Function              : aliased constant String := "logs";
   HTML_Data_Function         : aliased constant String := "html_data";
   Jobs_Function              : aliased constant String := "jobs";
   Dry_Run_Function           : aliased constant String := "dry_run";
   Quiet_Function             : aliased constant String := "quiet";
   Verbose_Function           : aliased constant String := "verbose";
   Plugins_Function           : aliased constant String := "plugins";
   Subdirs_Function           : aliased constant String := "subdirs";
   Incremental_Function       : aliased constant String := "incremental";

   --  Keeping this for later implemntation of -U main switch
   --     U_Main_Function            : aliased constant String := "u_main";
   U_Process_All_Function     : aliased constant String := "u_process_all";
   Database_Function          : aliased constant String := "database";
   Repositories_Function      : aliased constant String := "repositories";
   Engine_Repository_Function : aliased constant String := "engine_repository";
   Runners_Only_Function      : aliased constant String := "runners_only";
   Reporters_Only_Function    : aliased constant String := "reporters_only";
   Tool_Args_Function         : constant String         := "tool_args";
   Server_Port_Function       : aliased constant String := "port";

   --  Codepeer WebUI specific repositories
   Object_Codepeer_Dir_Function : aliased constant String :=
     "obj_codepeer_dir";
   Codepeer_Output_Dir_Function : aliased constant String := "output_dir";
   Codepeer_DB_Dir_Function     : aliased constant String := "db_dir";

   --  Added to handle --dry-run mode without project file
   Dry_Run_Without_Project_Function : aliased constant String :=
     "dry_run_without_project";

   No_Args_Root_Module_Functions :
     constant array (1 .. 21) of access constant String :=
       (Root_Function'Access,
        Logs_Function'Access,
        HTML_Data_Function'Access,
        Jobs_Function'Access,
        Dry_Run_Function'Access,
        Quiet_Function'Access,
        Verbose_Function'Access,
        Plugins_Function'Access,
        Subdirs_Function'Access,
        Incremental_Function'Access,
        -- U_Main_Function'Access,
        U_Process_All_Function'Access,
        Database_Function'Access,
        Repositories_Function'Access,
        Engine_Repository_Function'Access,
        Runners_Only_Function'Access,
        Reporters_Only_Function'Access,
        Server_Port_Function'Access,
        Object_Codepeer_Dir_Function'Access,
        Codepeer_Output_Dir_Function'Access,
        Codepeer_DB_Dir_Function'Access,
        Dry_Run_Without_Project_Function'Access);

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

   procedure Project_Class_Source_Suffixes_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  GNAThub.Project.source_suffixes class method handler

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

      Repository.Register_Command
         (Command => Logger_Log_Method,
          Params  => (1 .. 1 => Param ("message")),
          Handler => Logger_Instance_Method_Handler'Access,
          Class   => Logger_Class);
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
        (Command       => Project_Target_Method,
         Params        => No_Params,
         Handler       => Project_Class_Accessors_Handler'Access,
         Class         => Project_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => Project_Runtime_Method,
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
        (Command       => Project_Source_Files_Method,
         Params        => No_Params,
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
        (Command       => Project_Object_Dirs_Method,
         Params        => No_Params,
         Handler       => Project_Class_Accessors_Handler'Access,
         Class         => Project_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => Project_Source_Suffixes_Method,
         Params        => (1 .. 1 => Param ("language")),
         Handler       => Project_Class_Source_Suffixes_Handler'Access,
         Class         => Project_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => Project_Property_As_String_Method,
         Params        =>
           (Param ("property"),
            Param ("package", Optional => True)),
         Handler       => Project_Class_Properties_Handler'Access,
         Class         => Project_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => Project_Property_As_List_Method,
         Params        =>
           (Param ("property"),
            Param ("package", Optional => True)),
         Handler       => Project_Class_Properties_Handler'Access,
         Class         => Project_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => Scenario_Switches_Method,
         Handler       => Project_Class_Accessors_Handler'Access,
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

      Trace (Me, "  + GNAThub");

      for Command of No_Args_Root_Module_Functions loop
         Repository.Register_Command
           (Command => Command.all,
            Params  => No_Params,
            Handler => Root_Module_Handler'Access);
      end loop;

      Repository.Register_Command
        (Command => Tool_Args_Function,
         Params  => (1 .. 1 => Param ("tool_name")),
         Handler => Root_Module_Handler'Access);

      --  Classes

      Trace (Me, "  + GNAThub.Console");
      Register_Console_Class (Console_Class);

      Trace (Me, "  + GNAThub.Logger");
      Register_Logger_Class (Logger_Class);

      Trace (Me, "  + GNAThub.Project");
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
         Trace (Me, "Python VM already initialized");
         return;
      end if;

      Trace (Me, "Initialize Python VM: " & Python_Home.Display_Full_Name);

      Repository := new Scripts_Repository_Record;

      Register_Python_Scripting
        (Repo        => Repository,
         Module      => Python_Root_Module,
         Python_Home => Python_Home.Display_Full_Name);

      Python := GNATCOLL.Scripts.Python.Python_Scripting
                  (GNATCOLL.Scripts.Lookup_Scripting_Language
                    (Repository, Python_Name));

      Trace (Me, "Register Python modules");
      Register_Core_Modules;

      Trace (Me, "  + GNAThub.Database_API");
      Register_Database_Interaction_API (Repository);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Unregister_Python_Scripting (Repository);
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

      elsif Command = Logger_Log_Method then
         Trace (Get_Handle (Inst), Data.Nth_Arg (2));

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

      elsif Command = Project_Target_Method then
         Set_Return_Value (Data, GNAThub.Project.Target);

      elsif Command = Project_Runtime_Method then
         Set_Return_Value (Data, GNAThub.Project.Runtime);

      elsif Command = Project_Object_Dir_Method then
         Set_Return_Value (Data, Object_Dir.Display_Full_Name);

      elsif Command = Project_Source_File_Method then
         declare
            Name : constant String := Data.Nth_Arg (1);
         begin
            Set_Return_Value (Data, File (Name).Display_Full_Name);
         end;

      elsif Command = Project_Source_Files_Method then
         for Project of GNAThub.Project.All_Projects loop
            Set_Return_Value_As_List (Data);

            for File of File_Array_Access'
              (Project.Source_Files (Recursive => False)).all
            loop
               Set_Return_Value (Data, File.Display_Full_Name);
            end loop;

            Set_Return_Value_Key (Data, Project.Name);
         end loop;

      elsif Command = Project_Source_Dirs_Method then
         for Project of GNAThub.Project.All_Projects loop
            Set_Return_Value_As_List (Data);

            for Dir of Project.Source_Dirs (Recursive => False) loop
               Set_Return_Value (Data, Dir.Display_Full_Name);
            end loop;

            Set_Return_Value_Key (Data, Project.Name);
         end loop;

      elsif Command = Project_Object_Dirs_Method then
         Set_Return_Value_As_List (Data);
         for Project of GNAThub.Project.All_Projects loop
            if Project.Has_Attribute (GNATCOLL.Projects.Obj_Dir_Attribute) then
               Set_Return_Value (Data, Project.Object_Dir.Display_Full_Name);
            end if;
         end loop;

      elsif Command = Scenario_Switches_Method then
         Set_Return_Value_As_List (Data);

         for Var of Get_Scenario_Variables loop
            if Name /= "" then
               Set_Return_Value
                 (Data,
                  "-X" & To_String (Var.Key) & "=" & To_String (Var.Value));
            end if;
         end loop;

      else
         raise Python_Error with "Unknown method GNAThub.Project." & Command;
      end if;
   end Project_Class_Accessors_Handler;

   -------------------------------------------
   -- Project_Class_Source_Suffixes_Handler --
   -------------------------------------------

   procedure Project_Class_Source_Suffixes_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use GNAThub.Project;

      Language : constant String := Data.Nth_Arg (1);

      Spec_Ext : constant String :=
        Property_As_String (Property     => "Spec_Suffix",
                            Package_Name => Naming_Package,
                            Index        => Language);

      Body_Ext : constant String :=
        Property_As_String (Property     => "Body_Suffix",
                            Package_Name => Naming_Package,
                            Index        => Language);

   begin
      pragma Assert (Command = Project_Source_Suffixes_Method);

      Set_Return_Value_As_List (Data);
      Set_Return_Value (Data, Spec_Ext);
      Set_Return_Value (Data, Body_Ext);
   end Project_Class_Source_Suffixes_Handler;

   --------------------------------------
   -- Project_Class_Properties_Handler --
   --------------------------------------

   procedure Project_Class_Properties_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use GNAThub.Project;

      Property     : constant String    := Data.Nth_Arg (1);
      Package_Name : constant String    :=
                       Data.Nth_Arg (2, GNATdashboard_Package);
      Value        : constant String    :=
                       Property_As_String (Property, Package_Name);
      List         : String_List_Access :=
                       Property_As_List (Property, Package_Name);

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

      elsif Command = HTML_Data_Function then
         Set_Return_Value
           (Data,
            Create_From_Dir
              (Object_Dir, HTML_Data_Dir.Full_Name).Display_Full_Name);

      elsif Command = Jobs_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Jobs);

      elsif Command = Dry_Run_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Dry_Run);

      elsif Command = Dry_Run_Without_Project_Function then
         Set_Return_Value (Data,
                           GNAThub.Configuration.Dry_Run_Without_Project);

      elsif Command = Quiet_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Quiet);

      elsif Command = Verbose_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Verbose);

      elsif Command = Plugins_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Plugins);

      elsif Command = Subdirs_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Subdirs);

      elsif Command = Incremental_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Incremental);

      --  Keeping this for later implementation of -U main switch
      --        elsif Command = U_Main_Function then
      --           Set_Return_Value (Data, GNAThub.Configuration.U_Main);

      elsif Command = U_Process_All_Function then
         Set_Return_Value (Data, GNAThub.Configuration.U_Process_All);

      elsif Command = Repositories_Function then
         --  Create a dictionary containing 3 keys: system, global, local
         --  Each key is associated with the path to the corresponding
         --  repository.

         Set_Return_Value (Data, Core_Plugins_Dir.Display_Full_Name);
         Set_Return_Value_Key (Data, "system");

         Set_Return_Value (Data, Extra_Plugins_Dir.Display_Full_Name);
         Set_Return_Value_Key (Data, "global");

         if Configuration.Project /= "" then
            --  This condition is added to be able to handle gnathub command
            --  line --dry-run switch without project file associated
            Set_Return_Value (Data, Property_As_String ("Local_Repository"));
            Set_Return_Value_Key (Data, "local");
         end if;

      elsif Command = Engine_Repository_Function then
         Set_Return_Value (Data, Server_Engine_Dir.Display_Full_Name);

      elsif Command = Server_Port_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Port);

      elsif Command = Object_Codepeer_Dir_Function then
         Set_Return_Value (Data, Obj_Codepeer_Dir.Display_Full_Name);

      elsif Command = Codepeer_Output_Dir_Function then
         Set_Return_Value (Data, Codepeer_Output_Dir.Display_Full_Name);

      elsif Command = Codepeer_DB_Dir_Function then
         Set_Return_Value (Data, Codepeer_DB_Dir.Display_Full_Name);

      elsif Command = Tool_Args_Function then
         --  Return a list of arguments

         Set_Return_Value_As_List (Data);

         declare
            Tool_Name : constant String := Data.Nth_Arg (1);
         begin
            for Argument of GNAThub.Configuration.Tool_Args (Tool_Name) loop
               Set_Return_Value (Data, Argument);
            end loop;
         end;

      elsif Command = Runners_Only_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Runners_Only);

      elsif Command = Reporters_Only_Function then
         Set_Return_Value (Data, GNAThub.Configuration.Reporters_Only);

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
