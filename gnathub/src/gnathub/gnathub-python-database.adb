------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2014-2016, AdaCore                     --
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

with Ada.Strings.Unbounded;

with GNATCOLL.SQL;                  use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;             use GNATCOLL.SQL.Exec;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;

with GNAThub.Database;

with Database;                      use Database;

package body GNAThub.Python.Database is

   --  Convenience renamings

   function DB return GNATCOLL.SQL.Exec.Database_Connection
     renames GNAThub.Database.DB;
   package D renames Standard.Database;

   Me : constant Trace_Handle := Create ("GNATHUB.PYTHON.DATABASE");
   --  The handle to use to log messages

   -------------------------
   -- Prepared statements --
   -------------------------

   Insert_Message : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
          (((D.Resources_Messages.Message_Id   = Integer_Param (1))
           & (D.Resources_Messages.Resource_Id = Integer_Param (2))
           & (D.Resources_Messages.Line        = Integer_Param (3))
           & (D.Resources_Messages.Col_Begin   = Integer_Param (4))
           & (D.Resources_Messages.Col_End     = Integer_Param (5)))),
       On_Server => True);

   -----------
   -- Tools --
   -----------

   Tool_Class_Name : constant String := "Tool";
   Tool_Class      : Class_Type;

   type Tool_Property_Record is new Instance_Property_Record with record
      Id : Integer;
   end record;
   type Tool_Property is access all Tool_Property_Record'Class;

   procedure Tool_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Set_Tool_Fields
     (Tool_Inst : Class_Instance;
      Id        : Integer;
      Name      : String);
   --  Set the fields describing a Tool in Tool_Inst

   ----------------
   -- Categories --
   ----------------

   Category_Class_Name : constant String := "Category";
   Category_Class      : Class_Type;

   type Category_Property_Record is new Instance_Property_Record with record
      Id : Integer;
   end record;
   type Category_Property is access all Category_Property_Record'Class;

   procedure Category_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Set_Category_Fields
     (Category_Inst : Class_Instance;
      Id            : Integer;
      Label         : String;
      On_Side       : Boolean);
   --  Set the fields describing a Category in Category_Inst

   -----------
   -- Rules --
   -----------

   Rule_Class_Name : constant String := "Rule";
   Rule_Class      : Class_Type;

   type Rule_Property_Record is new Instance_Property_Record with record
      Id : Integer;
   end record;
   type Rule_Property is access all Rule_Property_Record'Class;

   procedure Rule_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Set_Rule_Fields
     (Rule_Inst  : Class_Instance;
      Id         : Integer;
      Name       : String;
      Identifier : String;
      Kind       : Integer;
      Tool_Id    : Integer);
   --  Set the fields describing a Rule in Rule_Inst

   --------------
   -- Messages --
   --------------

   Message_Class_Name : constant String := "Message";
   Message_Class      : Class_Type;

   type Message_Property_Record is new Instance_Property_Record with record
      Id : Integer;
   end record;
   type Message_Property is access all Message_Property_Record'Class;

   procedure Message_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Set_Message_Fields
     (Message_Inst : Class_Instance;
      Id           : Integer;
      Rule_Id      : Integer;
      Message_Data : String;
      Category_Id  : Integer;
      Line         : Integer := -1;
      Col_Begin    : Integer := -1;
      Col_End      : Integer := -1);
   --  Set the fields describing a Message in Message_Inst

   ---------------
   -- Resources --
   ---------------

   Resource_Class_Name : constant String := "Resource";
   Resource_Class      : Class_Type;

   type Resource_Property_Record is new Instance_Property_Record with record
      Id : Integer;
   end record;
   type Resource_Property is access all Resource_Property_Record'Class;

   procedure Resource_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for Resource commands

   procedure Set_Resource_Fields
     (Resource_Inst : Class_Instance;
      Id            : Integer;
      Name          : String;
      Kind          : Integer);
   --  Set the fields describing a Resource in Resource_Inst

   -------------
   -- Helpers --
   -------------

   procedure Clear_Tool_References (Tool_Name : String);
   --  Remove references of Tool_Name in the database, ie. delete:
   --
   --  * messages where rule_id matches the ID of a rule created for Tool_Name
   --  * rules created for Tool_Name
   --  * the tool from the Tools table
   --
   --  Return immediately if Tool_Name does not exist.

   ---------------------
   -- Set_Tool_Fields --
   ---------------------

   procedure Set_Tool_Fields
     (Tool_Inst : Class_Instance;
      Id        : Integer;
      Name      : String) is
   begin
      Set_Data (Tool_Inst, Tool_Class_Name,
                Tool_Property_Record'(Id => Id));
      Set_Property (Tool_Inst, "id", Id);
      Set_Property (Tool_Inst, "name", Name);
   end Set_Tool_Fields;

   --------------------------
   -- Tool_Command_Handler --
   --------------------------

   procedure Tool_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Tool_Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            Name : constant String := Nth_Arg (Data, 2);
            Q    : SQL_Query;
            R    : Forward_Cursor;
            Id   : Integer;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Tools.Id)),
               From  => D.Tools,
               Where => D.Tools.Name = Name);
            Tool_Inst := Nth_Arg (Data, 1, Tool_Class);

            R.Fetch (DB, Q);
            if R.Has_Row then
               --  A tool has been found with this name: return it
               Id := R.Integer_Value (0);

            else
               --  No tool with that name has been found: create one
               Id := DB.Insert_And_Get_PK
                 (SQL_Insert ((D.Tools.Name = Name)),
                  PK => D.Tools.Id);
               DB.Commit;
            end if;

            Set_Tool_Fields (Tool_Inst, Id, Name);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         declare
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Tools.Id, 1 => +D.Tools.Name)),
               From => D.Tools);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Tool_Inst := New_Instance (Get_Script (Data), Tool_Class);
               Set_Tool_Fields (Tool_Inst, R.Integer_Value (0), R.Value (1));
               Set_Return_Value (Data, Tool_Inst);

               R.Next;
            end loop;
         end;

      elsif Command = "clear_references" then
         Clear_Tool_References (Nth_Arg (Data, 1));

      else
         raise Python_Error with "Unknown method GNAThub.Tool." & Command;
      end if;
   end Tool_Command_Handler;

   ---------------------------
   -- Clear_Tool_References --
   ---------------------------

   procedure Clear_Tool_References (Tool_Name : String)
   is
      use Ada.Strings.Unbounded;

      Results  : Direct_Cursor;
      Iterator : Forward_Cursor;
      Tool_Id  : Integer;
      Rule_Id  : Integer;
      Rule_Ids : Unbounded_String;
   begin
      Fetch
        (Results, DB, SQL_Select
          (To_List ((0 => +D.Tools.Id)),
           From  => D.Tools,
           Where => D.Tools.Name = Tool_Name));

      if not Results.Has_Row then
         --  No tool with that name has been found, nothing to do
         return;
      end if;

      Assert (Me, Results.Rows_Count = 1, "Tool_Id expected to be unique");
      Tool_Id := Results.Integer_Value (0);

      --  Iterate over the tool's rules
      Fetch
        (Iterator, DB, SQL_Select
           (To_List ((0 => +D.Rules.Id)),
            From  => D.Rules,
            Where => D.Rules.Tool_Id = Tool_Id));

      while Iterator.Has_Row loop
         Rule_Id := Iterator.Integer_Value (0);

         if Iterator.Processed_Rows = 0 then
            Rule_Ids := To_Unbounded_String (Rule_Id'Img);
         else
            Append (Rule_Ids, "," & Rule_Id'Img);
         end if;

         --  Delete message entries in join tables (resources_messages &
         --  entities_messages).
         declare
            Iterator   : Forward_Cursor;
            Message_Id : Integer;
            Ids        : Unbounded_String;
         begin
            Fetch
              (Iterator, DB, SQL_Select
                 (To_List ((0 => +D.Messages.Id)),
                  From  => D.Messages,
                  Where => D.Messages.Rule_Id = Rule_Id));

            while Iterator.Has_Row loop
               Message_Id := Iterator.Integer_Value (0);

               if Iterator.Processed_Rows = 0 then
                  Ids := To_Unbounded_String (Message_Id'Img);
               else
                  Append (Ids, "," & Message_Id'Img);
               end if;

               Iterator.Next;
            end loop;

            DB.Execute
              (SQL_Delete
                 (From  => D.Resources_Messages,
                  Where =>
                    SQL_In (D.Entities_Messages.Message_Id, To_String (Ids))));
         end;

         Iterator.Next;
      end loop;

      --  Delete the entries in the messages table
      DB.Execute
        (SQL_Delete
           (From  => D.Messages,
            Where => SQL_In (D.Messages.Rule_Id, To_String (Rule_Ids))));
      --  Delete the entrise in the rules table
      DB.Execute
        (SQL_Delete
           (From  => D.Rules,
            Where => SQL_In (D.Rules.Id, To_String (Rule_Ids))));
      --  Delete the entry in the tools table
      DB.Execute
        (SQL_Delete (From => D.Tools, Where => D.Tools.Name = Tool_Name));
      DB.Commit;
   end Clear_Tool_References;

   -------------------------
   -- Set_Category_Fields --
   -------------------------

   procedure Set_Category_Fields
     (Category_Inst : Class_Instance;
      Id            : Integer;
      Label         : String;
      On_Side       : Boolean) is
   begin
      Set_Data (Category_Inst, Category_Class_Name,
                Category_Property_Record'(Id => Id));
      Set_Property (Category_Inst, "id", Id);
      Set_Property (Category_Inst, "label", Label);
      Set_Property (Category_Inst, "on_side", On_Side);
   end Set_Category_Fields;

   ------------------------------
   -- Category_Command_Handler --
   ------------------------------

   procedure Category_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Category_Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            Label   : constant String := Nth_Arg (Data, 2);
            On_Side : constant Boolean := Nth_Arg (Data, 3, False);
            Q       : SQL_Query;
            R       : Forward_Cursor;
            Id      : Integer;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Categories.Id)),
               From  => D.Categories,
               Where => (D.Categories.Label = Label) and
                        (D.Categories.On_Side = On_Side));
            Category_Inst := Nth_Arg (Data, 1, Category_Class);

            R.Fetch (DB, Q);

            if R.Has_Row then
               --  A Category has been found with this name: return it
               Id := R.Integer_Value (0);

            else
               --  No Category with that name has been found: create one
               Id := DB.Insert_And_Get_PK
                 (SQL_Insert
                    (((D.Categories.Label = Label) &
                      (D.Categories.On_Side = On_Side))),
                  PK => D.Categories.Id);
               DB.Commit;
            end if;
            Set_Category_Fields (Category_Inst, Id, Label, On_Side);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);

         declare
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Categories.Id,
                         1 => +D.Categories.Label,
                         2 => +D.Categories.On_Side)),
               From  => D.Categories);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Category_Inst := New_Instance
                 (Get_Script (Data), Category_Class);
               Set_Category_Fields
                 (Category_Inst,
                  R.Integer_Value (0), R.Value (1), R.Boolean_Value (2));
               Set_Return_Value (Data, Category_Inst);

               R.Next;
            end loop;
         end;
      else
         raise Python_Error with "Unknown method GNAThub.Category." & Command;
      end if;
   end Category_Command_Handler;

   ---------------------
   -- Set_Rule_Fields --
   ---------------------

   procedure Set_Rule_Fields
     (Rule_Inst  : Class_Instance;
      Id         : Integer;
      Name       : String;
      Identifier : String;
      Kind       : Integer;
      Tool_Id    : Integer) is
   begin
      Set_Data (Rule_Inst, Rule_Class_Name,
                Rule_Property_Record'(Id => Id));
      Set_Property (Rule_Inst, "id", Id);
      Set_Property (Rule_Inst, "name", Name);
      Set_Property (Rule_Inst, "identifier", Identifier);
      Set_Property (Rule_Inst, "kind", Kind);
      Set_Property (Rule_Inst, "tool_id", Tool_Id);
   end Set_Rule_Fields;

   --------------------------
   -- Rule_Command_Handler --
   --------------------------

   procedure Rule_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Rule_Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            Name       : constant String := Nth_Arg (Data, 2);
            Identifier : constant String := Nth_Arg (Data, 3);
            Kind       : constant Integer := Nth_Arg (Data, 4);
            Tool       : constant Class_Instance := Nth_Arg (Data, 5);
            Tool_Id    : constant Integer := Tool_Property
              (Get_Data (Tool, Tool_Class_Name)).Id;

            Q          : SQL_Query;
            R          : Forward_Cursor;
            Id         : Integer;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Rules.Id)),
               From  => D.Rules,
               Where => (D.Rules.Name = Name) and
                   (D.Rules.Identifier = Identifier) and
                   (D.Rules.Kind = Kind) and
                   (D.Rules.Tool_Id = Tool_Id));
            Rule_Inst := Nth_Arg (Data, 1, Rule_Class);

            R.Fetch (DB, Q);

            if R.Has_Row then
               --  A Rule has been found with this name: return it
               Id := R.Integer_Value (0);

            else
               --  No Rule with that name has been found: create one
               Id := DB.Insert_And_Get_PK
                 (SQL_Insert (
                  (D.Rules.Name = Name) &
                  (D.Rules.Identifier = Identifier) &
                  (D.Rules.Kind = Kind) &
                  (D.Rules.Tool_Id = Tool_Id)),
                  PK => D.Rules.Id);
               DB.Commit;
            end if;

            Set_Rule_Fields (Rule_Inst, Id, Name, Identifier, Kind, Tool_Id);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);

         declare
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Rules.Id,
                         1 => +D.Rules.Name,
                         2 => +D.Rules.Identifier,
                         3 => +D.Rules.Kind,
                         4 => +D.Rules.Tool_Id)),
               From  => D.Rules);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Rule_Inst := New_Instance (Get_Script (Data), Rule_Class);
               Set_Rule_Fields
                 (Rule_Inst,
                  R.Integer_Value (0),
                  R.Value (1),
                  R.Value (2),
                  R.Integer_Value (3),
                  R.Integer_Value (4));
               Set_Return_Value (Data, Rule_Inst);

               R.Next;
            end loop;
         end;
      else
         raise Python_Error with "Unknown method GNAThub.Rule." & Command;
      end if;
   end Rule_Command_Handler;

   ------------------------
   -- Set_Message_Fields --
   ------------------------

   procedure Set_Message_Fields
     (Message_Inst : Class_Instance;
      Id           : Integer;
      Rule_Id      : Integer;
      Message_Data : String;
      Category_Id  : Integer;
      Line         : Integer := -1;
      Col_Begin    : Integer := -1;
      Col_End      : Integer := -1) is
   begin
      Set_Data (Message_Inst, Message_Class_Name,
                Message_Property_Record'(Id => Id));
      Set_Property (Message_Inst, "id", Id);
      Set_Property (Message_Inst, "rule_id", Rule_Id);
      Set_Property (Message_Inst, "data", Message_Data);

      if Category_Id /= -1 then
         Set_Property (Message_Inst, "category_id", Category_Id);
      else
         Set_Property (Message_Inst, "category_id", "");
      end if;

      if Line /= -1 then
         Set_Property (Message_Inst, "line", Line);
      end if;

      if Col_Begin /= -1 then
         Set_Property (Message_Inst, "col_begin", Col_Begin);
      end if;

      if Col_End /= -1 then
         Set_Property (Message_Inst, "col_end", Col_End);
      end if;
   end Set_Message_Fields;

   -----------------------------
   -- Message_Command_Handler --
   -----------------------------

   procedure Message_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Message_Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            Rule         : constant Class_Instance := Nth_Arg (Data, 2);
            Rule_Id      : constant Integer := Rule_Property
              (Get_Data (Rule, Rule_Class_Name)).Id;
            Message_Data : constant String := Nth_Arg (Data, 3);
            Category     : constant Class_Instance :=
              Nth_Arg (Data, 4, Default => No_Class_Instance);
            Category_Id  : Integer := -1;  --  -1 iff Category is not specified
            Q            : SQL_Query;
            R            : Forward_Cursor;
            Id           : Integer;
         begin

            if Category = No_Class_Instance then
               Q := SQL_Select
                 (To_List ((0 => +D.Messages.Id)),
                  From  => D.Messages,
                  Where => D.Messages.Rule_Id = Rule_Id
                  and D.Messages.Data = Message_Data
                  and D.Messages.Category_Id = Null_Field_Integer);
            else
               Category_Id := Category_Property
                 (Get_Data (Category, Category_Class_Name)).Id;
               Q := SQL_Select
                 (To_List ((0 => +D.Messages.Id)),
                  From  => D.Messages,
                  Where => D.Messages.Rule_Id = Rule_Id
                  and D.Messages.Data = Message_Data
                  and D.Messages.Category_Id = Category_Id);
            end if;

            Message_Inst := Nth_Arg (Data, 1, Message_Class);

            R.Fetch (DB, Q);

            if R.Has_Row then
               --  A Message has been found with this name: return it
               Id := R.Integer_Value (0);
            else
               --  No Message with that name has been found: create one

               if Category = No_Class_Instance then
                  Id := DB.Insert_And_Get_PK
                    (SQL_Insert (
                     ((D.Messages.Rule_Id = Rule_Id)
                     & (D.Messages.Data = Message_Data))),
                     PK => D.Messages.Id);
               else
                  Id := DB.Insert_And_Get_PK
                    (SQL_Insert (
                     ((D.Messages.Rule_Id = Rule_Id)
                     & (D.Messages.Data = Message_Data)
                     & (D.Messages.Category_Id = Category_Id))),
                     PK => D.Messages.Id);
               end if;

               DB.Commit;
            end if;

            Set_Message_Fields
              (Message_Inst, Id, Rule_Id, Message_Data, Category_Id);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         declare
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Messages.Id,
                         1 => +D.Messages.Rule_Id,
                         2 => +D.Messages.Data,
                         3 => +D.Messages.Category_ID)),
               From  => D.Messages);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Message_Inst := New_Instance (Get_Script (Data), Message_Class);
               Set_Message_Fields
                 (Message_Inst,
                  R.Integer_Value (0), R.Integer_Value (1),
                  R.Value (2), R.Integer_Value (3, -1));
               Set_Return_Value (Data, Message_Inst);

               R.Next;
            end loop;
         end;
      else
         raise Python_Error with "Unknown method GNAThub.Message." & Command;
      end if;
   end Message_Command_Handler;

   -------------------------
   -- Set_Resource_Fields --
   -------------------------

   procedure Set_Resource_Fields
     (Resource_Inst : Class_Instance;
      Id            : Integer;
      Name          : String;
      Kind          : Integer) is
   begin
      Set_Data (Resource_Inst, Resource_Class_Name,
                Resource_Property_Record'(Id => Id));
      Set_Property (Resource_Inst, "id", Id);
      Set_Property (Resource_Inst, "name", Name);
      Set_Property (Resource_Inst, "kind", Kind);
   end Set_Resource_Fields;

   -----------------------------
   -- Resource_Command_Handler --
   -----------------------------

   procedure Resource_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Resource_Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         declare
            Name : constant String := Nth_Arg (Data, 2);
            Kind : constant Integer := Nth_Arg (Data, 3);

            Q    : SQL_Query;
            R    : Forward_Cursor;
            Id   : Integer;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Resources.Id)),
               From  => D.Resources,
               Where => D.Resources.Name = Name
               and D.Resources.Kind = Kind);

            Resource_Inst := Nth_Arg (Data, 1, Resource_Class);

            R.Fetch (DB, Q);
            if R.Has_Row then
               --  A Resource has been found with this name: return it
               Id := R.Integer_Value (0);

            else
               --  No Resource with that name has been found: create one

               Id := DB.Insert_And_Get_PK
                 (SQL_Insert (
                  ((D.Resources.Name = Name)
                  & (D.Resources.Kind = Kind))),
                  PK => D.Resources.Id);

               DB.Commit;
            end if;

            Set_Resource_Fields (Resource_Inst, Id, Name, Kind);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         declare
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Resources.Id,
                         1 => +D.Resources.Name,
                         2 => +D.Resources.Kind)),
               From  => D.Resources);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Resource_Inst := New_Instance
                 (Get_Script (Data), Resource_Class);
               Set_Resource_Fields
                 (Resource_Inst,
                  R.Integer_Value (0), R.Value (1), R.Integer_Value (2));
               Set_Return_Value (Data, Resource_Inst);

               R.Next;
            end loop;
         end;

      elsif Command = "get" then
         declare
            Name : constant String := Nth_Arg (Data, 1);
            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            Q := SQL_Select
              (To_List ((0 => +D.Resources.Id,
                         1 => +D.Resources.Name,
                         2 => +D.Resources.Kind)),
               From  => D.Resources,
               Where => D.Resources.Name = Name);
            R.Fetch (DB, Q);

            while R.Has_Row loop
               Resource_Inst := New_Instance
                 (Get_Script (Data), Resource_Class);
               Set_Resource_Fields
                 (Resource_Inst,
                  R.Integer_Value (0), R.Value (1), R.Integer_Value (2));
               Set_Return_Value (Data, Resource_Inst);

               R.Next;
            end loop;
         end;

      elsif Command = "add_messages" then
         declare
            --  Required parameters
            Resource    : constant Class_Instance := Nth_Arg (Data, 1);
            Resource_Id : constant Integer := Resource_Property
              (Get_Data (Resource, Resource_Class_Name)).Id;
            Global_List   : constant List_Instance := Nth_Arg (Data, 2);
         begin
            Database.DB.Automatic_Transactions (False);
            Database.DB.Execute ("BEGIN");

            for J in 1 .. Global_List.Number_Of_Arguments loop
               declare
                  List : constant List_Instance := Nth_Arg (Global_List, J);

                  Message      : constant Class_Instance := List.Nth_Arg (1);
                  Message_Id   : constant Integer := Message_Property
                    (Get_Data (Message, Message_Class_Name)).Id;
                  Line         : constant Integer := List.Nth_Arg (2);
                  Col_Begin    : constant Integer := List.Nth_Arg (3);
                  Col_End      : constant Integer := List.Nth_Arg (4);
               begin
                  Database.DB.Execute
                    (Stmt   => Insert_Message,
                     Params => (1 => +Message_Id,
                                2 => +Resource_Id,
                                3 => +Line,
                                4 => +Col_Begin,
                                5 => +Col_End));
               end;
            end loop;

            Database.DB.Commit_Or_Rollback;
            Database.DB.Automatic_Transactions (True);
         end;

      elsif Command = "add_message" then
         declare
            --  Required parameters
            Resource    : constant Class_Instance := Nth_Arg (Data, 1);
            Resource_Id : constant Integer := Resource_Property
              (Get_Data (Resource, Resource_Class_Name)).Id;
            Message     : constant Class_Instance := Nth_Arg (Data, 2);
            Message_Id  : constant Integer := Message_Property
              (Get_Data (Message, Message_Class_Name)).Id;

            --  Optional parameters
            Line        : constant Integer := Nth_Arg (Data, 3, 0);
            Col_Begin   : constant Integer := Nth_Arg (Data, 4, 1);
            Col_End     : constant Integer := Nth_Arg (Data, 5, Col_Begin);
         begin
            Database.DB.Execute
              (Stmt   => Insert_Message,
               Params => (1 => +Message_Id,
                          2 => +Resource_Id,
                          3 => +Line,
                          4 => +Col_Begin,
                          5 => +Col_End));
            DB.Commit;
         end;

      elsif Command = "list_messages" then
         Set_Return_Value_As_List (Data);
         declare
            Resource    : constant Class_Instance := Nth_Arg (Data, 1);
            Resource_Id : constant Integer := Resource_Property
              (Get_Data (Resource, Resource_Class_Name)).Id;

            Message_Inst : Class_Instance;

            Q : SQL_Query;
            R : Forward_Cursor;
         begin
            --  list all message in resources_messages

            Q := SQL_Select
              (To_List
                 ((0 => +D.Messages.Id,
                   1 => +D.Messages.Rule_Id,
                   2 => +D.Messages.Data,
                   3 => +D.Messages.Category_Id,
                   4 => +D.Resources_Messages.Line,
                   5 => +D.Resources_Messages.Col_Begin,
                   6 => +D.Resources_Messages.Col_End)),

               From  => D.Messages & D.Resources_Messages,

               Where => D.Resources_Messages.Resource_Id = Resource_Id
                    and D.Messages.Id = D.Resources_Messages.Message_Id);

            R.Fetch (DB, Q);

            while R.Has_Row loop
               Message_Inst := New_Instance (Get_Script (Data), Message_Class);
               Set_Message_Fields
                 (Message_Inst => Message_Inst,
                  Id           => R.Integer_Value (0),
                  Rule_Id      => R.Integer_Value (1),
                  Message_Data => R.Value (2),
                  Category_Id  => R.Integer_Value (3, -1),
                  Line         => R.Integer_Value (4, 0),
                  Col_Begin    => R.Integer_Value (5, 0),
                  Col_End      => R.Integer_Value (6, 0));

               Set_Return_Value (Data, Message_Inst);

               R.Next;
            end loop;

         end;
      else
         raise Python_Error with "Unknown method GNAThub.Resource." & Command;
      end if;
   end Resource_Command_Handler;

   ---------------------------------------
   -- Register_Database_Interaction_API --
   ---------------------------------------

   procedure Register_Database_Interaction_API
     (Repository : Scripts_Repository) is
   begin
      --  Tools

      Tool_Class := Repository.New_Class (Tool_Class_Name);

      Repository.Register_Command
        (Command => Constructor_Method,
         Params  => (1 .. 1 => Param ("name")),
         Handler => Tool_Command_Handler'Access,
         Class   => Tool_Class);

      Repository.Register_Command
        (Command       => "list",
         Params        => No_Params,
         Handler       => Tool_Command_Handler'Access,
         Class         => Tool_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => "clear_references",
         Params        => (1 .. 1 => Param ("tool")),
         Handler       => Tool_Command_Handler'Access,
         Class         => Tool_Class,
         Static_Method => True);

      --  Categories

      Category_Class := Repository.New_Class (Category_Class_Name);

      Repository.Register_Command
        (Command => Constructor_Method,
         Params  =>
           (Param ("label"),
            Param ("on_side", Optional => True)),
         Handler => Category_Command_Handler'Access,
         Class   => Category_Class);

      Repository.Register_Command
        (Command       => "list",
         Params        => No_Params,
         Handler       => Category_Command_Handler'Access,
         Class         => Category_Class,
         Static_Method => True);

      --  Rules

      Rule_Class := Repository.New_Class (Rule_Class_Name);

      Repository.Register_Command
        (Command => Constructor_Method,
         Params  =>
           (Param ("name"),
            Param ("identifier"),
            Param ("kind"),
            Param ("tool")),
         Handler => Rule_Command_Handler'Access,
         Class   => Rule_Class);

      Repository.Register_Command
        (Command       => "list",
         Params        => No_Params,
         Handler       => Rule_Command_Handler'Access,
         Class         => Rule_Class,
         Static_Method => True);

      --  Messages

      Message_Class := Repository.New_Class (Message_Class_Name);

      Repository.Register_Command
        (Command => Constructor_Method,
         Params  =>
           (Param ("rule"),
            Param ("message"),
            Param ("category", Optional => True)),
         Handler => Message_Command_Handler'Access,
         Class   => Message_Class);

      Repository.Register_Command
        (Command       => "list",
         Params        => No_Params,
         Handler       => Message_Command_Handler'Access,
         Class         => Message_Class,
         Static_Method => True);

      --  Resources

      Resource_Class := Repository.New_Class (Resource_Class_Name);

      Repository.Register_Command
        (Command => Constructor_Method,
         Params  =>
           (Param ("name"),
            Param ("kind")),
         Handler => Resource_Command_Handler'Access,
         Class   => Resource_Class);

      Repository.Register_Command
        (Command       => "get",
         Params        => (1 .. 1 => Param ("name")),
         Handler       => Resource_Command_Handler'Access,
         Class         => Resource_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command       => "list",
         Params        => No_Params,
         Handler       => Resource_Command_Handler'Access,
         Class         => Resource_Class,
         Static_Method => True);

      Repository.Register_Command
        (Command   => "add_message",
         Params    =>
           (Param ("message"),
            Param ("line",      Optional => True),
            Param ("col_begin", Optional => True),
            Param ("col_end",   Optional => True)),
         Handler   => Resource_Command_Handler'Access,
         Class     => Resource_Class);

      Repository.Register_Command
        (Command => "add_messages",
         Params  => (1 .. 1 => Param ("messages")),
         Handler => Resource_Command_Handler'Access,
         Class   => Resource_Class);

      Repository.Register_Command
        (Command => "list_messages",
         Params  => No_Params,
         Handler => Resource_Command_Handler'Access,
         Class   => Resource_Class);
   end Register_Database_Interaction_API;

end GNAThub.Python.Database;
