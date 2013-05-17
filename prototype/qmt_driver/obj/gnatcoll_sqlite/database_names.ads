------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Categories : aliased constant String := "categories";
   Ta_Categories : constant Cst_String_Access := TC_Categories'Access;
   TC_Entities : aliased constant String := "entities";
   Ta_Entities : constant Cst_String_Access := TC_Entities'Access;
   TC_Entities_Messages : aliased constant String := "entities_messages";
   Ta_Entities_Messages : constant Cst_String_Access := TC_Entities_Messages'Access;
   TC_Lines : aliased constant String := "lines";
   Ta_Lines : constant Cst_String_Access := TC_Lines'Access;
   TC_Lines_Messages : aliased constant String := "lines_messages";
   Ta_Lines_Messages : constant Cst_String_Access := TC_Lines_Messages'Access;
   TC_Messages : aliased constant String := "messages";
   Ta_Messages : constant Cst_String_Access := TC_Messages'Access;
   TC_Resource_Trees : aliased constant String := "resource_trees";
   Ta_Resource_Trees : constant Cst_String_Access := TC_Resource_Trees'Access;
   TC_Resources : aliased constant String := "resources";
   Ta_Resources : constant Cst_String_Access := TC_Resources'Access;
   TC_Resources_Messages : aliased constant String := "resources_messages";
   Ta_Resources_Messages : constant Cst_String_Access := TC_Resources_Messages'Access;
   TC_Rules : aliased constant String := "rules";
   Ta_Rules : constant Cst_String_Access := TC_Rules'Access;
   TC_Tools : aliased constant String := "tools";
   Ta_Tools : constant Cst_String_Access := TC_Tools'Access;

   NC_Category_Id : aliased constant String := "category_id";
   N_Category_Id : constant Cst_String_Access := NC_category_id'Access;
   NC_Child_Id : aliased constant String := "child_id";
   N_Child_Id : constant Cst_String_Access := NC_child_id'Access;
   NC_Col_Begin : aliased constant String := "col_begin";
   N_Col_Begin : constant Cst_String_Access := NC_col_begin'Access;
   NC_Col_End : aliased constant String := "col_end";
   N_Col_End : constant Cst_String_Access := NC_col_end'Access;
   NC_Data : aliased constant String := """data""";
   N_Data : constant Cst_String_Access := NC_data'Access;
   NC_Entity_Id : aliased constant String := "entity_id";
   N_Entity_Id : constant Cst_String_Access := NC_entity_id'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Kind : aliased constant String := "kind";
   N_Kind : constant Cst_String_Access := NC_kind'Access;
   NC_Label : aliased constant String := "label";
   N_Label : constant Cst_String_Access := NC_label'Access;
   NC_Line : aliased constant String := "line";
   N_Line : constant Cst_String_Access := NC_line'Access;
   NC_Line_Begin_Id : aliased constant String := "line_begin_id";
   N_Line_Begin_Id : constant Cst_String_Access := NC_line_begin_id'Access;
   NC_Line_Id : aliased constant String := "line_id";
   N_Line_Id : constant Cst_String_Access := NC_line_id'Access;
   NC_Message_Id : aliased constant String := "message_id";
   N_Message_Id : constant Cst_String_Access := NC_message_id'Access;
   NC_Name : aliased constant String := """name""";
   N_Name : constant Cst_String_Access := NC_name'Access;
   NC_Parent_Id : aliased constant String := "parent_id";
   N_Parent_Id : constant Cst_String_Access := NC_parent_id'Access;
   NC_Resource_Id : aliased constant String := "resource_id";
   N_Resource_Id : constant Cst_String_Access := NC_resource_id'Access;
   NC_Rule_Id : aliased constant String := "rule_id";
   N_Rule_Id : constant Cst_String_Access := NC_rule_id'Access;
   NC_Timestamp : aliased constant String := """timestamp""";
   N_Timestamp : constant Cst_String_Access := NC_timestamp'Access;
   NC_Tool_Id : aliased constant String := "tool_id";
   N_Tool_Id : constant Cst_String_Access := NC_tool_id'Access;
end Database_Names;
