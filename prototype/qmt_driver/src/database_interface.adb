--
--  QMT Driver
--  Copyright (C) 2012-2013, AdaCore

with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;   use GNATCOLL.SQL.Sqlite;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with Ada.Text_IO; use Ada.Text_IO;

package body Database_Interface is

   -------------------
   -- Initialize_DB --
   -------------------

   function Initialize_DB (Object_Dir : Virtual_File) return Boolean
   is
      Descr          : Database_Description;
      Schema_IO      : DB_Schema_IO;
      Schema         : DB_Schema;
      DB_File        : Virtual_File;
      Delete_Succeed : Boolean;
   begin
      Put_Line ("DB will be created in directory: "
                & Object_Dir.Display_Full_Name);
      DB_File := Create_From_Dir (Object_Dir, DB_File_Name);

      --  Check existance of a database, delete it before creating a new one
      if Is_Regular_File (DB_File) then
         Delete (DB_File, Delete_Succeed);
         if not Delete_Succeed then
            return False;
         end if;
      end if;

      Schema := New_Schema_IO (Create (DB_Schema_File_Name)).Read_Schema;

      Descr := GNATCOLL.SQL.Sqlite.Setup
        (Database => DB_File.Display_Full_Name);

      Schema_IO.DB := Descr.Build_Connection;
      Write_Schema (Schema_IO, Schema);

      --  Initialize session
      GNATCOLL.SQL.Sessions.Setup (Descr, Max_Sessions);

      return Schema_IO.DB.Success;

   end Initialize_DB;

   -------------------
   -- Save_Resource --
   -------------------

   function Save_Resource (Name : String;
                           Kind : Integer) return Detached_Resource
   is
      Session  : constant Session_Type := Get_New_Session;
      Resource : constant Detached_Resource'Class := New_Resource;
   begin
--        Put_Line ("[INFO] Save_Resource - Begin");
      --  Create Resource
      Resource.Set_Name (Name);
      Resource.Set_Kind (Kind);

      Session.Persist (Resource);
      Session.Commit;
--        Put_Line
--          ("[INFO] Save_Resource - Saving : " & Name
--          & "/ kind : " & Kind'Img);

      return Detached_Resource (Resource);
   end Save_Resource;

   ----------------------------
   -- Save_Resource_And_Tree --
   ----------------------------

   function Save_Resource_And_Tree (Name    : String;
                                    Kind    : Integer;
                                    Parent  : Detached_Resource)
                                    return Detached_Resource
   is
      Resource : Detached_Resource;
   begin
      --  Create Resource
      Resource := Save_Resource (Name, Kind);
--        Put_Line
--          ("[INFO] Save_Resource_And_Tree - Saving : " & Name
--           & "/ kind : " & Kind'Img);

      Save_Resource_Tree (Child  => Resource,
                          Parent => Parent);

      return Resource;
   end Save_Resource_And_Tree;

   ------------------------
   -- Save_Resource_Tree --
   ------------------------

   procedure Save_Resource_Tree (Child  : Detached_Resource;
                                 Parent : Detached_Resource)
   is
      Session  : constant Session_Type := Get_New_Session;
      Tree     : constant Detached_Resource_Tree'Class := New_Resource_Tree;
   begin

      Tree.Set_Child_Id (Child);
      if not (Parent = No_Detached_Resource) then
         Tree.Set_Parent_Id (Parent);
--           Put_Line
--             ("[INFO] Save_Resource_And_Tree - With Parent : "
--              & Parent.Name);
      end if;

      Session.Persist (Tree);
      Session.Commit;
   end Save_Resource_Tree;

end Database_Interface;
