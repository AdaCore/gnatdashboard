------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;   use GNATCOLL.SQL.Sqlite;

with Utils;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body Database_Interface is

   function Kind_Factory
     (From    : Base_Element'Class;
      Default : Detached_Element'Class) return Detached_Element'Class;
   --  ???

   ------------------
   -- Kind_Factory --
   ------------------

   function Kind_Factory
     (From    : Base_Element'Class;
      Default : Detached_Element'Class) return Detached_Element'Class
   is
   begin
      --  Manage rule kind
      if From in Rule'Class then
         case Rule (From).Kind is
         when Rule_Kind'Pos (Kind_Rule) =>
            return R : My_Detached_Rule do null; end return;
         when Rule_Kind'Pos (Kind_Metric) =>
            return R : Detached_Metric do null; end return;
         when others =>
            return Default;
         end case;
      end if;

      --  Manage resource kind
      if From in Resource'Class then
         case Resource (From).Kind is
         when Resource_Kind'Pos (Kind_Project) =>
            return R : Detached_Project do null; end return;
         when Resource_Kind'Pos (Kind_Directory) =>
            return R : Detached_Directory do null; end return;
         when Resource_Kind'Pos (Kind_File) =>
            return R : Detached_File do null; end return;
         when others =>
            return Default;
         end case;
      end if;

      return Default;
   end Kind_Factory;

   -------------------
   -- Initialize_DB --
   -------------------

   function Initialise_DB
     (DB_File     : Virtual_File;
      Schema_File : Virtual_File) return Boolean
   is
      Descr          : Database_Description;
      Schema_IO      : DB_Schema_IO;
      Schema         : DB_Schema;
      Delete_Succeed : Boolean;
   begin
      --  Check existance of a database, delete it before creating a new one
      Trace (Utils.Debug_Trace, "  Removing old DB");
      if Is_Regular_File (DB_File) then
         Delete (DB_File, Delete_Succeed);
         if not Delete_Succeed then
            return Utils.Return_On_Failure
              ("Unable to delete old Sqlite file: " &
                 DB_File.Display_Full_Name);
         end if;
      end if;

      --  Retieve schema from text file
      Trace (Utils.Debug_Trace, "  Reading DB schema from file: "
             & Schema_File.Display_Full_Name);
      Schema := New_Schema_IO (Schema_File).Read_Schema;

      Descr := GNATCOLL.SQL.Sqlite.Setup
        (Database => DB_File.Display_Full_Name);

      Schema_IO.DB := Descr.Build_Connection;
      Write_Schema (Schema_IO, Schema);

      --  Initialize session pool
      GNATCOLL.SQL.Sessions.Setup (Descr, Max_Sessions);
      Set_Default_Factory (Kind_Factory'Access);

      if not Schema_IO.DB.Success then
         return Utils.Return_On_Failure ("Unable to initialise the Database");
      end if;

      return True;
   end Initialise_DB;

   ------------------------------
   -- Create_And_Save_Resource --
   ------------------------------

   function Create_And_Save_Resource
     (Name : String;
      Kind : Resource_Kind) return Detached_Resource
   is
      Session      : constant Session_Type := Get_New_Session;
      Resource : constant Detached_Resource'Class := New_Resource;
   begin
      Resource.Set_Name (Name);
      Resource.Set_Kind (Resource_Kind'Pos (Kind));

      Session.Persist (Resource);
      Session.Commit;

      return Detached_Resource (Resource);
   end Create_And_Save_Resource;

   ------------------------
   -- Save_Resource_Tree --
   ------------------------

   procedure Save_Resource_Tree
     (Child  : Detached_Resource;
      Parent : Detached_Resource)
   is
      Session  : constant Session_Type := Get_New_Session;
      Tree     : constant Detached_Resource_Tree'Class := New_Resource_Tree;
   begin
      Tree.Set_Child_Id (Child);
      if not (Parent = No_Detached_Resource) then
         Tree.Set_Parent_Id (Parent);
      end if;

      Session.Persist (Tree);
      Session.Commit;
   end Save_Resource_Tree;

end Database_Interface;
