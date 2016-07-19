------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

with GNAT.Source_Info;

with GNATCOLL.SQL;            use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;       use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;    use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions;   use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;     use GNATCOLL.SQL.Sqlite;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with GNAThub.Constants;       use GNAThub.Constants;

package body GNAThub.Database is
   Me : constant Trace_Handle := Create (GNAT.Source_Info.Enclosing_Entity);

   type Abstract_Detached_Rule is
      abstract new Standard.Database.Orm.Detached_Rule with null record;

   type My_Detached_Rule is new Abstract_Detached_Rule with null record;
   type Detached_Metric is new Abstract_Detached_Rule with null record;

   type Abstract_Detached_Resource is
      abstract new Standard.Database.Orm.Detached_Resource with null record;

   type Detached_Project is new Abstract_Detached_Resource with null record;
   type Detached_Directory is new Abstract_Detached_Resource with null record;
   type Detached_File is new Abstract_Detached_Resource with null record;

   Max_Sessions : constant Natural := 2;
   Schema_IO    : DB_Schema_IO;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Database_File : GNATCOLL.VFS.Virtual_File;
      Overwrite     : Boolean)
   is
      SQLite : Database_Description;
   begin
      if Is_Regular_File (Database_File) then
         if Overwrite then
            declare
               Delete_Succeed : Boolean;
            begin
               --  Delete existing DB if any before creating a new one
               Trace (Me, "Delete existing copy of the database");
               Delete (Database_File, Delete_Succeed);

               if not Delete_Succeed then
                  raise Fatal_Error with "Failed to delete existing database";
               end if;
            end;
         else
            Trace (Me, "Use existing copy of the database");
         end if;
      else
         Trace (Me, "No previous copy of the database found");
      end if;

      SQLite := GNATCOLL.SQL.Sqlite.Setup (Database_File.Display_Full_Name);
      Schema_IO.DB := SQLite.Build_Connection;

      if Overwrite then
         declare
            Schema : DB_Schema;
         begin
            --  Retrieve schema from text file
            Trace (Me, "Schema: " & Database_Model_File.Display_Full_Name);
            Schema := New_Schema_IO (Database_Model_File).Read_Schema;

            --  And write it to the database (ie. create table if necessary)
            Trace (Me, "Write to database (create tables)");
            Write_Schema (Schema_IO, Schema);
         end;
      end if;

      if not Schema_IO.DB.Success then
         raise Fatal_Error with "Unable to initialize the database";
      end if;

      --  Initialize session pool
      GNATCOLL.SQL.Sessions.Setup (SQLite, Max_Sessions);
      Set_Default_Factory (Kind_Factory'Access);
   end Initialize;

   ------------------------------
   -- Create_And_Save_Resource --
   ------------------------------

   function Create_And_Save_Resource
     (Name : String;
      Kind : Resource_Kind) return Detached_Resource
   is
      Session  : constant Session_Type := Get_New_Session;
      Resource : constant Detached_Resource'Class := New_Resource;
   begin
      Trace (Me, "Create resource """ & Name & """");

      Resource.Set_Name (Name);
      Resource.Set_Kind (Resource_Kind'Pos (Kind));

      Session.Persist (Resource);
      Session.Commit;

      Trace (Me, "Resource """ & Name & """ saved to database");

      return Detached_Resource (Resource);
   end Create_And_Save_Resource;

   ------------------------
   -- Save_Resource_Tree --
   ------------------------

   procedure Save_Resource_Tree
     (Child  : Detached_Resource;
      Parent : Detached_Resource)
   is
      Session : constant Session_Type := Get_New_Session;
      Tree    : constant Detached_Resource_Tree'Class := New_Resource_Tree;
   begin
      Tree.Set_Child_Id (Child);

      if not (Parent = No_Detached_Resource) then
         Tree.Set_Parent_Id (Parent);
      end if;

      Session.Persist (Tree);
      Session.Commit;
   end Save_Resource_Tree;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Schema_IO.DB /= null then
         Schema_IO.DB.Close;
         Schema_IO.DB := null;
      end if;
   end Finalize;

   --------
   -- DB --
   --------

   function DB return GNATCOLL.SQL.Exec.Database_Connection is
   begin
      return Schema_IO.DB;
   end DB;

end GNAThub.Database;
