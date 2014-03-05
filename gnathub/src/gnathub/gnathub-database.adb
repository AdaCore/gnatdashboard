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

with GNATCOLL.SQL;            use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;       use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;    use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions;   use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;     use GNATCOLL.SQL.Sqlite;

with GNATCOLL.VFS;            use GNATCOLL.VFS;

with GNAThub.Constants;       use GNAThub.Constants;

package body GNAThub.Database is

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

   procedure Initialize (Database_File : GNATCOLL.VFS.Virtual_File) is
      Descr          : Database_Description;
      Schema         : DB_Schema;
      Delete_Succeed : Boolean;

   begin
      --  Check existance of a database, delete it before creating a new one

      if Is_Regular_File (Database_File) then
         Log.Debug ("Removing previous copy of the database: " &
                    Database_File.Display_Full_Name);

         Delete (Database_File, Delete_Succeed);

         if not Delete_Succeed then
            raise Error with "Unable to delete old Sqlite file: " &
                             Database_File.Display_Full_Name;
         end if;
      end if;

      --  Retieve schema from text file
      Log.Debug
        ("Loading database schema: " &
         GNAThub.Constants.Database_Model_File.Display_Full_Name);

      Schema := New_Schema_IO
        (GNAThub.Constants.Database_Model_File).Read_Schema;

      Descr := GNATCOLL.SQL.Sqlite.Setup
        (Database => Database_File.Display_Full_Name);

      Schema_IO.DB := Descr.Build_Connection;
      Write_Schema (Schema_IO, Schema);

      --  Initialize session pool
      GNATCOLL.SQL.Sessions.Setup (Descr, Max_Sessions);
      Set_Default_Factory (Kind_Factory'Access);

      if not Schema_IO.DB.Success then
         raise Error with "Unable to initialize the Database";
      end if;
   end Initialize;

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
