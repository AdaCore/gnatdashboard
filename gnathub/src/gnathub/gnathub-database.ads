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

with Database.Orm;               use Database.Orm;

with GNATCOLL.VFS;
with GNATCOLL.SQL.Exec;

--  This package defines the database interaction

package GNAThub.Database is

   ----------------
   --  Rule Kind --
   ----------------

   type Rule_Kind is (Kind_Rule, Kind_Metric);
   --  Correspond to the value of "kind" field in "Rule" table DB.
   --  Defined as follow: 0 -> Rule, 1 -> Metric

   ---------------------
   --  Resource Kind  --
   ---------------------

   type Resource_Kind is (Kind_Project, Kind_Directory, Kind_File);
   --  Corresponds to the value of "kind" field in "Resource" table DB.
   --  Defined as follow: 0 -> Project, 1 -> Directory, 2 -> File

   ---------------------
   --  DB Management  --
   ---------------------

   procedure Initialize
     (Database_File            : GNATCOLL.VFS.Virtual_File;
      Remove_Previous_Database : Boolean);
   --  Connect to the gnathub database.
   --
   --  If Remove_Previous_Database, remove any existing gnathub database,
   --  and initialize a new one with the gnathub schema.
   --
   --  Session pool is setup by this function.

   procedure Finalize;
   --  Terminate the database connection.

   -------------------------
   -- Data Access Objects --
   -------------------------

   function Create_And_Save_Resource
     (Name : String;
      Kind : Resource_Kind) return Detached_Resource;
   --  Create a Detached_Resource Object and persist it in the DB.
   --
   --  Name: the absolute file path
   --  Kind: kind of the resource
   --  Returns the created Detached_Resource object that represents the
   --  persisted resource in the database.

   procedure Save_Resource_Tree
     (Child  : Detached_Resource;
      Parent : Detached_Resource);
   --  Persist resource dependence in the project tree.
   --
   --  Child: DB object representation of the child resource.
   --  Parent: DB object representation of the parent resource.

   function DB return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return the currently connected database

end GNAThub.Database;
