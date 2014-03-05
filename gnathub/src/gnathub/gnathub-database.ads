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

with Orm;               use Orm;

with GNATCOLL.VFS;

--  This package gathered all functions and procedures that allow to
--  interact with the DB.

package GNAThub.Database is

   --------------------
   -- Object Factory --
   --------------------

   --  Rule Kind --

   type Rule_Kind is (Kind_Rule, Kind_Metric);
   --  Correspond to the value of "kind" field in "Rule" table DB.
   --  Defined as follow: 0 -> Rule, 1 -> Metric

   type Abstract_Detached_Rule is abstract new Orm.Detached_Rule
     with null record;
   --  ???

   type My_Detached_Rule is new Abstract_Detached_Rule with null record;
   --  ???
   type Detached_Metric is new Abstract_Detached_Rule with null record;
   --  ???

   --  Resource Kind  --

   type Resource_Kind is (Kind_Project, Kind_Directory, Kind_File);
   --  Correspond to the value of "kind" field in "Resource" table DB.
   --  Defined as follow: 0 -> Project, 1 -> Directory, 2 -> File

   type Abstract_Detached_Resource is abstract new Orm.Detached_Resource
     with null record;
   --  ???

   type Detached_Project is new Abstract_Detached_Resource with null record;
   --  ???
   type Detached_Directory is new Abstract_Detached_Resource with null record;
   --  ???
   type Detached_File is new Abstract_Detached_Resource with null record;
   --  ???

   ---------------------
   --  DB Management  --
   ---------------------

   procedure Initialize (Database_File : GNATCOLL.VFS.Virtual_File);
   --  Create GNAThub DB sqlite file. And initialize DB context that will
   --  be use in the whole program to interract with the DB. Session pool
   --  is setup by this function.
   --
   --  Object_Dir: directory in which the DB will be created.
   --  Schema_File: the file containing the database schema
   --  Returns wether the DB has been succefully created or not.

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

end GNAThub.Database;