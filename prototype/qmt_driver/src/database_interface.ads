------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.VFS; use GNATCOLL.VFS;
with Orm;          use Orm;

--  This package gathered all functions and procedures that allow to
--  interract with the DB.

package Database_Interface is

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

   -----------------
   -- Data Access --
   -----------------

   function Initialize_DB (Object_Dir : Virtual_File) return Boolean;
   --  Create Qualimetrics DB sqlite file. And initialize DB context that will
   --  be use in the whole program to interract with the DB. Session pool
   --  is setup by this function.
   --
   --  Object_Dir: the directory where the DB will be created:
   --      - project object directory if it is specified
   --      - otherwise in the same directory than the project root file is.
   --  Returns wether the DB has been succefully created or not.

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

private
   Max_Sessions        : Natural                    := 2;
   DB_Schema_File_Name : Filesystem_String          := "dbschema.txt";
   DB_File_Name        : constant Filesystem_String := "qualimetrics.db";
end Database_Interface;
