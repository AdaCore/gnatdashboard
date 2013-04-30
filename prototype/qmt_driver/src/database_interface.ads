--
--  QMT Driver$
--  Copyright (C) 2012-2013, AdaCore
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Orm;          use Orm;

package Database_Interface is

   function Initialize_DB (Object_Dir : Virtual_File) return Boolean;
   --  ???

   function Save_Resource (Name : String;
                           Kind : Integer) return Detached_Resource;
   --  ???

   function Save_Resource_And_Tree (Name    : String;
                                    Kind    : Integer;
                                    Parent  : Detached_Resource)
                                    return Detached_Resource;
   --  ???

   procedure Save_Resource_Tree (Child  : Detached_Resource;
                                 Parent : Detached_Resource);
   --  ???

private
   Max_Sessions        : Natural                    := 2;
   DB_Schema_File_Name : Filesystem_String          := "dbschema.txt";
   DB_File_Name        : constant Filesystem_String := "qualimetrics.db";
end Database_Interface;
