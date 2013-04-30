--
--  QMT Driver$
--  Copyright (C) 2012-2013, AdaCore

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with Ada.Text_IO;           use Ada.Text_IO;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with Ada.Command_Line;      use Ada.Command_Line;
with Database_Interface;    use Database_Interface;
with Project_Parser;        use Project_Parser;
with Orm;                   use Orm;
--  with GNATCOLL.Traces;

procedure Qualimetrics is
   procedure Main;
   --  ???

   ----------
   -- Main --
   ----------

   procedure Main
   is
      Tree : Project_Tree;
      DB_Succeed   : Boolean;
      Project_File : constant Virtual_File := Create
        (+"/Users/kiwi/code/gps/gps/gps.gpr");
   begin
--        GNATCOLL.Traces.Parse_Config_File (".gnatdebug");
      Tree := Load_Project_Tree (Project_File);

      DB_Succeed := Initialize_DB (if Tree.Root_Project.Object_Dir = No_File
                                   then Tree.Root_Project.Project_Path.Dir
                                   else Tree.Root_Project.Object_Dir);
      if DB_Succeed then
         Put_Line ("DB Created");
      end if;

      Save_Project_Tree (Root_Project => Tree.Root_Project);
      Tree.Unload;

--        GNATCOLL.Traces.Finalize;
   end Main;

begin

   Main;

end Qualimetrics;
