--
--  QMT Driver$
--  Copyright (C) 2012-2013, AdaCore
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package Project_Parser is

   function Load_Project_Tree (Project_File : Virtual_File)
                               return Project_Tree;
   --  ???

   procedure Save_Project_Tree (Root_Project : Project_Type);
   --  ???

end Project_Parser;
