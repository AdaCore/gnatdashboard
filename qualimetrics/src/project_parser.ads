------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GPS.CLI_Kernels;

package Project_Parser is

   function Load_Project_Tree
     (Path   : GNAT.Strings.String_Access;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) return Boolean;
   --  ???

   procedure Save_Project_Tree (Root_Project : Project_Type);
   --  ???

end Project_Parser;
