------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GNAT.Strings;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with GPS.CLI_Kernels;

package GNAThub.Project is

   function Initialized return Boolean;
   --  Whether a call to Load_Project_Tree has already been made

   procedure Load_Project_Tree
     (Path   : GNAT.Strings.String_Access;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  ???

   procedure Save_Project_Tree (Root_Project : Project_Type);
   --  ???

   function Object_Dir return Virtual_File
     with Pre => Initialized;
   --  Returns the object directory file

end GNAThub.Project;
