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

with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.VFS;   use GNATCOLL.VFS;

package GNAThub.Constants is

   --  GNAThub executable side environment properties

   Prefix_Dir : constant Virtual_File := Create (+Executable_Location);

   Share_Dir : constant Virtual_File :=
     Create_From_Dir (Prefix_Dir, "share/gnathub");

   Core_Dir     : constant Virtual_File := Create_From_Dir (Share_Dir, "core");
   Core_Lib_Dir : constant Virtual_File := Create_From_Dir (Core_Dir, "lib");

   Core_Plugin_Dir : constant Virtual_File :=
     Create_From_Dir (Core_Dir, "plug-ins");

   User_Plugin_Dir : constant Virtual_File :=
     Create_From_Dir (Share_Dir, "plug-ins");

   --  GNAThub project side environment properties, located in project
   --  object directory

   Exec_Dir : constant Virtual_File := Create ("gnathub");
   Logs_Dir : constant Virtual_File := Create_From_Dir (Exec_Dir, "logs");
   DB_File  : constant Virtual_File :=
     Create_From_Dir (Exec_Dir, "gnathub.db");

   --  Ada to Python API

   Python_Root_Module : constant String := "Qmt";

   --  GNAThub helper scripts

   Database_Schema_File : constant Virtual_File :=
     Create_From_Dir (Core_Dir, "database-schema.txt");

   Custom_Attributes_Definition_File : constant Virtual_File :=
     Create_From_Dir (Core_Dir, "custom-attributes-definition.py");

   Plugin_Runner : constant Virtual_File :=
     Create_From_Dir (Core_Dir, "generic-plugin-runner.py");

end GNAThub.Constants;
