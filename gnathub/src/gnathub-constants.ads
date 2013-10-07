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

   Scripts_Dir : constant Virtual_File :=
     Create_From_Dir (Share_Dir, "scripts");

   Lib_Dir  : constant Virtual_File := Create_From_Dir (Share_Dir, "lib");

   Core_Plugins_Dir : constant Virtual_File :=
     Create_From_Dir (Share_Dir, "core");

   Extra_Plugins_Dir : constant Virtual_File :=
     Create_From_Dir (Share_Dir, "extras");

   --  Ada to Python module name. Every entity of this module will be imported
   --  into the "GNAThub" module after initialization. This module should
   --  remain private to users.

   Python_Root_Module : constant String := "GNAThubCore";

   --  GNAThub database schema

   Database_Schema_File : constant Virtual_File :=
     Create_From_Dir (Scripts_Dir, "database-schema.txt");

   --  GNAThub helper scripts

   Custom_Attributes_Definition_File : constant Virtual_File :=
     Create_From_Dir (Scripts_Dir, "custom-attributes.py");

   Plugin_Runner : constant Virtual_File :=
     Create_From_Dir (Scripts_Dir, "plugin-runner.py");

   --  GNAThub project side environment properties, located in project
   --  object directory

   Root_Dir : constant Virtual_File := Create ("gnathub");
   Logs_Dir : constant Virtual_File := Create_From_Dir (Root_Dir, "logs");

   Database_File  : constant Virtual_File :=
     Create_From_Dir (Root_Dir, "gnathub.db");

end GNAThub.Constants;
