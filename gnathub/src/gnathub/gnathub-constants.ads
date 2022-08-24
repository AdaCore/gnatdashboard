------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2022, AdaCore                     --
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

with GNATCOLL.VFS;   use GNATCOLL.VFS;

package GNAThub.Constants is

   function Python_Home return Virtual_File;
   --  The GNAThub Python installation HOME

   function Scripts_Dir return Virtual_File;
   --  The directory containing the main Python entry scripts

   function Core_Plugins_Dir return Virtual_File;
   --  Core plugins packaged with GNAThub

   function Extra_Plugins_Dir return Virtual_File;
   --  Directory expected to contains zero or more user-defined plugins

   function Python_Root_Module return String;
   --  Ada to Python module name. Every entity of this module will be imported
   --  into the "GNAThub" module after initialization. This module should
   --  remain private to users.

   function Database_Model_File return Virtual_File;
   --  GNAThub database model file for database initialization

   function Plugin_Runner return Virtual_File;
   --  The path to the script in charge of chaining the execution of all
   --  plugins (core + user-defined).

   --  GNAThub project side environment properties, located in project
   --  object directory.

   function Root_Dir return Virtual_File;
   --  GNAThub root directory to be created in the project's object dir

   function Logs_Dir return Virtual_File;
   --  Directory containing the logs of a GNAThub execution

   function HTML_Data_Dir return Virtual_File;
   --  Directory containing the HTML report data of a GNAThub execution

   function Database_File return Virtual_File;
   --  The GNAThub database file where all plugins store or read their data

   --  Codepeer specific accessors for WebUI interface

   function Obj_Codepeer_Dir return Virtual_File;
   --  Directory containig Codepeer execution results after GNAThub run

   function Codepeer_Output_Dir return Virtual_File;
   --  [prj_name].output directory contained inside the Codepeer folder

   function Codepeer_DB_Dir return Virtual_File;
   --  [prj_name].db directory contained inside the Codepeer folder

end GNAThub.Constants;
