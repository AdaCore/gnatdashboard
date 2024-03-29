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

with Ada.Characters.Handling;

with GNATCOLL.Utils; use GNATCOLL.Utils;

with GNAThub.Project;       use GNAThub.Project;

package body GNAThub.Constants is

   function Prefix_Dir return Virtual_File;
   --  The directory containing the GNAThub executable

   function Share_Dir return Virtual_File;
   --  The share/ directory in GNAThub's installation

   function Sql_Dir return Virtual_File;
   --  The sql/ directory in GNAThub's installation

   ----------------
   -- Prefix_Dir --
   ----------------

   function Prefix_Dir return Virtual_File is
   begin
      return Create (+Executable_Location);
   end Prefix_Dir;

   -----------------
   -- Python_Home --
   -----------------

   function Python_Home return Virtual_File is
   begin
      return Create_From_Dir (Prefix_Dir, "share/gnathub/python");
   end Python_Home;

   ---------------
   -- Share_Dir --
   ---------------

   function Share_Dir return Virtual_File is
   begin
      return Create_From_Dir (Prefix_Dir, "share/gnathub");
   end Share_Dir;

   -------------
   -- Sql_Dir --
   -------------

   function Sql_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "sql");
   end Sql_Dir;

   ----------------------
   -- Core_Plugins_Dir --
   ----------------------

   function Core_Plugins_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "core");
   end Core_Plugins_Dir;

   -----------------
   -- Scripts_Dir --
   -----------------

   function Scripts_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "core/scripts");
   end Scripts_Dir;

   -----------------------
   -- Extra_Plugins_Dir --
   -----------------------

   function Extra_Plugins_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "extras");
   end Extra_Plugins_Dir;

   ------------------------
   -- Python_Root_Module --
   ------------------------

   function Python_Root_Module return String is
   begin
      return "GNAThubCore";
   end Python_Root_Module;

   -------------------
   -- Plugin_Runner --
   -------------------

   function Plugin_Runner return Virtual_File is
      (Create_From_Dir (Scripts_Dir, "plugin-runner.py"));

   -------------------------
   -- Database_Model_File --
   -------------------------

   function Database_Model_File return Virtual_File is
   begin
      return Create_From_Dir (Sql_Dir, "db.model");
   end Database_Model_File;

   --------------
   -- Root_Dir --
   --------------

   function Root_Dir return Virtual_File is
   begin
      return Create ("gnathub");
   end Root_Dir;

   --------------
   -- Logs_Dir --
   --------------

   function Logs_Dir return Virtual_File is
   begin
      return Create_From_Dir (Root_Dir, "logs");
   end Logs_Dir;

   -------------------
   -- HTML_Data_Dir --
   -------------------

   function HTML_Data_Dir return Virtual_File
   is
      Report_Dir : constant Virtual_File :=
        Create_From_Dir (Root_Dir, "html-report");
   begin
      return Create_From_Dir (Report_Dir, "data");
   end HTML_Data_Dir;

   -------------------
   -- Database_File --
   -------------------

   function Database_File return Virtual_File is
   begin
      return Create_From_Dir (Root_Dir, "gnathub.db");
   end Database_File;

   ----------------------
   -- Obj_Codepeer_Dir --
   ----------------------

   function Obj_Codepeer_Dir return Virtual_File is
   begin
      return Create_From_Dir (GNAThub.Project.Artifacts_Dir, "codepeer");
   end Obj_Codepeer_Dir;

   -------------------------
   -- Codepeer_Output_Dir --
   -------------------------

   function Codepeer_Output_Dir return Virtual_File is
      Name : constant Filesystem_String :=
        Filesystem_String
          (Ada.Characters.Handling.To_Lower (GNAThub.Project.Name));

      Dir  : Virtual_File;
   begin
      if GNAThub.Project.Project_Output_Directory /= No_File then
         --  Store value if "output_directory" attribute found in project
         Dir := GNAThub.Project.Project_Output_Directory;

      else
         Dir := Create_From_Dir (Obj_Codepeer_Dir, Name & ".output");
      end if;

      return Dir;
   end Codepeer_Output_Dir;

   ---------------------
   -- Codepeer_DB_Dir --
   ---------------------

   function Codepeer_DB_Dir return Virtual_File is
      Name : constant Filesystem_String :=
        Filesystem_String
          (Ada.Characters.Handling.To_Lower (GNAThub.Project.Name));

      Dir : Virtual_File;
   begin
      if GNAThub.Project.Project_Database_Directory /= No_File then
         --  Store value if "database_directory" attribute found in project
         Dir := GNAThub.Project.Project_Database_Directory;

      else
         Dir := Create_From_Dir (Obj_Codepeer_Dir, Name & ".db");
      end if;

      return Dir;
   end Codepeer_DB_Dir;

end GNAThub.Constants;
