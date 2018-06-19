------------------------------------------------------------------------------
--                               G N A T h u b                              --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

   -----------------------
   -- Server_Engine_Dir --
   -----------------------

   function Server_Engine_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "engine");
   end Server_Engine_Dir;

   ----------------------
   -- Core_Plugins_Dir --
   ----------------------

   function Core_Plugins_Dir return Virtual_File is
   begin
      return Create_From_Dir (Share_Dir, "core");
   end Core_Plugins_Dir;

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
      Runner : Virtual_File := Create_From_Dir
                                 (Python_Home, "bin/plugin-runner.py");
   begin
      if not Runner.Is_Regular_File then
         Runner := Create_From_Dir (Python_Home, "Scripts/plugin-runner.py");
      end if;

      return Runner;
   end Plugin_Runner;

   -------------------
   -- Server_Runner --
   -------------------

   function Server_Runner return Virtual_File is
      Runner : Virtual_File := Create_From_Dir
                                 (Python_Home, "bin/server-runner.py");
   begin
      if not Runner.Is_Regular_File then
         Runner := Create_From_Dir (Python_Home, "Scripts/server-runner.py");
      end if;

      return Runner;
   end Server_Runner;

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

end GNAThub.Constants;
