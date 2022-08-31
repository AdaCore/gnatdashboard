------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package can be used to facilitate GNATCOLL.Projects to GPR2 conversion

--  It provides primitives to emulate:
--   GNATCOLL.Projects.Artifacts_Dir
--   GNATCOLL.Projects.Attribute_Value
--   GNATCOLL.Projects.Create
--   GNATCOLL.Projects.Get_Runtime
--   GNATCOLL.Projects.Get_Target
--   GNATCOLL.Projects.Name
--   GNATCOLL.Projects.Object_Dir
--   GNATCOLL.Projects.Register_New_Attribute

--  The following file path types conversions are provided
--   GNATCOLL.VFS.Filesystem_String / GPR2.Path_Name.Object
--   GNATCOLL.VFS.Virtual_File / GPR2.Path_Name.Object

--  Output_Messages procedure can be used to print info/warnings/error messages

with GNAT.Strings;
with GNATCOLL.VFS;

with GPR2; use GPR2;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;

package GPR2_GNATCOLL_Projects is

   function Create
     (Self            : GPR2.Project.Tree.Object;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : GPR2.Project.View.Object'Class :=
        GPR2.Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File;
   --  GNATCOLL.Projects.Create function conversion

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Attribute_Id;
      Package_Name   : Package_Id := Project_Level_Scope;
      Index          : String := "";
      Default        : String := "") return String;
   --  GNATCOLL.Projects.Attribute_Value conversion (string attribute)

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Attribute_Id;
      Package_Name   : Package_Id := Project_Level_Scope;
      Index          : String := "")
      return GNAT.Strings.String_List_Access;
   --  GNATCOLL.Projects.Attribute_Value conversion (string list attribute)

   function Get_Target (Tree : GPR2.Project.Tree.Object) return String is
     (if Tree.Add_Tool_Prefix ("x") = "x" then "" else String (Tree.Target));
   --  GNATCOLL.Projects.Get_Target conversion

   function Find_All_Projects_Importing
     (Project      : GPR2.Project.View.Object;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return GPR2.Project.View.Set.Object;
   --  GNATCOLL.Projects.Find_All_Projects_Importing conversion

end GPR2_GNATCOLL_Projects;
