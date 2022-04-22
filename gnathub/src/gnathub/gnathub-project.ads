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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                   use GNAT.OS_Lib;

with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GPR2; use GPR2;

with GPR2.Project.View.Vector;

package GNAThub.Project is

   GNATdashboard : constant String;
   GNATdashboard_Package : constant GPR2.Package_Id;
   --  GNATdashboard_Package MUST be in lower case to avoid Prj
   --  error "cannot register a package with a non unique name"

   Local_Repository : constant GPR2.Optional_Attribute_Id;

   Project_Error : exception;
   --  Custom error for this module

   function Loaded return Boolean;
   --  Whether a call to Load has already been made

   function Initialized return Boolean;
   --  Whether this module is initialized or not

   procedure Initialize;
   --  Initialize this module

   procedure Load (Path : String)
      with Pre => Initialized;
   --  Load the project file in memory

   function File (Name : String) return Virtual_File;
   --  Create a new file. This will automatically try to resolve Name to an
   --  absolute path if it currently is a base name.
   --  If Name is an absolute path, it is returned as is. Otherwise, only the
   --  base name is used (ie, we remove any directory information from Name).

   procedure Update_Env (Key, Value : String)
      with Pre => Initialized;
   --  Update the environment. Should be called before any project is loaded.
   --  It will not impact already loaded projects.

   type Scenario_Variable is record
      Key   : Unbounded_String;
      Value : Unbounded_String;
   end record;

   package Scenario_Variables_Vector is
     new Ada.Containers.Vectors (Positive, Scenario_Variable);

   function Get_Scenario_Variables return Scenario_Variables_Vector.Vector;
   --  Return the scenario variables that were passed as -X switches on the
   --  command line.

   procedure Save_Project_Tree
     with Pre => Initialized and then Loaded;
   --  Store the project details in database

   function All_Projects return GPR2.Project.View.Vector.Object
      with Pre => Initialized;
   --  Return a flat list of all Projects.

   function Name return String
     with Pre => Initialized and then Loaded;
   --  Return the name of the root project

   function Path return Virtual_File
     with Pre => Initialized and then Loaded;
   --  Return the path of the root project

   function Project_Dir return Virtual_File
     with Pre => Initialized and then Loaded;
   --  Return the project directory of the root project

   function Project_Database_Directory return Virtual_File
     with Pre => Initialized and then Loaded;
     --  Return the project database directory if Database_Directory_Attribute
     --  present or No_File otherwise

   function Project_Output_Directory return Virtual_File
     with Pre => Initialized and then Loaded;
   --  Return the project database directory if Output_Directory_Attribute
   --  present or No_File otherwise

   function Target return String
     with Pre => Initialized and then Loaded;
   --  Return the target configured, if any, otherwise the empty string.
   --  Target defined on the command line (via --target=) overwrites the target
   --  configured in the project, if any.

   function Runtime return String
     with Pre => Initialized and then Loaded;
   --  Return the runtime configured, if any, otherwise the empty string.
   --  Runtime defined on the command line (via --runtime=) overwrites the
   --  runtime configured in the project, if any. This concerns only the
   --  runtime for Ada.

   function Artifacts_Dir return Virtual_File
     with Pre => Initialized and then Loaded;
   --  Return the artifacts directory if IDE mode and this value is defined
   --  or object directory defined for the project or project directory if
   --  the previous conditions are not present.

   function Object_Dir return Virtual_File
     with Pre => Initialized and then Loaded;
   --  Return the object directory file

   function Property_As_String
     (Property     : GPR2.Attribute_Id;
      Package_Name : GPR2.Optional_Package_Id := GNATdashboard_Package;
      Index        : String := "") return String
     with Pre => Initialized and then Loaded;
   --  Return the given property as a string. Return the empty string if the
   --  property does not exist

   function Property_As_List
     (Property     : GPR2.Attribute_Id;
      Package_Name : GPR2.Optional_Package_Id := GNATdashboard_Package;
      Index        : String := "") return String_List_Access
     with Pre => Initialized and then Loaded;
   --  Return the given property as a list. Return an empty list if the
   --  property does not exist

   procedure Finalize;
   --  Free memory used by this package

private

   Local_Repository : constant GPR2.Optional_Attribute_Id :=
                        +"Local_Repository";

   GNATdashboard : constant String := "dashboard";
   GNATdashboard_Package : constant GPR2.Package_Id :=
                             +GPR2.Name_Type (GNATdashboard);
   --  GNATdashboard_Package value

end GNAThub.Project;
