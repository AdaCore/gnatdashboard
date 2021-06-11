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

with GNAT.OS_Lib;

with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

package body GPR2_GNATCOLL_Projects is

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : String := "";
      Default        : String := "") return String is

      Attribute : constant GPR2.Project.Attribute.Object :=
                    Project.Attribute
                      (Name   => Attribute_Name,
                       Pack   => Package_Name,
                       Index  => (if Index = ""
                                  then GPR2.Project.Attribute_Index.Undefined
                                  else GPR2.Project.Attribute_Index.Create
                                    (Index)));

      use GPR2.Project.Registry.Attribute;
   begin
      if Attribute.Is_Defined
        and then Attribute.Kind = GPR2.Project.Registry.Attribute.Single
      then
         return Attribute.Value.Text;
      else
         return Default;
      end if;
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project        : GPR2.Project.View.Object;
      Attribute_Name : Attribute_Id;
      Package_Name   : Optional_Package_Id := No_Package;
      Index          : String := "")
      return GNAT.Strings.String_List_Access is

      function List (Attribute : GPR2.Project.Attribute.Object)
                     return GNAT.Strings.String_List_Access;
      --  convert attribute values to string list

      ----------
      -- List --
      ----------

      function List (Attribute : GPR2.Project.Attribute.Object)
                     return GNAT.Strings.String_List_Access is
      begin
         if Attribute.Is_Defined then
            declare
               List : constant GNAT.Strings.String_List_Access :=
                        new GNAT.Strings.String_List
                          (1 .. Integer (Attribute.Count_Values));
               I    : Integer := 1;
            begin
               for Value of Attribute.Values loop
                  List (I) := new String'(Value.Text);
                  I := I + 1;
               end loop;
               return List;
            end;
         else
            return null;
         end if;
      end List;

   begin
      return List
        (Project.Attribute
           (Name   => Attribute_Name,
            Pack   => Package_Name,
            Index  => (if Index = ""
                       then GPR2.Project.Attribute_Index.Undefined
                       else GPR2.Project.Attribute_Index.Create
                         (Index))));
   end Attribute_Value;

   ------------
   -- Create --
   ------------

   function Create
     (Self            : GPR2.Project.Tree.Object;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : GPR2.Project.View.Object'Class :=
        GPR2.Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File is
      use GNATCOLL.VFS;
   begin
      if GNAT.OS_Lib.Is_Absolute_Path (+Name) then
         return GNATCOLL.VFS.Create (Full_Filename => Name);
      else
         declare
            Full_Path : constant GPR2.Path_Name.Object := Self.Get_File
              (Base_Name        => GPR2.Path_Name.Create
                 (GPR2.Filename_Type (Name),
                  GPR2.Filename_Type (Name)).Simple_Name,
               View             => GPR2.Project.View.Object (Project),
               Use_Source_Path  => Use_Source_Path,
               Use_Object_Path  => Use_Object_Path);
         begin
            if not Full_Path.Is_Defined then
               return GNATCOLL.VFS.Create (Full_Filename => Name);
            else
               return GNATCOLL.VFS.Create
                 (Full_Filename => +String (Full_Path.Value));
            end if;
         end;
      end if;
   end Create;

   ---------------------------------
   -- Find_All_Projects_Importing --
   ---------------------------------

   function Find_All_Projects_Importing
     (Project      : GPR2.Project.View.Object;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return GPR2.Project.View.Set.Object
   is
      All_Projects_Importing : GPR2.Project.View.Set.Object;

      procedure Insert (View : GPR2.Project.View.Object);
      --  Insert View if needed in All_Projects_Importing

      ------------
      -- Insert --
      ------------

      procedure Insert (View : GPR2.Project.View.Object) is
      begin
         if not All_Projects_Importing.Contains (View) then
            All_Projects_Importing.Insert (View);
         end if;
      end Insert;
   begin
      if Project.Is_Defined then
         if Include_Self then
            Insert (Project);
         end if;
         for View of Project.Tree.Ordered_Views loop
            if View.Imports (Recursive => not Direct_Only).Contains (Project)
            then
               Insert (View);
            end if;
            if Project.Is_Extended and then View.Is_Extension_Of (Project) then
               Insert (View);
            end if;
         end loop;
         if Project.Is_Aggregated_In_Library then
            for View of Project.Aggregate_Libraries loop
               Insert (View);
            end loop;
         end if;
      end if;
      return All_Projects_Importing;
   end Find_All_Projects_Importing;

end GPR2_GNATCOLL_Projects;
