------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Command_Line;
with GNAT.IO;          use GNAT.IO;

package body Utils is

   -----------------------
   -- Return_On_Failure --
   -----------------------

   function Return_On_Failure (Message : String) return Boolean is
   begin
      --  /!\ Replace with Traces
      Put_Line (Message);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return False;
   end Return_On_Failure;

   ---------------------------
   -- Get_Deposit_Directory --
   ---------------------------

   function Get_Deposit_Directory
     (Root_Project : Project_Type) return Virtual_File is
   begin

      if Root_Project.Object_Dir = No_File then
         return Root_Project.Project_Path.Dir;
      else
         return Root_Project.Object_Dir;
      end if;

   end Get_Deposit_Directory;

end Utils;
