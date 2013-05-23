------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

with GPS;
with GPS.CLI_Kernels;

with GNAT;
with GNAT.Command_Line;
with GNAT.Strings;

package Qmt_Command_Line is

   type Qualimetrics_Command_Line is tagged private;
   --  ???

   procedure Configure (Self : in out Qualimetrics_Command_Line);
   --  ???

   function Parse
     (Self : in out Qualimetrics_Command_Line;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) return Boolean;
   --  ???

   function Get_Project_Name
     (Self : Qualimetrics_Command_Line) return GNAT.Strings.String_Access;
   --  ???

   procedure Destroy (Self : in out Qualimetrics_Command_Line);
   --  ???

private
   type Qualimetrics_Command_Line is tagged record
      Command_Line : GNAT.Command_Line.Command_Line_Configuration;
      Project_Name : aliased GNAT.Strings.String_Access;
   end record;

end Qmt_Command_Line;
