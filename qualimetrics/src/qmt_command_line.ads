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
   --  Command line for qualimetrics

   procedure Configure (Self : in out Qualimetrics_Command_Line);
   --  Define switches handle by qualimetrics

   function Parse
     (Self : in out Qualimetrics_Command_Line;
      Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record) return Boolean;
   --  Parse the command line

   function Get_Project_Name
     (Self : Qualimetrics_Command_Line) return GNAT.Strings.String_Access;
   --  Return Project name given on the command line

   function Get_Script_Arg
     (Self : Qualimetrics_Command_Line) return GNAT.Strings.String_Access;
   --  Returns value for --load switch
   --  Example: --load=python:example.py: return 'python:example.py'

   procedure Destroy (Self : in out Qualimetrics_Command_Line);
   --  Free allocated memory

private
   type Qualimetrics_Command_Line is tagged record
      Command_Line : GNAT.Command_Line.Command_Line_Configuration;
      Project_Name : aliased GNAT.Strings.String_Access;
      Script_Arg   : aliased GNAT.Strings.String_Access;
      Version      : aliased Boolean;
      Quiet        : aliased Boolean;
      Verbose      : aliased Boolean;
   end record;

end Qmt_Command_Line;
