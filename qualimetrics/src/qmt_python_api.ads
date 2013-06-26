------------------------------------------------------------------------------
--                 Q u a l i m e t r i c s     D r i v er                   --
--                                                                          --
--                    Copyright (C) 2012-2013, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------
with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS;              use GPS;
with GPS.CLI_Kernels;

package Qmt_Python_Api is

   Repo  : Scripts_Repository := new Scripts_Repository_Record;

   procedure Initialise (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Initialise Python API for qualimetrics.

end Qmt_Python_Api;
