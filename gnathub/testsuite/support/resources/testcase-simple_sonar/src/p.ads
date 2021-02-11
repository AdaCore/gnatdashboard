with P_G;
with P_G_G;
package P is
   type Arr is array (Integer range <>) of Integer;

   function F1 (I : Integer) return Arr;

   pragma Annotate (gnatcheck, Exempt_ON, "Unconstrained_Array_Returns", "exemption 1");
   function F2 (I : Integer) return Arr;
   pragma Annotate (gnatcheck, Exempt_Off, "Unconstrained_Array_Returns");

   package P_Inst is new P_G (Arr, F1);
   package P_Inst_Inst is new P_G_G;

end P;
