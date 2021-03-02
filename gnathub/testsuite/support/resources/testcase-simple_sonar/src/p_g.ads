generic
   type T (<>) is private;
   with function Create (I : Integer := 1) return T;
package P_G is
   function F1 return T;
   pragma Annotate (gnatcheck, Exempt_ON, "Unconstrained_Array_Returns", "exemption 2");
   function F2 return T;
   pragma Annotate (gnatcheck, Exempt_Off, "Unconstrained_Array_Returns");

   type U_Arr is array (Integer range <>) of Integer;
   function F3 return U_Arr;
   function F4 return U_Arr;

   pragma Annotate (gnatcheck, Exempt_ON, "Recursive_Subprograms", "exemption 3");
   function Factorial1 (N : Natural) return Natural;
   pragma Annotate (gnatcheck, Exempt_Off, "Recursive_Subprograms");

   function Factorial2 (N : Natural) return Natural;
end P_G;
