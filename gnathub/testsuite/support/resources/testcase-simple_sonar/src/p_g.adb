package body P_G is

   function F1 return T is
   begin
      return Create;
   end F1;

   function F2 return T is
   begin
      return Create;
   end F2;

   pragma Annotate (gnatcheck, Exempt_ON, "Unconstrained_Array_Returns", "exemption 4");
   function F_Inner_1 return T is
   begin
      return Create;
   end F_Inner_1;
   pragma Annotate (gnatcheck, Exempt_Off, "Unconstrained_Array_Returns");

   function F_Inner_2 return T is
   begin
      return Create;
   end F_Inner_2;

   function F3 return U_Arr is
   begin
      return (1 .. 10 => 0);
   end F3;

   function F4 return U_Arr is
   begin
      return (1 .. 10 => 0);
   end F4;

   function Factorial1 (N : Natural) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial1 (N - 1);
      end if;
   end Factorial1;

   function Factorial2 (N : Natural) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial2 (N - 1);
      end if;
   end Factorial2;
end P_G;
