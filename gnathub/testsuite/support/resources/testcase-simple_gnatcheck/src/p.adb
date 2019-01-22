package body P is

   function F_Inner (I : Integer) return Arr is
   begin
      return (1 .. I => I);
   end F_Inner;

   function F1 (I : Integer) return Arr is
   begin
      return F_Inner (I);
   end F1;

   function F2 (I : Integer) return Arr is
   begin
      return F1 (I);
   end F2;
end P;
