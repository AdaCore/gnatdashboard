with P_G;

generic
package P_G_G is
   type Arr is array (Integer range <>) of Integer;

   function F1 (I : Integer) return Arr is ((1..10 => 1));

   package Inst is  new P_G (Arr, F1);
end P_G_G;
