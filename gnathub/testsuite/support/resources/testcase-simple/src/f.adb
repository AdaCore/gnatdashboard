with Ada.Text_IO; use Ada.Text_IO;

package body F is

   ---------
   -- Foo --
   ---------

   procedure Foo (First_Path : Boolean := False) is
      Uninitialized : Integer;
   begin
      if First_Path then
         Put_Line ("First path taken");
      else
         Put_Line ("Second path taken");
      end if;

      Put_Line (Integer'Image (Uninitialized));
   end Foo;

end F;
