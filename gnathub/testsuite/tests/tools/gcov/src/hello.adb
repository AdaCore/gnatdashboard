with Ada.Text_IO; use Ada.Text_IO;
with F; use F;

procedure hello is
  Base : Integer;

  procedure Bar is
  begin
     for J in 1 .. 42 loop
         for K in 1 .. 42 loop
            if J * K > 200 then
               return;
            end if;
         end loop;
     end loop;
  end Bar;

begin
   Base := 42;
   Bar;
   F.Foo;
end;
