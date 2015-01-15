with Do_Nothing;

procedure Test_MCDC (Switch_1, Switch_2 : Boolean) is
begin
   --  Test for missing independent influence pair
   if Switch_1 and then Switch_2 then
      Do_Nothing;
   end if;
end Test_MCDC;
