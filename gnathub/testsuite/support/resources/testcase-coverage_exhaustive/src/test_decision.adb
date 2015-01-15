with Do_Nothing;

procedure Test_Decision (Switch_1, Switch_2 : Boolean) is
begin
   --  Test for outcome FALSE never exercised
   if Switch_1 then
      Do_Nothing;
   end if;

   --  Test for outcome TRUE never exercised
   if not Switch_1 then
      Do_Nothing;

      --  Test for no outcome exercised
      if Switch_2 then
         Do_Nothing;
      end if;
   end if;

   for I in False .. True loop
      --  Test for both outcome exercised
      if I then
         Do_Nothing;
      end if;
   end loop;
end Test_Decision;
