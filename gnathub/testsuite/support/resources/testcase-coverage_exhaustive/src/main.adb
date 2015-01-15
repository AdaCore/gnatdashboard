with Test_Stmt;
with Test_Decision;
with Test_MCDC;

procedure Main is
begin
   Test_Stmt (0);
   Test_Decision (True, False);
   Test_MCDC (True, False);
   Test_MCDC (True, True);
end Main;
