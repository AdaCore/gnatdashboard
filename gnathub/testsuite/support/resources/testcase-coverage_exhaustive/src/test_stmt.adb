with Do_Nothing;

procedure Test_Stmt (Length : Natural) is
begin
   --  Test for covered statement
   Do_Nothing;

   for I in 1 .. Length loop
      --  Test for uncovered statement
      Do_Nothing;
   end loop;

   --  Test for both covered and uncovered statement on the same line
   Do_Nothing; for I in 1 .. Length loop Do_Nothing; end loop;

   --  Test for two uncovered statements on the same line
   for I in 1 .. Length loop
      Do_Nothing; Do_Nothing;
   end loop;
end Test_Stmt;
