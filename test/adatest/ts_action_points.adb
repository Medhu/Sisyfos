with AUnit.Test_Suites; use AUnit.Test_Suites;

with Tc_Action_Points;

package body Ts_Action_Points is

   Result : aliased Test_Suite;
   Test_1 : aliased Tc_Action_Points.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Ts_Action_Points;
