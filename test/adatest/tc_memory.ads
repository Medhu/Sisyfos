--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013  Frank J Jorgensen
--
--      This program is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 3 of the License, or
--      (at your option) any later version.
--
--      This program is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with AUnit.Test_Cases; use AUnit;

package Tc_Memory is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Set_Up (T : in out Test_Case);
   --  Preparation performed before each routine

   procedure Tear_Down (T : in out Test_Case);
   --  Cleanup performed after each routine

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case) return Test_String;
   --  Returns name identifying the test case

end Tc_Memory;
