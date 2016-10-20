--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2016  Frank J Jorgensen
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

with AUnit.Run;
with AUnit.Reporter.Text;
with Ts_Hexagon_Utility;
with Ts_Landscape;

-------------
-- Harness --
-------------

procedure Harness3 is

   procedure Run_Hexagon_Utility is new AUnit.Run.Test_Runner (Ts_Hexagon_Utility.Suite);
   Reporter6 : AUnit.Reporter.Text.Text_Reporter;
   --
   procedure Run_Landscape is new AUnit.Run.Test_Runner (Ts_Landscape.Suite);
   Reporter7 : AUnit.Reporter.Text.Text_Reporter;

begin

   Run_Hexagon_Utility (Reporter6);
   Run_Landscape (Reporter7);

end Harness3;
