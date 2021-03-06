--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2019  Frank J Jorgensen
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
with TS_Hexagon_Client_Map;

-------------
-- Harness --
-------------

procedure Harness5 is

   procedure Run_Hexagon_Client_Map is new AUnit.Run.Test_Runner (TS_Hexagon_Client_Map.Suite);
   Reporter7 : AUnit.Reporter.Text.Text_Reporter;

begin

   Run_Hexagon_Client_Map (Reporter7);

end Harness5;
