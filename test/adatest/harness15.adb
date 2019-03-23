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
with Ts_Patch_Effects;

-------------
-- Harness --
-------------

procedure Harness15 is

   procedure Run_Game is new AUnit.Run.Test_Runner (Ts_Patch_Effects.Suite);
   Reporter1 : AUnit.Reporter.Text.Text_Reporter;
begin

   Run_Game (Reporter1);

end Harness15;
