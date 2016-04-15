--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015  Frank J Jorgensen
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

with Server.Server;

package Server.Server.Auto is
   procedure Check_Opponent_Communications
     (P_Player_List_Internal : in out Server.Type_Player_List);

   procedure Update_Game
     (P_Current_Player_Id    : in     Player.Type_Player_Id;
      P_Last_Update_Time     : in out Ada.Real_Time.Time;
      P_Countdown            : in out Positive;
      P_Player_List_Internal : in out Server.Type_Player_List);

end Server.Server.Auto;
