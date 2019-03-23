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

with Hexagon.Client_Map;
with Piece;
with Player;
with Piece.Server;
with Observation;

package Tc_Get_Pieces_Report.Test_Assistant is
   procedure Clear_Pieces_Report (P_Pieces_Report : in out Piece.Server.Type_Pieces_Report);
   procedure Update_From_Server
     (P_Player_Id  : in     Player.Type_Player_Id;
      P_Player_Map : in out Hexagon.Client_Map.Type_Client_Map_Info);
   function Verify_Observed_Patches
     (P_Client_Map   : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Observations : in Piece.Server.Type_Pieces_Report) return Boolean;
   function Verify_Observed_Pieces
     (P_Client_Map   : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Observations : in Piece.Server.Type_Pieces_Report) return Boolean;

end Tc_Get_Pieces_Report.Test_Assistant;
