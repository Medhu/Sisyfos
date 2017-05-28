--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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

package Game_RPC is
   type Type_RPC is (Start_Start, Start_End,
                     Stop_Start, Stop_End,
                     Get_Server_Info_Start, Get_Server_Info_End,
                     Create_Game_Start, Create_Game_End,
                     Save_Game_Start, Save_Game_End,
                     Load_Game_Start, Load_Game_End,
                     Create_Piece_Start, Create_Piece_End,
                     Put_Piece_Start, Put_Piece_End,
                     Remove_Piece_Start, Remove_Piece_End,
                     Get_Pieces_Report_Start, Get_Pieces_Report_End,
                     Perform_Attack_Start, Perform_Attack_End,
                     Perform_Move_Start, Perform_Move_End,
                     Perform_Patch_Effect_Start, Perform_Patch_Effect_End,
                     Perform_Piece_Effect_Start, Perform_Piece_Effect_End,
                     Perform_Construction_Start, Perform_Construction_End,
                     Perform_Demolition_Start, Perform_Demolition_End,
                     Perform_Ranged_Attack_Start, Perform_Ranged_Attack_End,
                     Grant_Piece_Effect_Start, Grant_Piece_Effect_End,
                     Revoke_Piece_Effect_Start, Revoke_Piece_Effect_End,
                     Grant_Patch_Effect_Start, Grant_Patch_Effect_End,
                     Revoke_Patch_Effect_Start, Revoke_Patch_Effect_End,
                     Get_Map_Start, Get_Map_End,
                     Join_Game_Start, Join_Game_End,
                     Leave_Game_Start, Leave_Game_End,
                     Get_Player_Name_Start, Get_Player_Name_End,
                     Get_Updates_Summary_Start, Get_Updates_Summary_End,
                     Client_Stopped_Start, Client_Stopped_End);

end Game_RPC;
