--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2016  Frank J Jorgensen
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

with Lua;
package Server.Lua_Interface is
   Verbose : constant Boolean := False;

   procedure Init (P_Lua_State : in Lua.Lua_State);

   function Observe_Game_Minimum_Details
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export
     (C,
      Observe_Game_Minimum_Details,
      "Observe_Game_Minimum_Details");

   function Observe_Game (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Observe_Game, "Observe_Game");

   function Create_Piece (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Create_Piece, "Create_Piece");

   function Put_Piece (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Put_Piece, "Put_Piece");

   function Remove_Piece (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Remove_Piece, "Remove_Piece");

   function Perform_Attack (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Attack, "Perform_Attack");

   function Perform_Ranged_Attack
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Ranged_Attack, "Perform_Ranged_Attack");

   function Perform_Move (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Move, "Perform_Move");

   function Perform_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Patch_Effect, "Perform_Patch_Effect");

   function Perform_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Piece_Effect, "Perform_Piece_Effect");

   function Grant_Piece_Effect (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Grant_Piece_Effect, "Grant_Piece_Effect");

   function Revoke_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Revoke_Piece_Effect, "Revoke_Piece_Effect");

   function Grant_Patch_Effect (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Grant_Patch_Effect, "Grant_Patch_Effect");

   function Revoke_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Revoke_Patch_Effect, "Revoke_Patch_Effect");

   function Perform_Construction
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Construction, "Perform_Construction");

   function Perform_Demolition (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Demolition, "Perform_Demolition");

   function Find_Piece_In_List (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Find_Piece_In_List, "Find_Piece_In_List");

   function Opponents_Activity_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export
     (C,
      Opponents_Activity_Report_Append,
      "Opponents_Activity_Report_Append");

   function Player_Activity_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export
     (C,
      Player_Activity_Report_Append,
      "Player_Activity_Report_Append");

   function Opponents_System_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export
     (C,
      Opponents_System_Report_Append,
      "Opponents_System_Report_Append");

   function Player_System_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export
     (C,
      Player_System_Report_Append,
      "Player_System_Report_Append");

   function Get_Player_Name (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export (C, Get_Player_Name, "Get_Player_Name");

   function Get_Map_Terrain (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export (C, Get_Map_Terrain, "Get_Map_Landscape");

   function Get_Map_Construction_List
     (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export (C, Get_Map_Construction_List, "Get_Map_Construction_List");

   function Get_Map_Pieces_List (P_Lua_State : Lua.Lua_State) return Integer;
   pragma Export (C, Get_Map_Pieces_List, "Get_Map_Pieces_List");
end Server.Lua_Interface;
