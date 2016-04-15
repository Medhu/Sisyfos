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

   function Perform_Attack_Pos (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Attack_Pos, "Perform_Attack_Pos");

   function Perform_Attack_Path
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Attack_Path, "Perform_Attack_Path");

   function Perform_Move_Pos (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Move_Pos, "Perform_Move_Pos");

   function Perform_Move_Path (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Move_Path, "Perform_Move_Path");

   function Perform_Ranged_Attack
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Ranged_Attack, "Perform_Ranged_Attack");

   function Perform_Construction
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Construction, "Perform_Construction");

   function Perform_Demolition (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Demolition, "Perform_Demolition");

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

   function Perform_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Patch_Effect, "Perform_Patch_Effect");

   function Perform_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Perform_Piece_Effect, "Perform_Piece_Effect");

   function End_Turn (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, End_Turn, "End_Turn");

   function Observation_Area (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Observation_Area, "Observation_Area");

   function Movement_Capability
     (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Movement_Capability, "Movement_Capability");

   function Attack_Capability (P_Lua_State : in Lua.Lua_State) return Integer;
   pragma Export (C, Attack_Capability, "Attack_Capability");

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
