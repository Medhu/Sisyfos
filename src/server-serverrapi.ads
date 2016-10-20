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

with Hexagon;
with Piece;
with Player;
with Utilities;
with Landscape;
with Status;
with Observation;
with Effect;
with Construction;
with Action;
with Hexagon.Area;

package Server.ServerRAPI is

   procedure Init
     (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters);
   procedure Start;
   procedure Stop;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status);

   procedure Set_Server_Info
     (P_Server_Info : in     Utilities.RemoteString_List.Vector;
      P_Status      :    out Status.Type_Adm_Status);

   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status);

   procedure Save_Game
     (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status);

   procedure Load_Game
     (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status);

   procedure Join_Game
     (P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status;
      P_Player_Id   :    out Player.Type_Player_Id);

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status);

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status)
      return Utilities.RemoteString.Type_String;

   procedure Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Type_Piece);

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id);

   procedure Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id);

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames
        .Vector);

   procedure Perform_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id);

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id);

   procedure Perform_Move
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_To_Pos      : in Hexagon.Type_Hexagon_Position);

   procedure Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect);

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect);

   procedure Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect);

   procedure Perform_Construction
     (P_Player_Id        : in Player.Type_Player_Id;
      P_Action_Type      : in Action.Type_Action_Type;
      P_Piece_Id         : in Piece.Type_Piece_Id;
      P_Construction_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction     : in Construction.Type_Construction);

   procedure Perform_Demolition
     (P_Player_Id      : in Player.Type_Player_Id;
      P_Action_Type    : in Action.Type_Action_Type;
      P_Piece_Id       : in Piece.Type_Piece_Id;
      P_Demolition_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction   : in Construction.Type_Construction);

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map);

   procedure Get_Updates_Summary
     (P_Player_Id : in Player.Type_Player_Id;  -- The player of this
   --client
      P_Countdown       : out Positive;  -- The countdown of this turn
      P_Game_Status     : out Status.Type_Game_Status;
      P_System_Messages : out Observation.Activity.Activity_Report.Vector);

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id);

end Server.ServerRAPI;
