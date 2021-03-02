--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2021  Frank J Jorgensen
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
with Player;
with Hexagon;
with Piece;
with Effect;
with Status;
with Utilities;
with Piece.Server;
with Landscape;
with Action;
with Server.Server;
with Landscape.Server;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Effect.Server;
with Hexagon.Area;

package Server.ServerAPI is

   type Type_Piece_Position is record
      Actual_Piece : Piece.Type_Piece;
      Actual_Pos   : Hexagon.Type_Hexagon_Position;
   end record;

   procedure Get_Server_Info (P_Server_Info : out Utilities.RemoteString_List.Vector);

   procedure Set_Server_Info (P_Server_Info : in Utilities.RemoteString_List.Vector);

   procedure Init (P_Fighting_Piece_Class, P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
   --
      P_Landscape_Info : in Landscape.Server.Type_Landscape_Type_Info_List;
      P_Piece_Info     : in Piece.Server.Fighting_Piece.Type_Piece_Type_Info_List;
      P_House_Info     : in Piece.Server.House_Piece.Type_House_Type_Info_List;
      P_Effect_Info    : in Effect.Server.Type_Effect_Type_Info_List;
   --
      P_Game_Creating, P_Game_Saving, P_Game_Loading : in Server.Type_Game_Archive_Procedure;
      P_Game_Joining, P_Game_Leaving : in Server.Type_Game_Joining_Leaving_Procedure;
      P_Game_Start                                   : in Server.Type_Game_Start_Procedure;
      P_Game_Upkeep                                  : in Server.Type_Game_Upkeep_Procedure;
      P_Game_End                                     : in Server.Type_Game_End_Procedure);
   procedure Start;
   procedure Run;
   procedure Stop;

   procedure Observe_Game_Minimum_Details (P_Minimum_Details : in Positive);

   procedure Observe_Game (P_Detail : in Positive);

   procedure Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Piece.Type_Piece; P_Piece_Id : out Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status; P_Force : in Boolean := False);

   procedure Put_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id                    : in Piece.Type_Piece_Id; P_Status : out Status.Type_Status);

   procedure Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
 P_Status                              :    out Status.Type_Status);

   procedure Perform_Attack (P_Player_Id        : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Ranged_Attack (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Move (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Status : out Status.Type_Status);

   procedure Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Status                                 :    out Status.Type_Status);

   procedure Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Pos : in     Hexagon.Type_Hexagon_Position; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Status                                 :    out Status.Type_Status);

   procedure Grant_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect : in Effect.Type_Effect; P_Status : out Status.Type_Status);

   procedure Revoke_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name; P_Status : out Status.Type_Status);

   procedure Grant_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Pos : in Hexagon.Type_Hexagon_Position; P_Effect : in Effect.Type_Effect;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A; P_Status : out Status.Type_Status);

   procedure Revoke_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Area                                  : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name; P_Status : out Status.Type_Status);

   function Is_Effect_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                            : in Piece.Type_Piece_Id;
      P_Effect_Name                         : in Effect.Type_Effect_Name) return Boolean;

   function Get_Effect_Aux_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                                 : in Piece.Type_Piece_Id;
      P_Effect_Name                              : in Effect.Type_Effect_Name) return Natural;

   function Find_Piece_In_List (P_Piece_Id : in Piece.Type_Piece_Id) return Type_Piece_Position;

   procedure Opponents_Activity_Report_Append
     (P_Detail               : Positive;
      P_Player_Id : Player.Type_Player_Id; -- Opponents of this player should get this report.
      P_Activity_Description : Utilities.RemoteString.Type_String);

   procedure Player_Activity_Report_Append (P_Detail : Positive;
      P_Player_Id                                    : Player.Type_Player_Id;
      P_Activity_Description                         : Utilities.RemoteString.Type_String);

   procedure Opponents_System_Report_Append (P_Detail : Positive;
      P_Player_Id                                     : Player.Type_Player_Id;
      P_Activity_Description                          : Utilities.RemoteString.Type_String);

   procedure Player_System_Report_Append (P_Detail : Positive; P_Player_Id : Player.Type_Player_Id;
      P_Activity_Description                       : Utilities.RemoteString.Type_String);

   function Get_Map_Terrain
     (P_Pos : in Hexagon.Type_Hexagon_Position) return Landscape.Type_Landscape;

   function Get_Map_Pieces_List
     (P_Pos : in Hexagon.Type_Hexagon_Position) return Landscape.Pieces_Here_List.Vector;

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id) return Utilities.RemoteString.Type_String;

   function Is_Player_In_Scenario (P_Player_Id : in Player.Type_Player_Id) return Boolean;

end Server.ServerAPI;
