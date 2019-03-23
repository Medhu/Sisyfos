--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2019  Frank J Jorgensen
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

with Server.Server.Cmd;

package Server.Server.Piece_Action is
   procedure Execute_Cmds (P_Cmd_List : in out Server.Cmd.Cmd_List_Pkg.Vector);

   procedure New_Piece
     (P_Piece        : in     Piece.Type_Piece;
      P_Piece_Server :    out Piece.Server.Type_Piece_Access_Class);

   procedure Delete_Piece
     (P_Piece_Server : in out Piece.Server.Type_Piece_Access_Class);

   procedure Init_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Piece.Server.Type_Piece_Access_Class;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer;
      P_Force              : in     Boolean := False);

   procedure Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Perform_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status;
      P_Attempts_Remaining                      : in out Integer);

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status;
      P_Attempts_Remaining                      : in out Integer);

   procedure Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_End_Pos             : in     Hexagon.Type_Hexagon_Position;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   procedure Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id           : in     Piece.Type_Piece_Id;
      P_Effect             : in     Effect.Type_Effect;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status             :    out Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

   Attempts_Remaining_Not_Updated : exception;
end Server.Server.Piece_Action;
