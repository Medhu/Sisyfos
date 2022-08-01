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

with Landscape;
with Landscape.Server;

package Piece.Server.Fighting_Piece is

   type Type_Piece is abstract new Piece.Server.Type_Piece with null record;
   type Type_Piece_Access is access all Type_Piece;
   type Type_Piece_Access_Class is access all Type_Piece'Class;

   type Type_Piece_Type_Info is record
      Type_Name      : Utilities.RemoteString.Type_String;
      Category       : Type_Category;
      Move_Landscape : Landscape.Server
        .Type_List_Landscape_Access; -- on what landscape can this piece move
      Attack_Landscape : Landscape.Server
        .Type_List_Landscape_Access; -- on what landscape can this piece attack
   end record;

   Piece_Class : Piece.Server.Type_Piece_Access_Class;

   type Type_Piece_Type_Info_List is
     array (Piece.Type_Piece_Type range <>) of Type_Piece_Type_Info;
   type Type_Piece_Type_Info_List_Access is
     access all Type_Piece_Type_Info_List;

   function Can_Attack_Here
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean;

   function Can_Move_Here
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean;

   procedure Init
     (P_Fighting_Piece_Class : in Piece.Server.Type_Piece'Class;
      P_Piece_Info           : in Type_Piece_Type_Info_List);

   --
   -- Perform Attack
   --
   procedure Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece         : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Winner                 : in     Player.Type_Player_Id);

   function Validate_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece : in Piece.Server.Fighting_Piece.Type_Piece)
      return Boolean is abstract;

   procedure Before_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status) is abstract;

   procedure Calculate_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id) is abstract;

   procedure End_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Perform Ranged Attack
   --
   procedure Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece         : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Winner                 : in     Player.Type_Player_Id);

   function Validate_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece : in Piece.Server.Fighting_Piece.Type_Piece)
      return Boolean is abstract;

   procedure Before_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status) is abstract;

   procedure Calculate_Ranged_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id) is abstract;

   procedure End_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Perform_Move
   --
   function Validate_Perform_Move
     (P_Player_Id    : in Player.Type_Player_Id;
      P_Action_Type  : in Action.Type_Action_Type;
      P_Moving_Piece : in Piece.Server.Fighting_Piece.Type_Piece;
      P_End_Pos : in Hexagon.Type_Hexagon_Position) return Boolean is abstract;

   procedure Before_Perform_Move
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status) is abstract;

   procedure Before_Perform_Move_Step
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status) is abstract;

   procedure End_Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece       : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_End_Pos            : in     Hexagon.Type_Hexagon_Position;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   procedure Perform_Move_Step
     (P_Player_Id              : in     Player.Type_Player_Id;
      P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch);

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece : in out Type_Piece) is abstract;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String;

   function Movement_Cost
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Fighting_Piece.Type_Piece;
      P_From_Patch  : in out Landscape.Type_Patch;
      P_To_Patch    : in out Landscape.Type_Patch) return Integer is abstract;

end Piece.Server.Fighting_Piece;
