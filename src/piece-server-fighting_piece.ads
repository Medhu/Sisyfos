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

   procedure Build_Attack_Steps
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece'Class;
      P_Attacking_Patch, P_Attacked_Patch : in     Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status;
      P_Path                              :    out Hexagon.Path.Vector);

   procedure Calculate_Attack_Result
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner :    out Player.Type_Player_Id) is abstract;

   function Calculate_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Integer is abstract;

   procedure After_Perform_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Winner                            : in     Player.Type_Player_Id;
      P_Player_Id : in     Player.Type_Player_Id) is abstract;

   function Calculate_Ranged_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Integer is abstract;

   procedure After_Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Winner                            : in     Player.Type_Player_Id;
      P_Player_Id : in     Player.Type_Player_Id) is abstract;

   procedure Perform_Attack_Step
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            : in     Player.Type_Player_Id);

   procedure Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            : in     Player.Type_Player_Id);

   function Validate_Moving_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Reachable
     (P_Moving_Piece : in Type_Piece'Class) return Boolean;

   function Validate_Move
     (P_Moving_Piece           : in Type_Piece'Class;
      P_From_Patch, P_To_Patch : in Landscape.Type_Patch) return Boolean;

   function Validate_Attackable
     (P_Moving_Piece : in Type_Piece'Class) return Boolean;

   function Validate_Attack
     (P_Attacking_Piece : in Type_Piece'Class;
      P_Attacking_Patch,
      P_Attacked_Patch : in Landscape.Type_Patch)
      return Boolean;

   function Validate_Ranged_Attack
     (P_Attacking_Piece : in Type_Piece'Class;
      P_Attacking_Patch,
      P_Attacked_Patch : in Landscape.Type_Patch)
      return Boolean;

   function Validate_Executing_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Target_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Target_Patch_Occupied
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Build_Move_Steps
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in     Type_Piece'Class;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id;
      P_Status                 :    out Status.Type_Status;
      P_Path                   :    out Hexagon.Path.Vector);

   procedure Pre_Validate_Perform_Move
     (P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece :        Type_Piece;
      P_Start_Patch  : in     Landscape.Type_Patch;
      P_End_Patch    :        Landscape.Type_Patch;
      P_Player_Id    : in     Player.Type_Player_Id;
      P_Status       :    out Status.Type_Status);

   procedure Pre_Validate_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece;
      P_Start_Patch                       : in     Landscape.Type_Patch;
      P_End_Patch                         :        Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status);

   procedure Pre_Validate_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece;
      P_Start_Patch                       : in     Landscape.Type_Patch;
      P_End_Patch                         :        Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status);

   function Calculate_Move_Action_Points
     (P_Action_Type            : in Action.Type_Action_Type;
      P_Moving_Piece           : in Type_Piece;
      P_From_Patch, P_To_Patch : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Integer is abstract;

   procedure After_Perform_Move
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id) is abstract;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_Piece;
      P_Path                              : in Hexagon.Path.Vector;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Perform_Ranged_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Perform_Move
     (P_Action_Type  : in Action.Type_Action_Type;
      P_Moving_Piece : in Type_Piece;
      P_Path         : in Hexagon.Path.Vector;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Perform_Move
     (P_Action_Type        : in Action.Type_Action_Type;
      P_Moving_Piece       : in Type_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Perform_Move_Step
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id);

   function Movement_Capability
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Server_Area
       .Type_Action_Capabilities_Access is abstract;

   function Movement_Terrain_Capability
     (P_Piece : in Type_Piece'Class)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Attack_Capability
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Server_Area
       .Type_Action_Capabilities_Access is abstract;

   function Attack_Terrain_Capability
     (P_Piece : in Type_Piece'Class)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   procedure Upkeep
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece             : in out Type_Piece) is abstract;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String;

end Piece.Server.Fighting_Piece;
