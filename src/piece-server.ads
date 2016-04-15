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

with Hexagon.Area.Server_Area;
with Hexagon.Server_Map;
with Status;
with Observation;
with Effect.Server;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Landscape;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Action;

package Piece.Server is
   type Type_Piece is abstract new Piece.Type_Piece with record
      Action_Points    : Integer := 10;
      Effects_On_Piece : Effect.Effect_List.Map;
   end record;

   type Type_Piece_Access is access all Type_Piece;
   type Type_Piece_Access_Class is access all Type_Piece'Class;

   type Type_Piece_Position is record
      Actual_Piece : Type_Piece_Access_Class;
      Actual_Pos   : Hexagon.Type_Hexagon_Position;
   end record;

   function Generate_Piece_Id return Type_Piece_Id;

   function Observation_Area
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Server_Area
       .Type_Action_Capabilities_Access is abstract;

   function Create_Piece_Area
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Server_Area
       .Type_Action_Capabilities_Access_A is abstract;

   procedure Init_Piece (P_Piece_Class : in out Type_Piece) is abstract;

   package Pieces_Server_List is new Ada.Containers.Vectors
     (Natural,
      Type_Piece_Position,
      Piece.Server."=");

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id) return Type_Piece_Position;

   function Left_Less_Pieces
     (Left, Right : in Type_Piece_Access_Class) return Boolean;

   function Equal_Pieces
     (Left, Right : in Type_Piece_Access_Class) return Boolean;

   type Type_Pieces_Report is record
      Observed_Patches : Observation.Observation_Of_Patches
        .Observations_Of_Patches
        .Set;
      Observed_Pieces : Observation.Observation_Of_Pieces
        .Observations_Of_Pieces
        .Set;
      Observed_Pieces_Info : Observation.Observation_Of_Pieces_Info
        .Observations_Of_Pieces_Info
        .Set;
      Observed_Pieces_Effects : Observation.Observation_Of_Pieces_Effects
        .Observations_Of_Pieces_Effects
        .Set;
      Observed_Patches_Effects : Observation.Observation_Of_Patches_Effects
        .Observations_Of_Patches_Effects
        .Set;
      Observed_Constructions : Observation.Observation_Of_Construction
        .Observations_Of_Construction
        .Set;
   end record;

   function Count_Positions return Integer;
   procedure Reset_Positions;
   procedure Set_Position
     (P_Piece : in Type_Piece;
      P_Pos   : in Hexagon.Type_Hexagon_Position);
   procedure Unset_Position
     (P_Piece : in Type_Piece;
      P_Pos   : in Hexagon.Type_Hexagon_Position);

   function Create_Piece_Name
     (P_Piece : in Piece.Server.Type_Piece)
      return Utilities.RemoteString.Type_String is abstract;

   function Validate_Create_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure After_Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in out Piece.Server.Type_Piece;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) is abstract;

   procedure Create_Piece
     (P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in     Piece.Type_Piece;
      P_Server_Piece :    out Piece.Server.Type_Piece_Access_Class;
      P_Status       :    out Status.Type_Status);

   -- This procedure is called during creation of pieces so client code
   -- is able to recalculate resources available. If not enough resources are
   -- available, then the P_Status is returned as False.
   procedure Spend_Resources_On_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Success     :    out Boolean) is abstract;

   procedure Validate_Create_Piece_Area
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece'Class;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Status      :    out Status.Type_Status);

   function Validate_Target_Patch_And_Piece
     (P_Patch     : in Landscape.Type_Patch;
      P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Server.Type_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean;

   procedure Put_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Type_Piece;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id) is abstract;

   function Validate_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Server.Type_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Remove_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Type_Piece;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Before_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_From_Patch  : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id) is abstract;

   function Validate_Grant_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Grant_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure After_Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Piece.Server.Type_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) is abstract;

   function Validate_Revoke_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Revoke_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure After_Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Piece.Server.Type_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) is abstract;

   function Validate_Grant_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Grant_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Patch       : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure After_Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Piece.Server.Type_Piece;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) is abstract;

   function Validate_Revoke_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   procedure Revoke_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Patch       : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure After_Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Piece.Server.Type_Piece;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) is abstract;

   function Validate_Perform_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Perform_Patch_Effect_Piece
     (P_Piece     : in Type_Piece'Class;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece'Class;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean;

   procedure Calculate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) is abstract;

   function Calculate_Patch_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) return Integer is abstract;

   function Validate_Perform_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean is abstract;

   function Validate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece'Class;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Calculate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) is abstract;

   function Calculate_Piece_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) return Integer is abstract;

   procedure Print_Pieces_In_Game;
   procedure Put_All_Pieces_Positions (P_Player_Id : in Player.Type_Player_Id);
   procedure Put (P_Piece : in Type_Piece);

   function Get_Pieces_Players
     (P_Patch : in Landscape.Type_Patch)
      return Player.Type_Player_Id;

   function Patch_Belongs_To_Player
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Is_Piece_Here
     (P_Patch : in Landscape.Type_Patch;
      P_Piece : in Piece.Type_Piece) return Boolean;
   function Is_Type_Of_Piece_Here
     (P_Patch         : in Hexagon.Server_Map.Type_Server_Patch;
      P_Type_Of_Piece :    Piece.Type_Piece_Type) return Boolean;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String;

   function Find_Slot_Of_Pieces
     (P_Patch : in Landscape.Type_Patch;
      P_Piece : in Piece.Type_Piece) return Positive;

   procedure Get_Pieces_Report
     (P_Player_Id     : in     Player.Type_Player_Id;
      P_Pieces_Report :    out Type_Pieces_Report);

   procedure Upkeep_All
     (P_Current_Player_Id : in Player.Type_Player_Id;
      P_Player_Id         : in Player.Type_Player_Id);

   procedure Save_Pieces
     (P_File_Name  : in Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List : in Pieces_Server_List.Vector);

   procedure Load_Pieces
     (P_File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List :    out Pieces_Server_List.Vector);

   All_Pieces_In_Game : Pieces_Server_List.Vector;

   Type_Of_Piece_Missing_Implementation : exception;
   Init_Not_Undefined_Piece_Id : exception;
   Init_Not_Undefined_Piece_Type : exception;
   Init_Not_Undefined_Player_Id : exception;
   Piece_Not_Found_Piece_Position : exception;
   Winner_Not_Returned_Properly : exception;

end Piece.Server;
