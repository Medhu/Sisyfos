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

with Hexagon.Area.Server_Area;
with Hexagon.Server_Map;
with Status;
with Observation;
with Ada.Containers.Vectors;
with Landscape;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Action;
with Ada.Unchecked_Deallocation;
with Effect;

package Piece.Server is
   type Type_Piece is abstract new Piece.Type_Piece with record
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

   --
   -- Create_Piece
   --
   procedure New_Piece
     (P_Piece        : in     Piece.Type_Piece;
      P_Server_Piece :    out Piece.Server.Type_Piece_Access_Class);

   procedure Delete_Piece
     (P_Server_Piece : in out Piece.Server.Type_Piece_Access_Class);

   function Validate_Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Server.Type_Piece) return Boolean is abstract;

   procedure Before_Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Put_Piece
   --
   procedure Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Type_Piece;
      P_Status      :    out Status.Type_Status);

   function Validate_Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Server.Type_Piece) return Boolean is abstract;

   procedure Before_Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Remove_Piece
   --
   procedure Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Status      :    out Status.Type_Status);

   function Validate_Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece : in out Piece.Server.Type_Piece) return Boolean is abstract;

   procedure Before_Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Perform_Patch_Effect
   --
   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect) is abstract;

   function Validate_Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
      return Boolean is abstract;

   procedure Before_Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   function Validate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece'Class;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean;

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Perform_Piece_Effect
   --
   function Validate_Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect) return Boolean is abstract;

   procedure Before_Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   function Validate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece'Class;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect) is abstract;

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Grant_Piece_Effect
   --
   procedure Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Status      :    out Status.Type_Status);

   function Validate_Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect) return Boolean is abstract;

   procedure Before_Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Revoke_Piece_Effect
   --
   procedure Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Status      :    out Status.Type_Status);

   function Validate_Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Effect      : in Effect.Type_Effect) return Boolean is abstract;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Grant_Patch_Effect
   --
   procedure Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status      :    out Status.Type_Status);

   function Validate_Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean is abstract;

   procedure Before_Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Revoke_Patch_Effects
   --
   procedure Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Server.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status      :    out Status.Type_Status);

   function Validate_Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Piece.Server.Type_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean is abstract;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Piece.Server.Type_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status) is abstract;

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Piece.Server.Type_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   procedure Print_Pieces_In_Game;
   procedure Put_All_Pieces_Positions (P_Player_Id : in Player.Type_Player_Id);
   procedure Put (P_Piece : in Type_Piece);

   function Get_Pieces_Players
     (P_Patch : in Landscape.Type_Patch) return Player.Type_Player_Id;

   function Patch_Belongs_To_Player
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Is_Players_Piece
     (P_Piece     : in Type_Piece;
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

   procedure Upkeep_All (P_Player_Id : in Player.Type_Player_Id);

   procedure Save_Pieces
     (P_File_Name  : in Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List : in Pieces_Server_List.Vector);

   procedure Load_Pieces
     (P_File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List :    out Pieces_Server_List.Vector);

   procedure Free_Piece is new Ada.Unchecked_Deallocation
     (Object => Piece.Server.Type_Piece'Class,
      Name   => Piece.Server.Type_Piece_Access_Class);

   All_Pieces_In_Game : Pieces_Server_List.Vector;

   Type_Of_Piece_Missing_Implementation : exception;
   Init_Not_Undefined_Piece_Id : exception;
   Init_Not_Undefined_Player_Id : exception;
   Piece_Not_Found_Piece_Position : exception;
   Winner_Not_Returned_Properly : exception;
   Piece_Id_Not_Init : exception;

end Piece.Server;
