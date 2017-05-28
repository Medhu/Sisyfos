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

with Hexagon;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Piece;
with Player;
with Utilities;
with Effect;
with Construction;

package Observation is
   pragma Remote_Types;

   package Observation_Of_Patches is
      type Type_Observed_Patch is record
         A, B    : Hexagon.Type_Hexagon_Numbers;
         Visible : Boolean;
      end record;

      function Left_Less_Observation (Left, Right : in Type_Observed_Patch) return Boolean;

      function Equal_Observation (Left, Right : in Type_Observed_Patch) return Boolean;

      package Observations_Of_Patches is new Ada.Containers.Ordered_Sets
        (Type_Observed_Patch,
         Left_Less_Observation,
         Equal_Observation);

      package Changes_To_Patches is new Ada.Containers.Vectors (Positive, Type_Observed_Patch);

      procedure Print_Pieces_Observed_Patches (P_Observed_Patches : in Observations_Of_Patches.Set);

      procedure Find_Delta_Observations
        (P_Current, P_Previous : in     Observations_Of_Patches.Set;
         P_Observations        :    out Changes_To_Patches.Vector);

   end Observation_Of_Patches;

   package Observation_Of_Pieces_Info is
      type Type_Observed_Piece_Info is record
         Piece_Here : Piece.Type_Piece;
         Valid      : Boolean;
      end record;

      function Left_Less_Observed_Pieces_Info
        (Left, Right : in Type_Observed_Piece_Info) return Boolean;

      function Equal_Observed_Pieces_Info
        (Left, Right : in Type_Observed_Piece_Info) return Boolean;

      package Observations_Of_Pieces_Info is new Ada.Containers.Ordered_Sets
        (Type_Observed_Piece_Info,
         Left_Less_Observed_Pieces_Info,
         Equal_Observed_Pieces_Info);

      package Changes_To_Pieces_Info is new Ada.Containers.Vectors
        (Positive,
         Type_Observed_Piece_Info,
         Equal_Observed_Pieces_Info);
      procedure Print_Pieces_Observed_Pieces_Info
        (P_Observed_Pieces_Info : in Observations_Of_Pieces_Info.Set);

      function Find_Piece_Id
        (P_Observed_Pieces_Info : in Observations_Of_Pieces_Info.Set;
         P_Piece_Id             :    Piece.Type_Piece_Id) return Boolean;

      procedure Find_Delta_Observed_Pieces_Info
        (P_Current, P_Previous  : in     Observations_Of_Pieces_Info.Set;
         P_Observed_Pieces_Info :    out Changes_To_Pieces_Info.Vector);

   end Observation_Of_Pieces_Info;

   package Observation_Of_Pieces is
      type Type_Observed_Piece is record
         Pos           : Hexagon.Type_Hexagon_Position;
         Piece_Here_Id : Piece.Type_Piece_Id;
      end record;

      function Left_Less_Observed_Pieces (Left, Right : in Type_Observed_Piece) return Boolean;

      function Equal_Observed_Pieces (Left, Right : in Type_Observed_Piece) return Boolean;

      package Observations_Of_Pieces is new Ada.Containers.Ordered_Sets
        (Type_Observed_Piece,
         Left_Less_Observed_Pieces,
         Equal_Observed_Pieces);

      package Changes_To_Pieces is new Ada.Containers.Vectors (Positive, Type_Observed_Piece);

      procedure Print_Pieces_Observed_Pieces (P_Observed_Pieces : in Observations_Of_Pieces.Set);

      function Find_Piece_Id
        (P_Observed_Pieces : in Observations_Of_Pieces.Set;
         P_Piece_Id        :    Piece.Type_Piece_Id) return Boolean;

      procedure Find_Delta_Observed_Pieces
        (P_Current, P_Previous : in     Observations_Of_Pieces.Set;
         P_Observed_Pieces     :    out Changes_To_Pieces.Vector);

   end Observation_Of_Pieces;

   package Observation_Of_Pieces_Effects is
      type Type_Piece_Effect is record
         Piece_Id : Piece.Type_Piece_Id;    -- identifies Type_Piece.Id to connect this
         --information to correct piece.
         Effect_Info : Effect.Type_Effect;
         Valid       : Boolean;
      end record;

      function Left_Less_Effect (Left, Right : in Type_Piece_Effect) return Boolean;

      function Equal_Effect (Left, Right : in Type_Piece_Effect) return Boolean;

      package Observations_Of_Pieces_Effects is new Ada.Containers.Ordered_Sets
        (Type_Piece_Effect,
         Left_Less_Effect,
         Equal_Effect);

      package Changes_To_Pieces_Effects is new Ada.Containers.Vectors (Positive, Type_Piece_Effect);

      procedure Find_Delta_Effect
        (P_Current, P_Previous : in     Observations_Of_Pieces_Effects.Set;
         P_Effects             :    out Changes_To_Pieces_Effects.Vector);

      procedure Print_Pieces_Effect (P_Player_Pieces : in Observations_Of_Pieces_Effects.Set);

   end Observation_Of_Pieces_Effects;

   package Observation_Of_Patches_Effects is
      type Type_Patch_Effect is record
         Pos         : Hexagon.Type_Hexagon_Position;
         Effect_Info : Effect.Type_Effect;
         Valid       : Boolean;
      end record;

      function Left_Less_Effect (Left, Right : in Type_Patch_Effect) return Boolean;

      function Equal_Effect (Left, Right : in Type_Patch_Effect) return Boolean;

      package Observations_Of_Patches_Effects is new Ada.Containers.Ordered_Sets
        (Type_Patch_Effect,
         Left_Less_Effect,
         Equal_Effect);

      package Changes_To_Patches_Effects is new Ada.Containers.Vectors
        (Positive,
         Type_Patch_Effect);

      procedure Find_Delta_Effect
        (P_Current, P_Previous : in     Observations_Of_Patches_Effects.Set;
         P_Effects             :    out Changes_To_Patches_Effects.Vector);

      procedure Print_Patches_Effect (P_Player_Patches : in Observations_Of_Patches_Effects.Set);

   end Observation_Of_Patches_Effects;

   package Observation_Of_Construction is
      type Type_Patch_Construction is record
         Pos               : Hexagon.Type_Hexagon_Position;
         Construction_Info : Construction.Type_Construction;
         Valid             : Boolean;
      end record;

      function Left_Less_Construction (Left, Right : in Type_Patch_Construction) return Boolean;

      function Equal_Construction (Left, Right : in Type_Patch_Construction) return Boolean;

      package Observations_Of_Construction is new Ada.Containers.Ordered_Sets
        (Type_Patch_Construction,
         Left_Less_Construction,
         Equal_Construction);

      package Changes_To_Construction is new Ada.Containers.Vectors
        (Positive,
         Type_Patch_Construction);

      procedure Find_Delta_Construction
        (P_Current, P_Previous : in     Observations_Of_Construction.Set;
         P_Construction        :    out Changes_To_Construction.Vector);

      procedure Print_Patches_Construction (P_Player_Patches : in Observations_Of_Construction.Set);

   end Observation_Of_Construction;

   package Activity is
      Internal_Details : constant Positive := 1;

      type Type_Activity_Report is record
         Player_Id            : Player.Type_Player_Id;
         Activity_Description : Utilities.RemoteString.Type_String;
      end record;

      function Equal (Left, Right : in Type_Activity_Report) return Boolean;

      package Activity_Report is new Ada.Containers.Vectors (Positive, Type_Activity_Report, Equal);

   end Activity;

   package Frames is
      type Type_Visibility_Frames is record
         Observed_Patches     : Observation_Of_Patches.Changes_To_Patches.Vector;
         Observed_Pieces      : Observation_Of_Pieces.Changes_To_Pieces.Vector;
         Pieces_Info          : Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Vector;
         Pieces_Effects_Info  : Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Vector;
         Patches_Effects_Info : Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Vector;
         Constructions_Info   : Observation_Of_Construction.Changes_To_Construction.Vector;
         Activities_Info      : Activity.Activity_Report.Vector;
      end record;

      package Piece_Visibility_Frames is new Ada.Containers.Vectors
        (Positive,
         Type_Visibility_Frames);

      procedure Clear_Frames (P_Piece_Visibility_Frames : in out Piece_Visibility_Frames.Vector);
   end Frames;

end Observation;
