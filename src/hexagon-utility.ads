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

with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Player;
with Status;
with Piece;

package Hexagon.Utility is

   type Type_Path_Node is record
      Position : Hexagon.Type_Hexagon_Position;
      F, G, H  : Integer;
      Parent   : Hexagon.Type_Hexagon_Position;
   end record;

   function ID_Hashed (id : Hexagon.Type_Hexagon_Position) return Ada.Containers.Hash_Type;

   package Action_Capability_Vector is new Ada.Containers.Hashed_Maps (
      Key_Type        => Hexagon.Type_Hexagon_Position,
      Element_Type    => Type_Path_Node,
      Hash            => ID_Hashed,
      Equivalent_Keys => Hexagon."=");

   function Hexagon_Distance
     (P_From, P_To : in Hexagon.Type_Hexagon_Position)
      return               Integer;

   procedure Find_Path
     (P_Player_Id        : in Player.Type_Player_Id;
      P_Type_Piece_Type  : in Piece.Type_Piece_Type;
      P_From, P_To       : in     Hexagon.Type_Hexagon_Position;
      P_Status           : out Status.Type_Status;
      P_Path             : out Hexagon.Path.Vector);

   procedure Put_Line (P_Text : in Ada.Strings.Unbounded.Unbounded_String);
end Hexagon.Utility;
