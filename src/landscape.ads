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

with Piece;
with Hexagon;
with Ada.Streams; use Ada.Streams;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Construction;
with Effect;

package Landscape is
   pragma Remote_Types;

   type Type_Landscape is new Positive;
   Undefined_Landscape : constant Type_Landscape := 9999999;

   type Type_List_Landscape is array (Landscape.Type_Landscape range <>) of Boolean;

   package Pieces_Here_List is new Ada.Containers.Vectors
     (Positive,
      Piece.Type_Piece_Id,
      Piece."=");
   package Pieces_Here_Sort is new Pieces_Here_List.Generic_Sorting (Piece.Piece_Left_Less);

   type Type_Land is tagged record
      Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      Landscape_Here : Landscape.Type_Landscape;
   end record;

   type Type_Patch (P_Known : Boolean) is new Type_Land with record
      case P_Known is
         when True =>
            Pieces_Here        : Pieces_Here_List.Vector;
            Constructions_Here : Construction.Construction_List.Set;
            Effects_Here       : Effect.Effect_List.Map;
         when False =>
            null;
      end case;
   end record;

   type Type_Map is array (1 .. 100, 1 .. 100) of Type_Land;

   procedure Put (P_Patch : in Type_Patch);
   procedure Put_Pieces_Here (P_Pieces_Here : in Pieces_Here_List.Vector);

   procedure Write_Patch (Stream : access Root_Stream_Type'Class; Item : in Type_Patch);
   for Type_Patch'Write use Write_Patch;

   procedure Read_Patch (Stream : access Root_Stream_Type'Class; Item : out Type_Patch);
   for Type_Patch'Read use Read_Patch;

end Landscape;
