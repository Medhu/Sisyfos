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

with Utilities;
with Player;

package Piece is

   pragma Remote_Types;

   type Type_Piece_Type is new Positive;
   Undefined_Piece_Type : constant Type_Piece_Type := 9999999;

   type Type_Piece_Id is new Positive;
   Undefined_Piece_Id : constant Type_Piece_Id := 9999999;

   type Type_Category is (Fighting_Piece, House_Piece);

   type Type_Piece is tagged record
      Id            : Type_Piece_Id;
      Type_Of_Piece : Type_Piece_Type;
      Category      : Type_Category;
      Name          : Utilities.RemoteString.Type_String;
      Player_Id     : Player.Type_Player_Id; --:= 0; -- which player owns this piece.
   end record;

   function Get_Type_Of_Piece_Description
     (P_Piece : in Type_Piece)
      return    Utilities.RemoteString.Type_String;
   function Get_Name (P_Piece : in Type_Piece) return Utilities.RemoteString.Type_String;
   procedure Set_Name
     (P_Piece : in out Type_Piece;
      P_Name  : in Utilities.RemoteString.Type_String);

   function Piece_Left_Less (Left, Right : in Type_Piece_Id) return Boolean;

   procedure Put (P_Piece : in Type_Piece);
   function Left_Less_Pieces (Left, Right : in Type_Piece) return Boolean;
   function Equal_Pieces (Left, Right : in Type_Piece) return Boolean;

end Piece;
