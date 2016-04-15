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

with Text_IO;

package body Piece is
   Verbose : constant Boolean := False;

   function Piece_Left_Less (Left, Right : in Type_Piece_Id) return Boolean is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Piece_Left_Less - enter - exit");
      end if;

      return Left < Right;
   end Piece_Left_Less;

   function Get_Type_Of_Piece_Description
     (P_Piece : in Type_Piece)
      return    Utilities.RemoteString.Type_String
   is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Get_Type_Of_Piece_Description - enter - exit");
      end if;

      return Utilities.RemoteString.To_Unbounded_String ("Default Type Of Piece");
   end Get_Type_Of_Piece_Description;

   function Get_Name (P_Piece : in Type_Piece) return Utilities.RemoteString.Type_String is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Get_Name - enter - exit");
      end if;

      return P_Piece.Name;
   end Get_Name;

   procedure Set_Name
     (P_Piece : in out Type_Piece;
      P_Name  : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Set_Name - enter - exit");
      end if;

      P_Piece.Name := P_Name;
   end Set_Name;

   function Left_Less_Pieces (Left, Right : in Type_Piece) return Boolean is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Left_Less_Pieces - enter - exit");
      end if;

      return Integer (Left.Id) < Integer (Right.Id);
   end Left_Less_Pieces;

   function Equal_Pieces (Left, Right : in Type_Piece) return Boolean is
   begin
      if Verbose then
         Text_IO.Put_Line("Piece.Equal_Pieces - enter - exit");
      end if;

      return Left = Right;
   end Equal_Pieces;

   procedure Put (P_Piece : in Type_Piece) is
   begin
      Text_IO.Put_Line
        ("Piece.Id=" &
         P_Piece.Id'Img &
         " P_Piece.Type_Of_Piece=" &
         P_Piece.Type_Of_Piece'Img &
         " Piece.PLayer_Id=" &
         P_Piece.Player_Id'Img);
   end Put;

end Piece;
