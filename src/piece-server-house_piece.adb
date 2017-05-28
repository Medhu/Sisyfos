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

with Text_IO;

package body Piece.Server.House_Piece is

   Verbose              : constant Boolean := False;
   House_Type_Info_List : Type_House_Type_Info_List_Access;

   procedure Init
     (P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
      P_House_Info        : in Type_House_Type_Info_List)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.House_Piece.Init - enter");
      end if;

      if P_House_Piece_Class.Id /= Piece.Undefined_Piece_Id then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_House_Piece_Class.Type_Of_Piece /= Piece.Undefined_Piece_Type then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_House_Piece_Class.Player_Id /= Player.Undefined_Player_Id then
         raise Init_Not_Undefined_Player_Id;
      end if;

      House_Class := new Piece.Server.Type_Piece'Class'(P_House_Piece_Class);

      House_Type_Info_List := new Type_House_Type_Info_List'(P_House_Info);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.House_Piece.Init - exit");
      end if;
   end Init;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String
   is
   begin
      return House_Type_Info_List.all (P_Piece.Type_Of_Piece).Type_Name;
   end Get_Type_Of_Piece_Name;

   function Can_Construct_On_Land
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean
   is
   begin
      return Piece.Server.House_Piece.House_Type_Info_List (P_Type_Of_Piece)
          .Construct_Landscape
          (P_Landscape);
   end Can_Construct_On_Land;

   function Validate_Exisiting_Construction
     (P_Patch        : in Landscape.Type_Patch;
      P_Construction : in Construction.Type_Construction) return Boolean
   is
      Ret : Boolean := False;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Exisiting_Construction - enter");
      end if;

      if Construction.Construction_List.Has_Element
          (Construction.Construction_List.Find
             (P_Patch.Constructions_Here,
              P_Construction))
      then
         Ret := True;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Exisiting_Construction - exit, status=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Exisiting_Construction;

end Piece.Server.House_Piece;
