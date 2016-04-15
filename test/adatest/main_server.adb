--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013  Frank J Jorgensen
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

with Hexagon.Server_Map;
with Piece;
with Server;
with Server.Server;
with Text_IO;
with Test_Piece;

procedure main_server is
   Run         : Boolean := True;
   A_Char      : Character;
   Test_Class1 : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2 : Test_Piece.Type_My_Test_House_Access_Class;

begin
   Text_IO.Put_Line ("Test Game - v0.2");x
   delay 0.0;

   Test_Class1               := new Test_Piece.Type_My_Test_Piece;
   Test_Class1.Id            := Piece.Undefined_Piece_Id;
   Test_Class1.Type_Of_Piece := Piece.Undefined_Piece_Type;
   Test_Class1.Player_Id     := 1;

   Test_Class2               := new Test_Piece.Type_My_Test_House;
   Test_Class2.Id            := Piece.Undefined_Piece_Id;
   Test_Class2.Type_Of_Piece := Piece.Undefined_Piece_Type;
   Test_Class2.Player_Id     := 1;

   Server.Server.Init
     (Test_Class1.all,
      Test_Class2.all,
      Test_Piece.Pieces_Type_Info_List,
      Test_Piece.Houses_Type_Info_List,
      Test_Piece.Test_Start_Game'Access,
      Test_Piece.Test_Upkeep_Game'Access,
      Test_Piece.Test_End_Game'Access);
   Server.Server.Start;

   while Run loop

      Text_IO.Get_Immediate (A_Char);

      if A_Char = '0' then
         Run := False;
         Server.Server.Stop;
      end if;
   end loop;

   delay 100.0;
end main_server;
