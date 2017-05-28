--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2017  Frank J Jorgensen
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

-- gnatmake -I..\src\landscape -I..\src\map -I..\src\piece -I..\src\utilities -I..\src\player
--create_a_map.adb

with Ada.Streams;
with Hexagon;
with Landscape;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Text_IO;   -- defines Current_Output. See RM A.10.1

procedure Create_A_Map is
   A_Landscape : Landscape.Type_Landscape;
   B_Landscape : Landscape.Type_Landscape;
   t           : Ada.Streams.Stream_IO.File_Type;

   Ut_Stream, Inn_Stream : Stream_Access;
--A, B : Integer;
begin

   A_Landscape := Landscape.Mountain;

   Create (t, Ada.Streams.Stream_IO.Out_File, "test.dat");
   Ut_Stream := Ada.Streams.Stream_IO.Stream (t);

   for Trav_X in 1 .. 100 loop
      for Trav_Y in 1 .. 100 loop

         case (Trav_X * Trav_Y) is
            when 0 .. 14 =>
               A_Landscape := Landscape.Grass;
            when 15 .. 30 =>
               A_Landscape := Landscape.Forest;
            when 35 .. 65 =>
               A_Landscape := Landscape.Grass;
            when others =>
               A_Landscape := Landscape.Mountain;
         end case;

         if Trav_X in 20 .. 25 and Trav_Y = 20 then
            A_Landscape := Landscape.Water;
         end if;

         Landscape.Type_Landscape'Write (Ada.Streams.Stream_IO.Stream (t), A_Landscape);
      end loop;
   end loop;

   Close (t);

   Text_IO.Put_Line ("try read");

   Open (t, Ada.Streams.Stream_IO.In_File, "test.dat");
   Inn_Stream := Ada.Streams.Stream_IO.Stream (t);

   for Trav_X in 1 .. 100 loop
      for Trav_Y in 1 .. 100 loop
         Landscape.Type_Landscape'Read (Ada.Streams.Stream_IO.Stream (t), B_Landscape);

         Text_IO.Put_Line
           (" Trav_X=" & Trav_X'Img & " Trav_Y=" & Trav_Y'Img & " Landscape=" & B_Landscape'Img);

      end loop;
   end loop;

   Close (t);

end Create_A_Map;
