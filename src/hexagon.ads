--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2019  Frank J Jorgensen
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

with Ada.Containers.Vectors;

with Ada.Streams; use Ada.Streams;

package Hexagon is
   pragma Remote_Types;

   Server_Max_Number : constant Integer  := 100;
   Server_Min_Number : constant Integer  := 1;

   type Type_Hexagon_Numbers is range Server_Min_Number .. Server_Max_Number;
   type Type_Hexagon_Position (P_Valid : Boolean := False) is record
      case P_Valid is
         when True =>
            A, B : Type_Hexagon_Numbers;
         when False =>
            null;
      end case;
   end record;

   procedure Write_Hexagon_Numbers
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Numbers);
   for Type_Hexagon_Numbers'Write use Write_Hexagon_Numbers;

   procedure Read_Hexagon_Numbers
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Numbers);
   for Type_Hexagon_Numbers'Read use Read_Hexagon_Numbers;

   procedure Write_Hexagon_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Position);
   for Type_Hexagon_Position'Write use Write_Hexagon_Position;

   procedure Read_Hexagon_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Position);
   for Type_Hexagon_Position'Read use Read_Hexagon_Position;

   function To_String (P_Position : in Type_Hexagon_Position) return String;
end Hexagon;
