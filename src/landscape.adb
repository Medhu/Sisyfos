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

with Text_IO;
with Hexagon;

package body Landscape is
   Verbose : constant Boolean := False;

   function To_String (P_Land : in Type_Land) return String
   is
   begin
      return "(" & Hexagon.To_String(P_Land.Pos) & ")";
   end To_String;

   procedure Put (P_Patch : in Type_Patch) is
   begin
      Text_IO.Put_Line ("Patch Pos :" & To_String(P_Patch) & " ");
   end Put;

   procedure Write_Patch (Stream : access Root_Stream_Type'Class; Item : in Type_Patch) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Landscape.Write_Patch - enter A=" & Item.Pos.A'Img & " B=" & Item.Pos.B'Img);
      end if;

      Hexagon.Type_Hexagon_Position'Write (Stream, Item.Pos);
      Landscape.Type_Landscape'Write (Stream, Item.Landscape_Here);

      if Verbose then
         Text_IO.Put_Line ("Landscape.Write_Patch - exit");
      end if;
   end Write_Patch;

   procedure Read_Patch (Stream : access Root_Stream_Type'Class; Item : out Type_Patch) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Landscape.Read_Patch - enter");
      end if;

      Hexagon.Type_Hexagon_Position'Read (Stream, Item.Pos);

      Landscape.Type_Landscape'Read (Stream, Item.Landscape_Here);

      if Verbose then
         Text_IO.Put_Line
           ("Landscape.Read_Patch - exit - Read_Patch A=" &
            Item.Pos.A'Img &
            " Patch B=" &
            Item.Pos.B'Img &
            " Item.Landscape_Here=" &
            Item.Landscape_Here'Img);
      end if;
   end Read_Patch;

end Landscape;
