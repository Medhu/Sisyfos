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

package body Landscape is
   Verbose : constant Boolean := False;

   procedure Put (P_Patch : in Type_Patch) is
   begin
      Text_IO.Put_Line ("Patch Pos :" & P_Patch.Pos.A'Img & "a + " & P_Patch.Pos.B'Img & "b" );
   end Put;

   procedure Put_Pieces_Here (P_Pieces_Here : in Pieces_Here_List.Vector) is
      Trav : Landscape.Pieces_Here_List.Cursor;

   begin
      if Verbose then
         Text_IO.Put_Line ("Landscape.Put_Pieces_Here - enter");
      end if;

      Trav := Landscape.Pieces_Here_List.First (P_Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) loop
         Text_IO.Put_Line ("Id=" & Landscape.Pieces_Here_List.Element (Trav)'Img);

         Trav := Landscape.Pieces_Here_List.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Landscape.Put_Pieces_Here - exit");
      end if;

   end Put_Pieces_Here;

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

      Item.Pieces_Here := Landscape.Pieces_Here_List.Empty_Vector;

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
