--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2016  Frank J Jorgensen
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

package body Hexagon is
   Verbose : constant Boolean := False;

   procedure Write_Hexagon_Numbers
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Numbers)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Write_Hexagon_Numbers - enter - write=" & Item'Img);
      end if;

      Integer'Write (Stream, Integer (Item));

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Write_Hexagon_Numbers - exit");
      end if;
   end Write_Hexagon_Numbers;

   procedure Read_Hexagon_Numbers
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Numbers)
   is
      Read_Item : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Read_Hexagon_Numbers - enter");
      end if;

      Integer'Read (Stream, Read_Item);

      Item := Type_Hexagon_Numbers (Read_Item);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Read_Hexagon_Numbers - exit - read=" & Read_Item'Img);
      end if;
   end Read_Hexagon_Numbers;

   procedure Write_Hexagon_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Position)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Write_Hexagon_Position - enter");
      end if;

      Boolean'Write (Stream, Item.P_Valid);
      if Item.P_Valid then
         Type_Hexagon_Numbers'Write (Stream, Item.A);
         Type_Hexagon_Numbers'Write (Stream, Item.B);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Write_Hexagon_Position - exit");
      end if;
   end Write_Hexagon_Position;

   procedure Read_Hexagon_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Position)
   is
      Valid : Boolean;
      A, B  : Type_Hexagon_Numbers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Read_Hexagon_Position - enter");
      end if;

      Boolean'Read (Stream, Valid);
      if Valid then
         Type_Hexagon_Numbers'Read (Stream, A);
         Type_Hexagon_Numbers'Read (Stream, B);
         Item := Type_Hexagon_Position'(True, A, B);
      else
         Item := Type_Hexagon_Position'(P_Valid => False);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Read_Hexagon_Position - exit");
      end if;
   end Read_Hexagon_Position;

end Hexagon;
