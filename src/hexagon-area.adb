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

package body Hexagon.Area is
   Verbose : constant Boolean := False;

   function Find
     (P_Reachable : in Type_Action_Capabilities;
      P_A, P_B    : in Type_Hexagon_Delta_Numbers)
      return        Boolean
   is
      Found : Boolean;
      Trav  : Integer;
   begin
      Found := False;
      Trav  := P_Reachable'First;
      while Trav in P_Reachable'First .. P_Reachable'Last and not Found loop
         if P_Reachable (Trav).A = P_A and P_Reachable (Trav).B = P_B then
            Found := True;
         else
            Trav := Trav + 1;
         end if;
      end loop;
      return Found;
   end Find;

   procedure Read_Type_Hexagon_Delta_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Delta_Position)
   is
      Valid : Boolean;
      A, B  : Type_Hexagon_Delta_Numbers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Read_Type_Action_Capability - enter");
      end if;

      Boolean'Read (Stream, Valid);
      if Valid then
         Type_Hexagon_Delta_Numbers'Read (Stream, A);
         Type_Hexagon_Delta_Numbers'Read (Stream, B);
         Item := Type_Hexagon_Delta_Position'(True, A, B);
      else
         Item := Type_Hexagon_Delta_Position'(P_Valid => False);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Read_Type_Action_Capability - exit");
      end if;
   end Read_Type_Hexagon_Delta_Position;

   procedure Write_Type_Hexagon_Delta_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Delta_Position)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Write_Type_Action_Capability - enter");
      end if;

      Boolean'Write (Stream, Item.P_Valid);
      if Item.P_Valid then
         Type_Hexagon_Delta_Numbers'Write (Stream, Item.A);
         Type_Hexagon_Delta_Numbers'Write (Stream, Item.B);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Write_Type_Action_Capability - exit");
      end if;
   end Write_Type_Hexagon_Delta_Position;

   procedure Write_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Action_Capabilities)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Write_Type_Action_Capabilities - enter");
      end if;

      for Trav in Item'First .. Item'Last loop
         Type_Hexagon_Delta_Position'Write (Stream, Item (Trav));
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Write_Type_Action_Capabilities - exit");
      end if;
   end Write_Type_Action_Capabilities;

   procedure Read_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Action_Capabilities)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("Read_Type_Action_Capabilities - enter");
      end if;

      for Trav in Item'First .. Item'Last loop
         Type_Hexagon_Delta_Position'Read (Stream, Item (Trav));
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Read_Type_Action_Capabilities - exit");
      end if;
   end Read_Type_Action_Capabilities;

   procedure Output_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Action_Capabilities)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Output_Type_Action_Capabilities - enter");
      end if;
      Integer'Write (Stream, Item'First);
      Integer'Write (Stream, Item'Last);
      --
      Type_Action_Capabilities'Write (Stream, Item);

      if Verbose then
         Text_IO.Put_Line ("Output_Type_Action_Capabilities - exit");
      end if;
   end Output_Type_Action_Capabilities;

   function Input_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class)
      return   Type_Action_Capabilities
   is
      First_Index_1, Last_Index_1 : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Input_Type_Action_Capabilities - enter");
      end if;
      Integer'Read (Stream, First_Index_1);
      Integer'Read (Stream, Last_Index_1);
      declare
         Ret : Type_Action_Capabilities (First_Index_1 .. Last_Index_1);
      begin

         Type_Action_Capabilities'Read (Stream, Ret);

         if Verbose then
            Text_IO.Put_Line ("Input_Type_Action_Capabilities - exit");
         end if;
         return Ret;
      end;

   end Input_Type_Action_Capabilities;

   function To_String (P_Delta_Position : in Type_Hexagon_Delta_Position) return String
   is
   begin
      return "(" & P_Delta_Position.A'Img & ", " & P_Delta_Position.B'Img & ")";
   end To_String;

end Hexagon.Area;
