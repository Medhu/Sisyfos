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
--
with Text_IO;
package body Effect is

   function Effect_Less_Than_Keys (Left, Right : Type_Effect_Name) return Boolean is
   begin
      return Left < Right;
   end Effect_Less_Than_Keys;

   function Effect_Equal_Element (Left, Right : Type_Effect) return Boolean is
   begin
      return Left = Right;
   end Effect_Equal_Element;

   procedure Print_Effect_List (P_Effect_List : in Effect.Effect_List.Map) is
      Trav_Effects : Effect.Effect_List.Cursor;
   begin
      Text_IO.Put_Line
        ("Effects in Effect List: length=" & Effect.Effect_List.Length (P_Effect_List)'Img);
      Trav_Effects := Effect.Effect_List.First (P_Effect_List);
      while Effect.Effect_List.Has_Element (Trav_Effects) loop

         Text_IO.Put_Line
           ("Effect_Name=" &
            Effect.Effect_List.Element (Trav_Effects).Effect_Name'Img &
            " Aux=" &
            Effect.Effect_List.Element (Trav_Effects).Aux'Img);

         Trav_Effects := Effect.Effect_List.Next (Trav_Effects);
      end loop;

   end Print_Effect_List;

end Effect;
