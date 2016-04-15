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
with Ada.Containers.Hashed_Maps;

package body Construction.Server is
   Verbose : constant Boolean := False;
   Construction_Type_Info_List : Type_Construction_Type_Info_List_Access;

   procedure Init (P_Construction_Info : in Type_Construction_Type_Info_List) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Construction.Server.Init - enter");
      end if;

      Construction_Type_Info_List := new Type_Construction_Type_Info_List'(P_Construction_Info);

      if Verbose then
         Text_IO.Put_Line ("Construction.Server.Init - exit");
      end if;
   end Init;

   function Is_Blocking_Neighbour_Number
     (P_Construction_List : in Construction_List.Set;
      P_Neighbour_Number  :    Positive) return Boolean
   is
      Trav_Construction : Construction.Construction_List.Cursor;
      A_Construction    : Construction.Type_Construction;
      Found             : Boolean;
   begin
      Found := False;

      Trav_Construction := Construction_List.First (P_Construction_List);
      while Construction_List.Has_Element (Trav_Construction) and not Found loop

         A_Construction := Construction_List.Element (Trav_Construction);
         Found          :=
           Construction_Type_Info_List.all (A_Construction).Blocking_Neighbour_Number
             (P_Neighbour_Number);

         Trav_Construction := Construction_List.Next (Trav_Construction);
      end loop;

      return Found;
   end Is_Blocking_Neighbour_Number;

   procedure Save_Construction
     (P_Stream            : in Ada.Streams.Stream_IO.Stream_Access;
      P_Construction_List : in Construction_List.Set)
   is
   begin
      Construction_List.Set'Write (P_Stream, P_Construction_List);
   end Save_Construction;

   procedure Load_Construction
     (P_Stream            : in     Ada.Streams.Stream_IO.Stream_Access;
      P_Construction_List :    out Construction_List.Set)
   is
   begin
      Construction_List.Set'Read (P_Stream, P_Construction_List);
   end Load_Construction;

end Construction.Server;
