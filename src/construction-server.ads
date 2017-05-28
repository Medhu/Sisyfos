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

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Utilities;
with Hexagon.Server_Map;

package Construction.Server is

   type Type_Way_List is
     array
     (Hexagon.Server_Map.Type_Neighbour_List'First ..
          Hexagon.Server_Map.Type_Neighbour_List'Last) of Boolean;

   type Type_Construction_Type_Info is record
      Type_Name                 : Utilities.RemoteString.Type_String;
      Blocking_Neighbour_Number : Type_Way_List;
   end record;

   type Type_Construction_Type_Info_List is
     array (Construction.Type_Construction range <>) of Type_Construction_Type_Info;
   type Type_Construction_Type_Info_List_Access is access all Type_Construction_Type_Info_List;

   procedure Init (P_Construction_Info : in Type_Construction_Type_Info_List);

   function Is_Blocking_Neighbour_Number
     (P_Construction_List : in Construction_List.Set;
      P_Neighbour_Number  :    Positive) return Boolean;

   procedure Save_Construction
     (P_Stream            : in Ada.Streams.Stream_IO.Stream_Access;
      P_Construction_List : in Construction_List.Set);

   procedure Load_Construction
     (P_Stream            : in     Ada.Streams.Stream_IO.Stream_Access;
      P_Construction_List :    out Construction_List.Set);

end Construction.Server;
