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

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Utilities;

package Effect.Server is

   type Type_Effect_Type_Info is record
      Type_Name                 : Utilities.RemoteString.Type_String;
   end record;

   type Type_Effect_Type_Info_List is array (Effect.Type_Effect_Name range <>) of Type_Effect_Type_Info;
   type Type_Effect_Type_Info_List_Access is access all Type_Effect_Type_Info_List;

   procedure Init (P_Effect_Info : in Type_Effect_Type_Info_List);

   procedure Save_Effects(P_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                          P_Effect_List : in Effect_List.Map);

   procedure Load_Effects(P_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                          P_Effect_List : out Effect_List.Map);

end Effect.Server;
