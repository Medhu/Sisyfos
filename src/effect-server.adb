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

with Ada.Containers.Hashed_Maps;
with Text_IO;

package body Effect.Server is
   Verbose : constant Boolean := False;
   Effect_Type_Info_List : Type_Effect_Type_Info_List_Access;

   procedure Init (P_Effect_Info : in Type_Effect_Type_Info_List)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Effect.Server.Init - enter");
      end if;

      Effect_Type_Info_List := new Type_Effect_Type_Info_List'(P_Effect_Info);

      if Verbose then
         Text_IO.Put_Line ("Effect.Server.Init - exit");
      end if;
   end Init;

   procedure Save_Effects( P_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                          P_Effect_List : in Effect_List.Map)
   is
   begin
      Effect_List.Map'Write(P_Stream, P_Effect_List);
   end Save_Effects;

   procedure Load_Effects(P_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                          P_Effect_List : out Effect_List.Map)
   is
   begin
      Effect_List.Map'Read(P_Stream, P_Effect_List);
   end Load_Effects;

end Effect.Server;
