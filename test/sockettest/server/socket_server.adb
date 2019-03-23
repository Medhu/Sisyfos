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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Text_IO;
with Socket_ServerRCI;
with Utilities;

procedure socket_server
is
      Command_Line  : Utilities.RemoteString.Type_Command_Parameters;
begin
   for Arguments_Index in 1 .. Ada.Command_Line.Argument_Count loop

      if Ada.Command_Line.Argument (Arguments_Index)'Length > 3
        and then Ada.Strings.Fixed.Translate
                    (Ada.Command_Line.Argument (Arguments_Index) (1 .. 3),
                     Ada.Strings.Maps.Constants.Lower_Case_Map) =
                 "ip:"
      then
         Command_Line (1) :=
            Utilities.RemoteString.To_Unbounded_String
              (Ada.Command_Line.Argument (Arguments_Index) (
           4 .. Ada.Command_Line.Argument (Arguments_Index)'Length));
      end if;

      if Ada.Command_Line.Argument (Arguments_Index)'Length > 5
        and then Ada.Strings.Fixed.Translate
                    (Ada.Command_Line.Argument (Arguments_Index) (1 .. 5),
                     Ada.Strings.Maps.Constants.Lower_Case_Map) =
                 "port:"
      then
         Command_Line (2) :=
            Utilities.RemoteString.To_Unbounded_String
              (Ada.Command_Line.Argument (Arguments_Index) (
           6 .. Ada.Command_Line.Argument (Arguments_Index)'Length));

      end if;

   end loop;

   Text_IO.Put_Line("Socket Test");

   Socket_ServerRCI.Init(Command_Line);

end socket_server;


