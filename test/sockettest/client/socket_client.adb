--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2021  Frank J Jorgensen
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
with Client.ClientRPC;
with Utilities;
with Hexagon;
with Piece;
with Effect;
with Hexagon.Area;

procedure socket_client is
   An_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1) := (1 => Hexagon.Type_Hexagon_Position'(True, 8,3));
begin
   Text_IO.Put_Line ("Socket Test");
   Client.ClientRPC.Init;
   Client.ClientRPC.Connect (Utilities.RemoteString.To_Unbounded_String ("10.0.1.20"), 4000);
   Text_IO.Put_Line ("Client managed to connect");

   Client.ClientRPC.Start;
   Client.ClientRPC.Stop;

   Client.ClientRPC.Put_Piece (1, 2, Hexagon.Type_Hexagon_Position'(True, 3, 4), 5);

   Client.ClientRPC.Remove_Piece (1, 2, 3);

   Client.ClientRPC.Create_Piece
     (1,
      2,
      Hexagon.Type_Hexagon_Position'(True, 3, 4),
      Piece.Type_Piece'
        (5,
         Piece.Type_Piece_Type (6),
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("Test"),
         7));

   Client.ClientRPC.Perform_Attack (1, 2, 3, 4);

   Client.ClientRPC.Perform_Ranged_Attack (1, 2, 3, 4);

   Client.ClientRPC.Perform_Move (1, 2, 3, Hexagon.Type_Hexagon_Position'(True, 4, 5));

   Client.ClientRPC.Perform_Patch_Effect (1, 2, 3, 4, An_Area);

   Client.ClientRPC.Perform_Piece_Effect (1, 2, 3, 4);

   Client.ClientRPC.Grant_Piece_Effect (1, 2, 3, Effect.Type_Effect'(4, 5));

   Client.ClientRPC.Revoke_Piece_Effect (1, 2, 3, 4);

   Client.ClientRPC.Grant_Patch_Effect (1, 2, 3, Effect.Type_Effect'(4, 5), An_Area);

   Client.ClientRPC.Revoke_Patch_Effect (1, 2, 3, 4, An_Area);

   delay 2.0;
end socket_client;
