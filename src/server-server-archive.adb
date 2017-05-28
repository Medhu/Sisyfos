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

with Utilities;
with Hexagon.Server_Map;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Text_IO;

package body Server.Server.Archive is

   Verbose : constant Boolean := False;

   procedure Creating_Game
     (P_Directory        : in     Utilities.RemoteString.Type_String;
      P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Scenario_Name    :    out Utilities.RemoteString.Type_String;
      P_Player_List      :    out Server.Type_Player_List;
      P_Countdown        :    out Positive)
   is
      Create_File   : Ada.Streams.Stream_IO.File_Type;
      Create_Stream : Ada.Streams.Stream_IO.Stream_Access;

      Throwaway : Utilities.RemoteString.Type_String;
      Dummy : Player.Type_Player_Id;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Archive.Creating_Game - enter P_Create_File_Name=" &
            Utilities.RemoteString.To_String (P_Create_File_Name));
      end if;

      Piece.Server.Reset_Positions;
      Hexagon.Server_Map.Reset_Pieces_On_Patches;

      Ada.Streams.Stream_IO.Open
        (Create_File,
         Ada.Streams.Stream_IO.In_File,
         Utilities.RemoteString.To_String (P_Directory) &
         Utilities.RemoteString.To_String (P_Create_File_Name));
      Create_Stream := Ada.Streams.Stream_IO.Stream (Create_File);

      Utilities.RemoteString.Type_String'Read (Create_Stream, P_Scenario_Name);

      Integer'Read (Create_Stream, P_Countdown);
      Player.Type_Player_Id'Read (Create_Stream, Dummy);
      for Trav_Player in P_Player_List'First .. P_Player_List'Last loop
         Utilities.RemoteString.Type_String'Read (Create_Stream, Throwaway);
         Boolean'Read (Create_Stream, P_Player_List (Trav_Player).In_Scenario);
         Boolean'Read
           (Create_Stream,
            P_Player_List (Trav_Player).Is_Observing);
      end loop;

      Ada.Streams.Stream_IO.Close (Create_File);

      Hexagon.Server_Map.Load_Map
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "maps\" &
            Utilities.RemoteString.To_String (P_Create_File_Name)));

      Piece.Server.Load_Pieces
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "pieces\" &
            Utilities.RemoteString.To_String (P_Create_File_Name)),
         Piece.Server.All_Pieces_In_Game);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Archive.Loading_Game - exit");
      end if;
   end Creating_Game;

   procedure Saving_Game
     (P_Directory : in Utilities.RemoteString.Type_String;
      P_Save_File_Name,
      P_Scenario_Name : in Utilities.RemoteString.Type_String;
      P_Player_List   : in Server.Type_Player_List;
      P_Countdown     : in Positive)
   is
      Save_File   : Ada.Streams.Stream_IO.File_Type;
      Save_Stream : Ada.Streams.Stream_IO.Stream_Access;

      Dummy : Player.Type_Player_Id := Player.Type_Player_Id(1);
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Archive.Saving_Game - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Save_File,
         Ada.Streams.Stream_IO.Out_File,
         Utilities.RemoteString.To_String (P_Directory) &
         Utilities.RemoteString.To_String (P_Save_File_Name));
      Save_Stream := Ada.Streams.Stream_IO.Stream (Save_File);

      Utilities.RemoteString.Type_String'Write (Save_Stream, P_Scenario_Name);
      Integer'Write (Save_Stream, P_Countdown);
      Player.Type_Player_Id'Write(Save_Stream, Dummy);

      -- write all players - add statuses about if they are in scenario etc
      for Trav_Player in P_Player_List'First .. P_Player_List'Last loop
         Utilities.RemoteString.Type_String'Write
           (Save_Stream,
            P_Player_List (Trav_Player).Player_Name);
         Boolean'Write (Save_Stream, P_Player_List (Trav_Player).In_Scenario);
         Boolean'Write (Save_Stream, P_Player_List (Trav_Player).Is_Observing);
      end loop;

      Ada.Streams.Stream_IO.Close (Save_File);

      Hexagon.Server_Map.Save_Map
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "maps\" &
            Utilities.RemoteString.To_String (P_Save_File_Name)));

      Piece.Server.Save_Pieces
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "pieces\" &
            Utilities.RemoteString.To_String (P_Save_File_Name)),
         Piece.Server.All_Pieces_In_Game);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Archive.Saving_Game - exit");
      end if;
   end Saving_Game;

   procedure Loading_Game
     (P_Directory      : in     Utilities.RemoteString.Type_String;
      P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Scenario_Name  :    out Utilities.RemoteString.Type_String;
      P_Player_List    :    out Server.Type_Player_List;
      P_Countdown      :    out Positive)
   is
      Load_File   : Ada.Streams.Stream_IO.File_Type;
      Load_Stream : Ada.Streams.Stream_IO.Stream_Access;

      Dummy : Player.Type_Player_Id;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Archive.Loading_Game - enter P_Loading_File_Name=" &
            Utilities.RemoteString.To_String (P_Load_File_Name));
      end if;

      Piece.Server.Reset_Positions;
      Hexagon.Server_Map.Reset_Pieces_On_Patches;

      Ada.Streams.Stream_IO.Open
        (Load_File,
         Ada.Streams.Stream_IO.In_File,
         Utilities.RemoteString.To_String (P_Directory) &
         Utilities.RemoteString.To_String (P_Load_File_Name));
      Load_Stream := Ada.Streams.Stream_IO.Stream (Load_File);

      Utilities.RemoteString.Type_String'Read (Load_Stream, P_Scenario_Name);

      Text_IO.Put_Line
        ("Server.Server.Archive.Loading_Game - P_Scenario_Name=" &
         Utilities.RemoteString.To_String (P_Scenario_Name));

      Integer'Read (Load_Stream, P_Countdown);
      Player.Type_Player_Id'Read(Load_Stream, Dummy);

      for Trav_Player in P_Player_List'First .. P_Player_List'Last loop
         Utilities.RemoteString.Type_String'Read
           (Load_Stream,
            P_Player_List (Trav_Player).Player_Name);
         Boolean'Read (Load_Stream, P_Player_List (Trav_Player).In_Scenario);
         Boolean'Read (Load_Stream, P_Player_List (Trav_Player).Is_Observing);
      end loop;

      Ada.Streams.Stream_IO.Close (Load_File);

      Hexagon.Server_Map.Load_Map
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "maps\" &
            Utilities.RemoteString.To_String (P_Load_File_Name)));

      Piece.Server.Load_Pieces
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Utilities.RemoteString.To_String (P_Directory) &
            "pieces\" &
            Utilities.RemoteString.To_String (P_Load_File_Name)),
         Piece.Server.All_Pieces_In_Game);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Archive.Loading_Game - exit");
      end if;
   end Loading_Game;

end Server.Server.Archive;
