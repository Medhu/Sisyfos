--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2017  Frank J Jorgensen
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
with AUnit.Assertions;
with Text_IO;
with Piece;
with Hexagon;
with Landscape;
with Utilities;
with Observation;
with Test_Piece;
with Effect;
with Server.Server;

package body Tc_Streaming is

   Verbose : constant Boolean := False;
   ----------
   -- Name --
   ----------

   Test_Map1, Test_Map2 : Landscape.Type_Map;
   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test that streaming input and output of landscape information works");
   end Name;

   procedure Test_Write_And_Read_Map (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_File1, Test_File2     : Ada.Streams.Stream_IO.File_Type;
      Test_Stream1, Test_Stream2 : Ada.Streams.Stream_IO.Stream_Access;

      use Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_Map - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Test_File1,
         Ada.Streams.Stream_IO.Out_File,
         "c:\temp\stream_test_1.dat");
      Test_Stream1 := Ada.Streams.Stream_IO.Stream (Test_File1);

      for Trav_X in Test_Map1'First (1) .. Test_Map1'Last (1) loop
         for Trav_Y in Test_Map1'First (2) .. Test_Map1'Last (2) loop
            Test_Map1 (Trav_X, Trav_Y).Pos :=
              Hexagon.Type_Hexagon_Position'
                (True,
                 Hexagon.Type_Hexagon_Numbers (Trav_X),
                 Hexagon.Type_Hexagon_Numbers (Trav_Y));
            Test_Map1 (Trav_X, Trav_Y).Landscape_Here := Test_Piece.Landscape_Water;
         end loop;
      end loop;

      Landscape.Type_Map'Write (Test_Stream1, Test_Map1);

      Ada.Streams.Stream_IO.Close (Test_File1);

      Ada.Streams.Stream_IO.Open
        (Test_File2,
         Ada.Streams.Stream_IO.In_File,
         "c:\temp\stream_test_1.dat");
      Test_Stream2 := Ada.Streams.Stream_IO.Stream (Test_File2);

      for Trav_X in Test_Map2'First (1) .. Test_Map2'Last (1) loop
         for Trav_Y in Test_Map2'First (2) .. Test_Map2'Last (2) loop
            Test_Map2 (Trav_X, Trav_Y).Pos            := Hexagon.Type_Hexagon_Position'(True, 1, 1);
            Test_Map2 (Trav_X, Trav_Y).Landscape_Here := Test_Piece.Landscape_Water;
         end loop;
      end loop;

      Landscape.Type_Map'Read (Test_Stream2, Test_Map2);

      Ada.Streams.Stream_IO.Close (Test_File2);

      AUnit.Assertions.Assert (Condition => Test_Map1 = Test_Map2, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_Map - exit");
      end if;
   end Test_Write_And_Read_Map;

   procedure Test_Write_And_Read_String (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_File1, Test_File2     : Ada.Streams.Stream_IO.File_Type;
      Test_Stream1, Test_Stream2 : Ada.Streams.Stream_IO.Stream_Access;

      A_String1, A_String2, A_String3, A_String4 : Utilities.RemoteString.Type_String;

      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_String - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Test_File1,
         Ada.Streams.Stream_IO.Out_File,
         "c:\temp\stream_test_2.dat");
      Test_Stream1 := Ada.Streams.Stream_IO.Stream (Test_File1);

      A_String1 := Utilities.RemoteString.To_Unbounded_String ("0123456789");
      Utilities.RemoteString.Type_String'Write (Test_Stream1, A_String1);
      A_String2 := Utilities.RemoteString.To_Unbounded_String ("0123456789012345");
      Utilities.RemoteString.Type_String'Write (Test_Stream1, A_String2);

      Ada.Streams.Stream_IO.Close (Test_File1);

      Ada.Streams.Stream_IO.Open
        (Test_File2,
         Ada.Streams.Stream_IO.In_File,
         "c:\temp\stream_test_2.dat");
      Test_Stream2 := Ada.Streams.Stream_IO.Stream (Test_File2);

      Utilities.RemoteString.Type_String'Read (Test_Stream2, A_String3);
      Utilities.RemoteString.Type_String'Read (Test_Stream2, A_String4);

      AUnit.Assertions.Assert (Condition => A_String1 = A_String3, Message => "...");
      AUnit.Assertions.Assert (Condition => A_String2 = A_String4, Message => "...");

      Ada.Streams.Stream_IO.Close (Test_File2);

      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_String - exit");
      end if;
   end Test_Write_And_Read_String;

   procedure Test_Write_And_Read_Observations (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_File1, Test_File2     : Ada.Streams.Stream_IO.File_Type;
      Test_Stream1, Test_Stream2 : Ada.Streams.Stream_IO.Stream_Access;

      --      A_String1, A_String2, A_String3, A_String4 : Utilities.RemoteString.Type_String;
      The_Visibility_Frames_Write,
      The_Visibility_Frames_Read : Observation.Frames.Piece_Visibility_Frames.Vector;

      Frame_Observation     : Observation.Observation_Of_Patches.Changes_To_Patches.Vector;
      Frame_Observed_Pieces : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;
      Frame_Pieces_Info     : Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Vector;
      Frame_Effects : Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Vector;
      Frame_Activity_Info   : Observation.Activity.Activity_Report.Vector;
      Frame                 : Observation.Frames.Type_Visibility_Frames;

      use Utilities.RemoteString;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_Observations - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Test_File1,
         Ada.Streams.Stream_IO.Out_File,
         "c:\temp\stream_test_3.dat");
      Test_Stream1 := Ada.Streams.Stream_IO.Stream (Test_File1);

      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Frame_Observation,
         Observation.Observation_Of_Patches.Type_Observed_Patch'
           (Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (4), True));

      Observation.Observation_Of_Pieces.Changes_To_Pieces.Append
        (Frame_Observed_Pieces,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'
           (Hexagon.Type_Hexagon_Position'
              (True, Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (4)),
            Piece.Type_Piece_Id (1)));

      Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Append
        (Frame_Pieces_Info,
         Observation.Observation_Of_Pieces_Info.Type_Observed_Piece_Info'(Piece.Type_Piece'
           (1,
            Test_Piece.Sentry_Piece,
            Piece.Fighting_Piece,
            Utilities.RemoteString.To_Unbounded_String (""),
                2),
          True));

      Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Append
        (Frame_Effects,
         Observation.Observation_Of_Pieces_Effects.Type_Piece_Effect'
           (Piece.Type_Piece_Id (1), Effect.Type_Effect'(Test_Piece.Effect_Action_Point, 1), True));

      Server.Server.Player_Activity_Report_Append
        (5,
         1,
         Utilities.RemoteString.To_Unbounded_String ("test"));

      Frame.Observed_Patches :=
        Observation.Observation_Of_Patches.Changes_To_Patches.Copy (Frame_Observation);
      Frame.Observed_Pieces :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Copy (Frame_Observed_Pieces);
      Frame.Pieces_Info :=
        Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Copy (Frame_Pieces_Info);
      Frame.Pieces_Effects_Info :=
        Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Copy (Frame_Effects);
      Frame.Activities_Info := Observation.Activity.Activity_Report.Copy (Frame_Activity_Info);

      Observation.Frames.Piece_Visibility_Frames.Append (The_Visibility_Frames_Write, Frame);
      Observation.Frames.Piece_Visibility_Frames.Append (The_Visibility_Frames_Write, Frame);

      Text_IO.Put_Line
        ("Frame.Observed_Patches Lenght=" &
         Observation.Observation_Of_Patches.Changes_To_Patches.Length (Frame.Observed_Patches)'Img);
      Text_IO.Put_Line
        ("Frame.Observed_Pieces Lenght=" &
         Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Frame.Observed_Pieces)'Img);
      Text_IO.Put_Line
        ("Frame.Pieces_Info Lenght=" &
         Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Length (Frame.Pieces_Info)'Img);
      Text_IO.Put_Line
        ("Frame.Effects_Info Lenght=" &
         Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Length
           (Frame.Pieces_Effects_Info)'
           Img);
      Text_IO.Put_Line
        ("Frame.Activites_Info Lenght=" &
         Observation.Activity.Activity_Report.Length (Frame.Activities_Info)'Img);

      Text_IO.Put_Line
        ("Frame length=" &
         Observation.Frames.Piece_Visibility_Frames.Length (The_Visibility_Frames_Write)'Img);

      --      text_IO.Put_Line("Frame length=" &
      --Observation.Frames.Piece_Visibility_Frames.First(The_Visibility_Frames)'Img  );
      Observation.Frames.Piece_Visibility_Frames.Vector'Write
        (Test_Stream1,
         The_Visibility_Frames_Write);
      --        A_String1 := Utilities.RemoteString.To_Unbounded_String("0123456789");
      --        Utilities.RemoteString.Type_String'Write(Test_Stream1, A_String1);
      --        A_String2 := Utilities.RemoteString.To_Unbounded_String("0123456789012345");
      --        Utilities.RemoteString.Type_String'Write(Test_Stream1, A_String2);

      Ada.Streams.Stream_IO.Close (Test_File1);

      Ada.Streams.Stream_IO.Open
        (Test_File2,
         Ada.Streams.Stream_IO.In_File,
         "c:\temp\stream_test_3.dat");
      Test_Stream2 := Ada.Streams.Stream_IO.Stream (Test_File2);

      Observation.Frames.Piece_Visibility_Frames.Vector'Read
        (Test_Stream2,
         The_Visibility_Frames_Read);
      --        Utilities.RemoteString.Type_String'Read(Test_Stream2, A_String3);
      --        Utilities.RemoteString.Type_String'Read(Test_Stream2, A_String4);
      --
      AUnit.Assertions.Assert
        (Condition =>
           Observation.Frames.Piece_Visibility_Frames.Length (The_Visibility_Frames_Read) = 2,
         Message => "...");

      --        AUnit.Assertions.Assert (Condition => A_String2 = A_String4, Message => "...");

      Ada.Streams.Stream_IO.Close (Test_File2);

      if Verbose then
         Text_IO.Put_Line ("Tc_Landscape_Streaming.Test_Write_And_Read_Observations - exit");
      end if;
   end Test_Write_And_Read_Observations;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Write_And_Read_Map'Access,
         Name    => "");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Write_And_Read_String'Access,
         Name    => "");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Write_And_Read_Observations'Access,
         Name    => "");
   end Register_Tests;

end Tc_Streaming;
