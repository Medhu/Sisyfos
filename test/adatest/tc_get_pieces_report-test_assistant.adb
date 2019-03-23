--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2019  Frank J Jorgensen
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
with Piece;
with Ada.Strings.Unbounded;
with Landscape;
with Ada.Containers;
with Piece.Client_Piece;
with Piece.Server;
with Test_Piece;

package body Tc_Get_Pieces_Report.Test_Assistant is
   Verbose : constant Boolean := False;
   Frame : Positive := 1;

   type Type_Test_Assistant_Map is array (1 .. 100, 1 .. 100) of Boolean;

   procedure Clear_Pieces_Report (P_Pieces_Report : in out Piece.Server.Type_Pieces_Report) is
   begin

      Observation.Observation_Of_Patches.Observations_Of_Patches.Clear
        (P_Pieces_Report.Observed_Patches);

      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Clear
        (P_Pieces_Report.Observed_Pieces);

      Observation.Observation_Of_Pieces_Info.Observations_Of_Pieces_Info.Clear (P_Pieces_Report.Observed_Pieces_Info);

      Observation.Observation_Of_Patches_Effects.Observations_Of_Patches_Effects.Clear
        (P_Pieces_Report.Observed_Patches_Effects);

   end Clear_Pieces_Report;

   procedure Update_From_Server
     (P_Player_Id  : in     Player.Type_Player_Id;
      P_Player_Map : in out Hexagon.Client_Map.Type_Client_Map_Info)
   is
      Player_Pieces_Visibility_Frames      : Observation.Frames.Piece_Visibility_Frames.Vector;
      Player_Observations_List : Observation.Observation_Of_Patches.Changes_To_Patches.Vector;
      Player_Observed_Pieces_List : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;
      Player_Observed_Patches_Effects_List : Observation.Observation_Of_Patches_Effects
        .Changes_To_Patches_Effects
        .Vector;
      Players_Pieces_Info : Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Vector;
      Players_Pieces_Effects : Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Vector;
      Frame_Cursor : Observation.Frames.Piece_Visibility_Frames.Cursor;

   begin
      if Verbose then
         Text_IO.Put_Line("tc_get_pieces_report.test_assistant.update_from_server - enter");
      end if;

      Piece.Client_Piece.Get_Pieces_Report (P_Player_Id, Player_Pieces_Visibility_Frames);

      Frame_Cursor :=
        Observation.Frames.Piece_Visibility_Frames.First (Player_Pieces_Visibility_Frames);

      while Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) loop
         Player_Observed_Pieces_List :=
           Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Pieces;
         Player_Observations_List :=
           Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Patches;

         Players_Pieces_Info := Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Pieces_Info;
         Players_Pieces_Effects := Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Pieces_Effects_Info;

         Piece.Client_Piece.Set_Reports_On_Pieces(Players_Pieces_Info, Players_Pieces_Effects);

         Hexagon.Client_Map.Set_Reports_On_Map
           (P_Player_Map,
            Player_Observations_List,
            Player_Observed_Pieces_List,
            Player_Observed_Patches_Effects_List);

         Hexagon.Client_Map.Save_Scenario
           (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "f" & Frame'Img & "0000.html"),
            P_Player_Map);

         Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
         Frame        := Frame + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line("tc_get_pieces_report.test_assistant.update_from_server - exit");
      end if;

   end Update_From_Server;

   function Verify_Observed_Patches
     (P_Client_Map   : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Observations : in Piece.Server.Type_Pieces_Report) return Boolean
   is
      Tmp_Map          : Type_Test_Assistant_Map := (others => (others => False));
      Tmp_Observations : Observation.Observation_Of_Patches.Observations_Of_Patches.Set;
      Found_Cursor     : Observation.Observation_Of_Patches.Observations_Of_Patches.Cursor;
      Result           : Boolean;

      use Ada.Containers;
      use Piece;
   begin
      Tmp_Observations :=
        Observation.Observation_Of_Patches.Observations_Of_Patches.Copy
          (P_Observations.Observed_Patches);

      for Trav_A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop

            Found_Cursor :=
              Observation.Observation_Of_Patches.Observations_Of_Patches.Find
                (Tmp_Observations,
                 Observation.Observation_Of_Patches.Type_Observed_Patch'
                   (Hexagon.Type_Hexagon_Numbers (Trav_A),
                    Hexagon.Type_Hexagon_Numbers (Trav_B),
                    True));

            if Observation.Observation_Of_Patches.Observations_Of_Patches.Has_Element
                (Found_Cursor)
            then

               if P_Client_Map.Map (Trav_A, Trav_B).Visible then

                  Tmp_Map (Trav_A, Trav_B) := True;
               else
                  Tmp_Map (Trav_A, Trav_B) := False;
               end if;
            else
               if P_Client_Map.Map (Trav_A, Trav_B).Visible then
                  Tmp_Map (Trav_A, Trav_B) := False;
               else
                  Tmp_Map (Trav_A, Trav_B) := True;
               end if;
            end if;
         end loop;

      end loop;

      Result := True;
      for Trav_A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            Result := Result and Tmp_Map (Trav_A, Trav_B);
         end loop;
      end loop;

      return Result;
   end Verify_Observed_Patches;

   function Verify_Observed_Pieces
     (P_Client_Map   : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Observations : in Piece.Server.Type_Pieces_Report) return Boolean
   is
      Tmp_Map          : Type_Test_Assistant_Map := (others => (others => True));
      Tmp_Observations : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Found_Cursor     : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Cursor;
      Result           : Boolean;
      Counter          : Integer                 := 0;
      Trav_Pieces      : Landscape.Pieces_Here_List.Cursor;

      use Ada.Containers;
      use Piece;
   begin
      Text_IO.Put_Line ("Test_Assistant Observed Pieces");
      Text_IO.Put_Line
        ("Observed_Pieces length=" &
         Observation.Observation_Of_Pieces.Observations_Of_Pieces.Length
           (P_Observations.Observed_Pieces)'
           Img);

      --
      --
      Tmp_Observations :=
        Observation.Observation_Of_Pieces.Observations_Of_Pieces.Copy
          (P_Observations.Observed_Pieces);

      for Trav_A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop

            Trav_Pieces :=
              Landscape.Pieces_Here_List.First (P_Client_Map.Map (Trav_A, Trav_B).Pieces_Here);
            while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

               Found_Cursor :=
                 Observation.Observation_Of_Pieces.Observations_Of_Pieces.Find
                   (Tmp_Observations,
                    Observation.Observation_Of_Pieces.Type_Observed_Piece'
                      (Hexagon.Type_Hexagon_Position'
                         (True,
                          Hexagon.Type_Hexagon_Numbers (Trav_A),
                          Hexagon.Type_Hexagon_Numbers (Trav_B)),
                       Landscape.Pieces_Here_List.Element (Trav_Pieces)));

               if Observation.Observation_Of_Pieces.Observations_Of_Pieces.Has_Element
                   (Found_Cursor)
               then
                  Tmp_Map (Trav_A, Trav_B) := Tmp_Map (Trav_A, Trav_B) and True;
               else
                  Tmp_Map (Trav_A, Trav_B) := False;
               end if;

               Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
            end loop;
         end loop;

      end loop;

      Result := True;
      for Trav_A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            Result := Result and Tmp_Map (Trav_A, Trav_B);
         end loop;
      end loop;

      return Result;
   end Verify_Observed_Pieces;
end Tc_Get_Pieces_Report.Test_Assistant;
