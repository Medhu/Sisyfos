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
with Hexagon.Server_Map;
with Ada.Real_Time;
with Ada.Task_Identification, Ada.Exceptions;
with Effect.Server;
with Server.Server.Piece_Action;
with Server.Server.Cmd;
with Server.Server.Auto;
with Server.Server.Archive;
with Ada.Containers;

package body Server.Server is

   Verbose : constant Boolean := False;

   procedure Init (P_Fighting_Piece_Class, P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
   --
      P_Landscape_Info : in Landscape.Server.Type_Landscape_Type_Info_List;
      P_Piece_Info     : in Piece.Server.Fighting_Piece.Type_Piece_Type_Info_List;
      P_House_Info     : in Piece.Server.House_Piece.Type_House_Type_Info_List;
      P_Effect_Info    : in Effect.Server.Type_Effect_Type_Info_List;

      P_Game_Creating, P_Game_Saving, P_Game_Loading : in Type_Game_Archive_Procedure;
      P_Game_Joining, P_Game_Leaving                 : in Type_Game_Joining_Leaving_Procedure;
      P_Game_Start : in Type_Game_Start_Procedure; P_Game_Upkeep : in Type_Game_Upkeep_Procedure;
      P_Game_End                                     : in Type_Game_End_Procedure)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Init - enter");
      end if;

      Text_IO.Put_Line ("Sisyfos Server - v0.6.1 Copyright (C) 2015-2021  Frank J Jorgensen");
      Text_IO.Put_Line
        ("This program comes with ABSOLUTELY NO WARRANTY; for details see attached gpl.txt");
      Text_IO.Put_Line ("or <http://www.gnu.org/licenses/>");
      Text_IO.Put_Line ("This is free software, and you are welcome to redistribute it");
      Text_IO.Put_Line ("under certain conditions; see attached file gpl.txt");
      Text_IO.Put_Line ("or <http://www.gnu.org/licenses/>");
      Text_IO.New_Line;
      Text_IO.New_Line;

      Hexagon.Server_Map.Init (Hexagon.Server_Map.A_Map);
      Landscape.Server.Init (P_Landscape_Info);
      Piece.Server.Fighting_Piece.Init (P_Fighting_Piece_Class, P_Piece_Info);
      Piece.Server.House_Piece.Init (P_House_Piece_Class, P_House_Info);
      Effect.Server.Init (P_Effect_Info);

      Game_Creating := P_Game_Creating;
      Game_Saving   := P_Game_Saving;
      Game_Loading  := P_Game_Loading;

      Game_Joining := P_Game_Joining;
      Game_Leaving := P_Game_Leaving;
      --
      Game_Start  := P_Game_Start;
      Game_Upkeep := P_Game_Upkeep;
      Game_End    := P_Game_End;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Init - exit");
      end if;
   end Init;

   Game_State : Status.Type_Game_Status := Status.Playing;

   Player_List_Internal : Type_Player_List;
   Server_Info          : Utilities.RemoteString_List.Vector;

   Minimum_Details : Positive := 1;

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id) return Utilities.RemoteString.Type_String
   is
   begin
      return Player_List_Internal (P_Player_Id).Player_Name;
   end Get_Player_Name;

   function Is_Player_In_Scenario (P_Player_Id : in Player.Type_Player_Id) return Boolean is
   begin
      return Player_List_Internal (P_Player_Id).In_Scenario;
   end Is_Player_In_Scenario;

   procedure Update_Server_Info_Maps (P_Server_Info : in out Utilities.RemoteString_List.Vector) is
   begin
      Utilities.Delete_Starting_With
        (P_Server_Info, Utilities.RemoteString.To_Unbounded_String ("Map:"));
      Utilities.Delete_Starting_With
        (P_Server_Info, Utilities.RemoteString.To_Unbounded_String ("Saved:"));

      -- Populate Server Info with maps and saved games currently
      -- exisiting in the servers file system.
      Utilities.Get_Files (P_Server_Info, "scenarios", "Map:");
      Utilities.Get_Files (P_Server_Info, "saved", "Saved:");
   end Update_Server_Info_Maps;

   procedure Set_Server_Info (P_Server_Info : in Utilities.RemoteString_List.Vector) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Set_Server_Info - enter");
      end if;

      Server_Info := Utilities.RemoteString_List.Copy (P_Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Set_Server_Info - exit");
      end if;
   end Set_Server_Info;

   procedure Get_Server_Info (P_Server_Info : out Utilities.RemoteString_List.Vector) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Get_Server_Info - enter");
      end if;

      P_Server_Info := Utilities.RemoteString_List.Copy (Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Get_Server_Info - exit");
      end if;
   end Get_Server_Info;

   procedure Observe_Game_Minimum_Details (P_Minimum_Details : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Observe_Game_Minimum_Details - enter");
      end if;

      Minimum_Details := P_Minimum_Details;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Observe_Game_Minimum_Details - exit");
      end if;
   end Observe_Game_Minimum_Details;

   procedure Observe_Game (P_Detail : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Observe_Game - enter P_Detail=" & P_Detail'Img & " Minimum_Details=" &
            Minimum_Details'Img);
      end if;

      if P_Detail >= Minimum_Details then
         -- Prepare any report regarding visible/invisible and pieces
         for Trav_Player in Player_List_Internal'First .. Player_List_Internal'Last loop

            if Player_List_Internal (Trav_Player).Active then
               Observation.Observation_Of_Patches.Observations_Of_Patches.Clear
                 (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations
                    .Observed_Patches);

               Observation.Observation_Of_Pieces.Observations_Of_Pieces.Clear
                 (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations
                    .Observed_Pieces);

               Observation.Observation_Of_Pieces_Info.Observations_Of_Pieces_Info.Clear
                 (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations
                    .Observed_Pieces_Info);

               Observation.Observation_Of_Pieces_Effects.Observations_Of_Pieces_Effects.Clear
                 (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations
                    .Observed_Pieces_Effects);

               Observation.Observation_Of_Patches_Effects.Observations_Of_Patches_Effects.Clear
                 (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations
                    .Observed_Patches_Effects);

               Piece.Server.Get_Pieces_Report
                 (Player.Type_Player_Id (Trav_Player),
                  Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                    .Current_Player_Pieces_Observations);

               -- compare observations now with observations we sent last time.
               -- send delta
               --
               declare
                  Frame_Observation : Observation.Observation_Of_Patches.Changes_To_Patches.Vector;
                  Frame_Observed_Pieces : Observation.Observation_Of_Pieces.Changes_To_Pieces
                    .Vector;
                  Frame_Pieces_Info : Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                    .Vector;
                  Frame_Pieces_Effects : Observation.Observation_Of_Pieces_Effects
                    .Changes_To_Pieces_Effects
                    .Vector;
                  Frame_Patches_Effects : Observation.Observation_Of_Patches_Effects
                    .Changes_To_Patches_Effects
                    .Vector;
                  Frame_Activity_Info : Observation.Activity.Activity_Report.Vector;
                  Frame               : Observation.Frames.Type_Visibility_Frames;

                  use Ada.Containers;
               begin
                  Observation.Observation_Of_Patches.Find_Delta_Observations
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Current_Player_Pieces_Observations
                       .Observed_Patches,
                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Patches,
                     Frame_Observation);

                  Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Current_Player_Pieces_Observations
                       .Observed_Pieces,
                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces,
                     Frame_Observed_Pieces);

                  Observation.Observation_Of_Pieces_Info.Find_Delta_Observed_Pieces_Info
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Current_Player_Pieces_Observations
                       .Observed_Pieces_Info,
                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces_Info,
                     Frame_Pieces_Info);

                  Observation.Observation_Of_Pieces_Effects.Find_Delta_Effect
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Current_Player_Pieces_Observations
                       .Observed_Pieces_Effects,
                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces_Effects,
                     Frame_Pieces_Effects);

                  Observation.Observation_Of_Patches_Effects.Find_Delta_Effect
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Current_Player_Pieces_Observations
                       .Observed_Patches_Effects,
                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Patches_Effects,
                     Frame_Patches_Effects);

                  Frame_Activity_Info :=
                    Observation.Activity.Activity_Report.Copy
                      (Player_List_Internal (Player.Type_Player_Id (Trav_Player)).Activity_Reports);
                  Observation.Activity.Activity_Report.Clear
                    (Player_List_Internal (Player.Type_Player_Id (Trav_Player)).Activity_Reports);

                  if Observation.Observation_Of_Patches.Changes_To_Patches.Length
                      (Frame_Observation) /=
                    0 or
                    Observation.Observation_Of_Pieces.Changes_To_Pieces.Length
                        (Frame_Observed_Pieces) /=
                      0 or
                    Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Length
                        (Frame_Pieces_Info) /=
                      0 or
                    Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Length
                        (Frame_Patches_Effects) /=
                      0 or
                    Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Length
                        (Frame_Pieces_Effects) /=
                      0 or
                    Observation.Activity.Activity_Report.Length (Frame_Activity_Info) /= 0 then

                     Frame.Observed_Patches     := Frame_Observation;
                     Frame.Observed_Pieces      := Frame_Observed_Pieces;
                     Frame.Pieces_Info          := Frame_Pieces_Info;
                     Frame.Pieces_Effects_Info  := Frame_Pieces_Effects;
                     Frame.Patches_Effects_Info := Frame_Patches_Effects;
                     Frame.Activities_Info      := Frame_Activity_Info;

                     Observation.Frames.Piece_Visibility_Frames.Append
                       (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                          .Visibility_Frames,
                        Frame);

                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Patches :=
                       Observation.Observation_Of_Patches.Observations_Of_Patches.Copy
                         (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                            .Current_Player_Pieces_Observations
                            .Observed_Patches);

                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces :=
                       Observation.Observation_Of_Pieces.Observations_Of_Pieces.Copy
                         (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                            .Current_Player_Pieces_Observations
                            .Observed_Pieces);

                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces_Info :=
                       Observation.Observation_Of_Pieces_Info.Observations_Of_Pieces_Info.Copy
                         (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                            .Current_Player_Pieces_Observations
                            .Observed_Pieces_Info);

                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Patches_Effects :=
                       Observation.Observation_Of_Patches_Effects.Observations_Of_Patches_Effects
                         .Copy
                         (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                            .Current_Player_Pieces_Observations
                            .Observed_Patches_Effects);

                     Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                       .Previous_Player_Pieces_Observations
                       .Observed_Pieces_Effects :=
                       Observation.Observation_Of_Pieces_Effects.Observations_Of_Pieces_Effects.Copy
                         (Player_List_Internal (Player.Type_Player_Id (Trav_Player))
                            .Current_Player_Pieces_Observations
                            .Observed_Pieces_Effects);

                  end if;
               end;
            end if;
         end loop;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Observe_Game - exit");
      end if;
   end Observe_Game;

   procedure Opponents_Activity_Report_Append (P_Detail : Positive;
      P_Player_Id                                       : Player.Type_Player_Id;
      P_Activity_Description                            : Utilities.RemoteString.Type_String)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Opponents_Activity_Report_Append - enter");
      end if;

      for Trav_Opponent_Players in Player_List_Internal'First .. Player_List_Internal'Last loop

         if Player_List_Internal (Trav_Opponent_Players).In_Scenario and
           Trav_Opponent_Players /= P_Player_Id then
            if P_Detail >= Minimum_Details then
               Observation.Activity.Activity_Report.Append
                 (Player_List_Internal (Trav_Opponent_Players).Activity_Reports,
                  Observation.Activity.Type_Activity_Report'
                    (Trav_Opponent_Players, P_Activity_Description));
            end if;
         end if;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Opponents_Activity_Report_Append - exit");
      end if;
   end Opponents_Activity_Report_Append;

   procedure Opponents_System_Report_Append (P_Detail : Positive;
      P_Player_Id                                     : Player.Type_Player_Id;
      P_Activity_Description                          : Utilities.RemoteString.Type_String)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Opponents_System_Report_Append - enter");
      end if;

      for Trav_Opponent_Players in Player_List_Internal'First .. Player_List_Internal'Last loop

         if Player_List_Internal (Trav_Opponent_Players).In_Scenario and
           Trav_Opponent_Players /= P_Player_Id then

            if P_Detail >= Minimum_Details then
               Observation.Activity.Activity_Report.Append
                 (Player_List_Internal (Trav_Opponent_Players).System_Messages,
                  Observation.Activity.Type_Activity_Report'
                    (Trav_Opponent_Players, P_Activity_Description));
            end if;
         end if;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Opponents_System_Report_Append - enter");
      end if;
   end Opponents_System_Report_Append;

   procedure Player_Activity_Report_Append (P_Detail : Positive;
      P_Player_Id                                    : Player.Type_Player_Id;
      P_Activity_Description                         : Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Activity_Report_Append - enter: '" &
            Utilities.RemoteString.To_String (P_Activity_Description) & "'");
      end if;

      if P_Detail >= Minimum_Details then
         Observation.Activity.Activity_Report.Append
           (Player_List_Internal (P_Player_Id).Activity_Reports,
            Observation.Activity.Type_Activity_Report'(P_Player_Id, P_Activity_Description));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Activity_Report_Append - exit");
      end if;
   end Player_Activity_Report_Append;

   procedure Player_System_Report_Append (P_Detail : Positive; P_Player_Id : Player.Type_Player_Id;
      P_Activity_Description                       : Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_System_Report_Append - enter");
      end if;

      if P_Detail >= Minimum_Details then
         Observation.Activity.Activity_Report.Append
           (Player_List_Internal (P_Player_Id).System_Messages,
            Observation.Activity.Type_Activity_Report'(P_Player_Id, P_Activity_Description));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_System_Report_Append - enter");
      end if;
   end Player_System_Report_Append;

   task body Type_Game_Engine is
      use Player;
      use Piece;

      Run : Status.Type_Engine_State := Status.Starting;

      Countdown                          : Positive := 1;
      Last_Update_Time, Last_Worker_Time : Ada.Real_Time.Time;

      Scenario_Name    : Utilities.RemoteString.Type_String;
      Create_File_Name : Utilities.RemoteString.Type_String;
      Save_File_Name   : Utilities.RemoteString.Type_String;
      Load_File_Name   : Utilities.RemoteString.Type_String;

      --
      Cmd_List : Server.Cmd.Cmd_List_Pkg.Vector;

      use Status;
      use Ada.Real_Time;
      use Ada.Task_Identification, Ada.Exceptions;
      use Utilities.RemoteString;
      use Ada.Containers;
   begin
      accept Entry_Start do
         Run := Status.Starting;
      end Entry_Start;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Game_Engine - Starts");
      end if;

      Countdown := 1;

      -- Initialize Server_Info with existing Maps and Saved games in the server at start.
      -- This information will be maintained during Save-Game functionality.
      Update_Server_Info_Maps (Server_Info);

      -- Setting some timers
      Last_Update_Time := Ada.Real_Time.Clock;
      Last_Worker_Time := Last_Update_Time;

   -- Must initialise as we don't know which players will be activated (this needs some improvement
   -- this is clearly not foolproof...)
      for Trav_Player in Player_List_Internal'First .. Player_List_Internal'Last loop
         Player_List_Internal (Trav_Player).Last_Update_Summary :=
           Last_Update_Time + Ada.Real_Time.Milliseconds (Time_Interval * 4);
         Player_List_Internal (Trav_Player).Last_System_Warning := Last_Update_Time;
      end loop;

      while Run /= Status.Stopped loop

         select
            --
            -- Administrative entries
            --
            accept Entry_Get_Map (P_Server_Map : out Landscape.Type_Map) do
               Hexagon.Server_Map.Get_Map (P_Server_Map);
            end Entry_Get_Map;
         or
            accept Entry_Get_Server_Info (P_Server_Info : out Utilities.RemoteString_List.Vector;
               P_Status                                 : out Status.Type_Adm_Status) do

               if Utilities.RemoteString_List.Length (Server_Info) = 0 then
                  P_Status := Status.Not_Ready;
               else
                  P_Status := Status.Adm_Ok;
                  Get_Server_Info (P_Server_Info);
               end if;
            end Entry_Get_Server_Info;

         or
            accept Entry_Set_Server_Info (P_Server_Info : in     Utilities.RemoteString_List.Vector;
               P_Status                                 :    out Status.Type_Adm_Status) do

               if Utilities.RemoteString_List.Length (Server_Info) = 0 then
                  P_Status := Status.Not_Ready;
               else
                  P_Status := Status.Adm_Ok;
                  Set_Server_Info (P_Server_Info);
               end if;

               P_Status := Status.Adm_Ok;

            end Entry_Set_Server_Info;

         or
            accept Entry_Create_Game (P_Create_File_Name : in Utilities.RemoteString.Type_String;
               P_Player_Name_List                        : in Utilities.RemoteString_List.Vector;
               P_Status                                  :    out Status.Type_Adm_Status) do

               if Run = Status.Starting then
                  declare
                     Trav_Player_Names : Utilities.RemoteString_List.Cursor;
                     I                 : Positive := 1;
                  begin
                     Trav_Player_Names := Utilities.RemoteString_List.First (P_Player_Name_List);
                     while Utilities.RemoteString_List.Has_Element (Trav_Player_Names) loop

                        Player_List_Internal (Player.Type_Player_Id (I)).Player_Name :=
                          Utilities.RemoteString_List.Element (Trav_Player_Names);
                        Player_List_Internal (Player.Type_Player_Id (I)).Active := False;

                        Utilities.RemoteString_List.Append
                          (Server_Info,
                           Utilities.RemoteString.To_Unbounded_String
                             ("Registered Player" & I'Img & ":") &
                           Utilities.RemoteString.To_String
                             (Player_List_Internal (Player.Type_Player_Id (I)).Player_Name));

                        I                 := I + 1;
                        Trav_Player_Names := Utilities.RemoteString_List.Next (Trav_Player_Names);
                     end loop;

                  end;

                  P_Status         := Status.Adm_Ok;
                  Run              := Status.Creating_Game;
                  Create_File_Name := P_Create_File_Name;
               else
                  P_Status := Status.Cant_Create_Now;
               end if;

            end Entry_Create_Game;
         or
            accept Entry_Save_Game (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
               P_Status                              :    out Status.Type_Adm_Status) do
               --               if Admin_Run = Status.Playing_Game then
               if Run = Status.Ongoing then
                  Save_File_Name := P_Save_File_Name;

                  P_Status := Status.Adm_Ok;
                  Run      := Status.Saving_Game;
               else
                  P_Status := Status.Cant_Save_Now;
               end if;

            end Entry_Save_Game;
         or
            accept Entry_Load_Game (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
               P_Status                              :    out Status.Type_Adm_Status) do

               if Run = Status.Starting then
                  Run            := Status.Loading_Game;
                  P_Status       := Status.Adm_Ok;
                  Load_File_Name := P_Load_File_Name;
               else
                  P_Status := Status.Cant_Load_Now;
               end if;

            end Entry_Load_Game;
         or
            accept Entry_Join_Game (P_Player_Id :    out Player.Type_Player_Id;
               P_Player_Name                    : in     Utilities.RemoteString.Type_String;
               P_Status                         :    out Status.Type_Adm_Status) do

               if Run = Status.Ongoing then
                  P_Player_Id := 0;
                  for Trav in Player_List_Internal'First .. Player_List_Internal'Last loop
                     if Player_List_Internal (Trav).Player_Name = P_Player_Name then
                        if not Player_List_Internal (Trav).Active then
                           P_Player_Id                        := Trav;
                           Player_List_Internal (Trav).Active := True;
                           P_Status                           := Status.Adm_Ok;

                           Server.Opponents_System_Report_Append
                             (1, P_Player_Id,
                              Utilities.RemoteString.To_String (P_Player_Name) &
                              Utilities.RemoteString.To_Unbounded_String (" has joined the game"));
                        else
                           P_Status := Status.Already_Joined;
                        end if;
                     end if;
                  end loop;
                  if P_Player_Id = 0 then
                     P_Status := Status.Not_Playing;
                  else
                     Run := Status.Joining_Game;
                  end if;
               else
                  P_Status := Status.Not_Ready;
               end if;

            end Entry_Join_Game;
         or
            accept Entry_Leave_Game (P_Player_Id : in     Player.Type_Player_Id;
               P_Player_Name                     : in     Utilities.RemoteString.Type_String;
               P_Status                          :    out Status.Type_Adm_Status) do

               if Run = Status.Ongoing then
                  if Player_List_Internal (P_Player_Id).Active then
                     Player_List_Internal (P_Player_Id).Active := False;
                     P_Status                                  := Status.Adm_Ok;

                     Server.Opponents_System_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_String (P_Player_Name) &
                        Utilities.RemoteString.To_Unbounded_String (" has left the game"));

                     Run := Status.Leaving_Game;
                  else
                     P_Status := Status.Not_Playing;
                  end if;
               else
                  P_Status := Status.Not_Ready;
               end if;

            end Entry_Leave_Game;

         or
            accept Entry_Get_Player_Name (P_Player_Id : in     Player.Type_Player_Id;
               P_Player_Name                          :    out Utilities.RemoteString.Type_String;
               P_Status                               :    out Status.Type_Adm_Status) do

               if Run = Status.Ongoing then
                  P_Player_Name := Player_List_Internal (P_Player_Id).Player_Name;
                  P_Status      := Status.Adm_Ok;
               else
                  P_Status := Status.Not_Playing;
               end if;

            end Entry_Get_Player_Name;
            --
            -- Game related entries
            --
         or
            accept Entry_Get_Activity_Reports (P_Player_Id : in     Player.Type_Player_Id;
               P_Activity_Report_List :    out Observation.Activity.Activity_Report.Vector) do
               P_Activity_Report_List := Player_List_Internal (P_Player_Id).Activity_Reports;
               Observation.Activity.Activity_Report.Clear
                 (Player_List_Internal (P_Player_Id).Activity_Reports);
            end Entry_Get_Activity_Reports;
         or
            -- To be used by a client that requests the reports created by his
            -- pieces
            accept Entry_Get_Pieces_Report (P_Player_Id : in     Player.Type_Player_Id;
               P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames.Vector) do

               P_Visibility_Frames := Player_List_Internal (P_Player_Id).Visibility_Frames;

               Observation.Frames.Clear_Frames
                 (Player_List_Internal (P_Player_Id).Visibility_Frames);

               Observation.Activity.Activity_Report.Clear
                 (Player_List_Internal (P_Player_Id).Activity_Reports);

               if Game_State = Status.End_Of_Game then
                  Player_List_Internal (P_Player_Id).Last_Report := True;
               end if;
            end Entry_Get_Pieces_Report;
         or
            accept Entry_Create_Piece
              (P_Player_Id   : in Player.Type_Player_Id; -- The player that placed this order
               P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
               P_Piece       : in Piece.Type_Piece) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Create_Piece - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  Server.Piece_Action.New_Piece (P_Piece, A_Piece_Server);

                  if Piece.Server.Validate_Create_Piece
                      (P_Player_Id, P_Action_Type, P_Pos,
                       Piece.Server.Type_Piece'Class (A_Piece_Server.all))
                  then

                     Server.Cmd.Create_Piece
                       (Cmd_List, P_Player_Id, P_Action_Type, P_Pos, A_Piece_Server);
                  end if;

               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Create_Piece - exit");
               end if;

            end Entry_Create_Piece;
         or
            accept Entry_Put_Piece (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
               P_Piece_Id                       : in Piece.Type_Piece_Id) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Put_Piece - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to put a piece that is not yours"));
                  else
                     if Piece.Server.Validate_Put_Piece
                         (P_Player_Id, P_Action_Type, P_Pos,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all))
                     then
                        Server.Cmd.Put_Piece
                          (Cmd_List, P_Player_Id, P_Action_Type, P_Pos, A_Piece_Server.all.Id);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Put Piece Piece Id:" & P_Piece_Id'Img & " Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Put_Piece - exit");
               end if;

            end Entry_Put_Piece;
         or
            accept Entry_Remove_Piece (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Remove_Piece - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to remove a piece that is not yours"));
                  else
                     if Piece.Server.Validate_Remove_Piece
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all))
                     then
                        Server.Cmd.Remove_Piece
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Remove Piece Piece Id:" & P_Piece_Id'Img &
                           " Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Remove_Piece - exit");
               end if;

            end Entry_Remove_Piece;
         or
            accept Entry_Perform_Attack (P_Player_Id     : in Player.Type_Player_Id;
               P_Action_Type                             : in Action.Type_Action_Type;
               P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id) do

               if Verbose then
                  Text_IO.Put_Line
                    ("Server.Server.Game_Engine.Entry_Perform_Attack - enter P_Attacking_Piece_Id=" &
                     P_Attacking_Piece_Id'Img & " P_Attacked_Piece_Id=" & P_Attacked_Piece_Id'Img);
               end if;

               declare
                  An_Attacking_Piece_Server : Piece.Server.Type_Piece_Access_Class;
                  An_Attacked_Piece_Server  : Piece.Server.Type_Piece_Access_Class;
               begin

                  An_Attacking_Piece_Server :=
                    Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id).Actual_Piece;
                  An_Attacked_Piece_Server :=
                    Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id).Actual_Piece;

                  if An_Attacking_Piece_Server.all.Category /= Piece.Fighting_Piece then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack with a piece that can't attack"));

                  elsif An_Attacked_Piece_Server.all.Category /= Piece.Fighting_Piece then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack a piece that can't be attacked"));

                  elsif not Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (An_Attacking_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack with a piece that is not yours"));

                     -- check if player attacks himself
                  elsif Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (An_Attacked_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack a piece that is yours"));

                  else

                     if Piece.Server.Fighting_Piece.Validate_Perform_Attack
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Fighting_Piece.Type_Piece'Class
                            (An_Attacking_Piece_Server.all),
                          Piece.Server.Fighting_Piece.Type_Piece'Class
                            (An_Attacked_Piece_Server.all))
                     then
                        Server.Cmd.Perform_Attack
                          (Cmd_List, P_Player_Id, P_Action_Type, An_Attacking_Piece_Server.all.Id,
                           An_Attacked_Piece_Server.all.Id);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Perform Attack either Attacking Piece Id:" & P_Attacking_Piece_Id'Img &
                           " or Attacked Piece Id:" & P_Attacked_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Attack - exit");
               end if;

            end Entry_Perform_Attack;
         or
            accept Entry_Perform_Ranged_Attack (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type                                : in Action.Type_Action_Type;
               P_Attacking_Piece_Id, P_Attacked_Piece_Id    : in Piece.Type_Piece_Id) do

               if Verbose then
                  Text_IO.Put_Line
                    ("Server.Server.Game_Engine.Entry_Perform_Ranged_Attack - enter");
               end if;

               declare
                  An_Attacking_Piece_Server : Piece.Server.Type_Piece_Access_Class;
                  An_Attacked_Piece_Server  : Piece.Server.Type_Piece_Access_Class;
               begin

                  An_Attacking_Piece_Server :=
                    Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id).Actual_Piece;
                  An_Attacked_Piece_Server :=
                    Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id).Actual_Piece;

                  if An_Attacking_Piece_Server.all.Category /= Piece.Fighting_Piece then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack with a piece that can't attack"));

                  elsif An_Attacked_Piece_Server.all.Category /= Piece.Fighting_Piece then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack a piece that can't be attacked"));

                  elsif not Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (An_Attacking_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack with a piece that is not yours"));

                     -- check if player attacks himself
                  elsif Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (An_Attacked_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to attack a piece that is yours"));

                  else
                     if Piece.Server.Fighting_Piece.Validate_Perform_Ranged_Attack
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Fighting_Piece.Type_Piece'Class
                            (An_Attacking_Piece_Server.all),
                          Piece.Server.Fighting_Piece.Type_Piece'Class
                            (An_Attacked_Piece_Server.all))
                     then
                        Server.Cmd.Perform_Ranged_Attack
                          (Cmd_List, P_Player_Id, P_Action_Type, An_Attacking_Piece_Server.all.Id,
                           An_Attacked_Piece_Server.all.Id);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Perform Ranged Attack either Attacking Piece Id:" &
                           P_Attacking_Piece_Id'Img & " or Attacked Piece Id:" &
                           P_Attacked_Piece_Id'Img & " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Ranged_Attack - exit");
               end if;
            end Entry_Perform_Ranged_Attack;
         or
            accept Entry_Perform_Move (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_To_Pos                            : in Hexagon.Type_Hexagon_Position) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Enter_Perform_Move - enter");
               end if;

               declare
                  A_Moving_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Moving_Piece_Server :=
                    Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if A_Moving_Piece_Server.all.Category /= Piece.Fighting_Piece then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to move with a piece that can't move"));

                  elsif not Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (A_Moving_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to move a piece that is not yours"));

                  else
                     if Piece.Server.Fighting_Piece.Validate_Perform_Move
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece_Server.all),
                          P_To_Pos)
                     then
                        Server.Cmd.Perform_Move
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Moving_Piece_Server.all.Id,
                           P_To_Pos);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Perform Move Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Enter_Perform_Move - exit");
               end if;

            end Entry_Perform_Move;
         or
            accept Entry_Perform_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect_Name                                    : in Effect.Type_Effect_Name;
               P_Area : in Hexagon.Area.Type_Action_Capabilities_A) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Patch_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (A_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to perform a patch effect with a piece that is not yours"));

                  else
                     if Piece.Server.Validate_Perform_Patch_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Effect_Name, P_Area)
                     then
                        Server.Cmd.Perform_Patch_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id, P_Effect_Name,
                           P_Area);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Perform Patch Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Patch_Effect - exit");
               end if;

            end Entry_Perform_Patch_Effect;
         or
            accept Entry_Perform_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect_Name                                    : in Effect.Type_Effect_Name) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Piece_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece
                      (Piece.Server.Type_Piece (A_Piece_Server.all), P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to perform a piece effect with a piece that is not yours"));

                  else

                     if Piece.Server.Validate_Perform_Piece_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Effect_Name)
                     then
                        Server.Cmd.Perform_Piece_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id, P_Effect_Name);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Perform Piece Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Perform_Piece_Effect - exit");
               end if;

            end Entry_Perform_Piece_Effect;
         or
            accept Entry_Grant_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect                                  : in Effect.Type_Effect) do
               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Grant_Piece_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to grant effect to a piece that is not yours"));

                  else
                     if Piece.Server.Validate_Grant_Piece_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Effect)
                     then
                        Server.Cmd.Grant_Piece_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id, P_Effect);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Grant Piece Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Grant_Piece_Effect - exit");
               end if;
            end Entry_Grant_Piece_Effect;
         or
            accept Entry_Revoke_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect_Name                              : in Effect.Type_Effect_Name) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Revoke_Piece_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to revoke effect from a piece that is not yours"));
                  else
                     if Piece.Server.Validate_Revoke_Piece_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Effect_Name)
                     then

                        Server.Cmd.Revoke_Piece_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id,
                           P_Effect_Name);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Revoke Piece Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Revoke_Piece_Effect - exit");
               end if;
            end Entry_Revoke_Piece_Effect;
         or
            accept Entry_Grant_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect                                  : in Effect.Type_Effect;
               P_Area : in Hexagon.Area.Type_Action_Capabilities_A) do
               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Grant_Patch_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to grant an effect to a patch with a piece that is not yours"));

                  else
                     if Piece.Server.Validate_Grant_Patch_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Area, P_Effect)
                     then

                        Server.Cmd.Grant_Patch_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id, P_Effect,
                           P_Area);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Grant Patch Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Grant_Patch_Effect - exit");
               end if;
            end Entry_Grant_Patch_Effect;
         or
            accept Entry_Revoke_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
               P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
               P_Effect_Name                              : in Effect.Type_Effect_Name;
               P_Area : in Hexagon.Area.Type_Action_Capabilities_A) do

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Revoke_Patch_Effect - enter");
               end if;

               declare
                  A_Piece_Server : Piece.Server.Type_Piece_Access_Class;
               begin

                  A_Piece_Server := Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece;

                  if not Piece.Server.Is_Players_Piece (A_Piece_Server.all, P_Player_Id) then
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("You attempted to revoke an effect from a patch with a piece that is not yours"));

                  else
                     if Piece.Server.Validate_Revoke_Patch_Effect
                         (P_Player_Id, P_Action_Type,
                          Piece.Server.Type_Piece'Class (A_Piece_Server.all), P_Area, P_Effect_Name)
                     then
                        Server.Cmd.Revoke_Patch_Effect
                          (Cmd_List, P_Player_Id, P_Action_Type, A_Piece_Server.all.Id,
                           P_Effect_Name, P_Area);
                     end if;
                  end if;
               exception
                  when Piece.Server.Piece_Not_Found_Piece_Position =>
                     Server.Player_Activity_Report_Append
                       (1, P_Player_Id,
                        Utilities.RemoteString.To_Unbounded_String
                          ("Revoke Piece Effect Piece Id:" & P_Piece_Id'Img &
                           " not valid. Command will be cancelled."));
               end;

               if Verbose then
                  Text_IO.Put_Line ("Server.Server.Game_Engine.Entry_Revoke_Patch_Effect - exit");
               end if;
            end Entry_Revoke_Patch_Effect;
         or
            accept Entry_Get_Updates_Summary (P_Player_Id : in     Player.Type_Player_Id;
               P_Countdown :    out Positive; P_Game_State : out Status.Type_Game_Status;
               P_System_Messages :    out Observation.Activity.Activity_Report.Vector) do

               P_Countdown  := Countdown;
               P_Game_State := Game_State;

               P_System_Messages :=
                 Observation.Activity.Activity_Report.Copy
                   (Player_List_Internal (P_Player_Id).System_Messages);
               Observation.Activity.Activity_Report.Clear
                 (Player_List_Internal (P_Player_Id).System_Messages);

               Player_List_Internal (P_Player_Id).Last_Update_Summary := Ada.Real_Time.Clock;

               -- keep track of who got win loss messages
               if Game_State = Status.End_Of_Game then
                  Player_List_Internal (P_Player_Id).Game_State_Reported := True;
               end if;

            end Entry_Get_Updates_Summary;

         or
            accept Entry_Client_Stopped (P_Player_Id : in Player.Type_Player_Id) do

               Run := Status.Client_Stopped;

               Player_List_Internal (P_Player_Id).Active := False;
               Opponents_System_Report_Append
                 (Observation.Activity.Internal_Details, P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String ("Your opponent quit unexpectedly"));

               Observe_Game (1);

            end Entry_Client_Stopped;
         or
            accept Entry_Get_Game_Engine_State (P_Game_Engine_State : out Status.Type_Engine_State)
            do
               P_Game_Engine_State := Run;
            end Entry_Get_Game_Engine_State;
         or
            accept Entry_Stop do
               Run := Status.Stopped;
            end Entry_Stop;
         else
            null;
         end select;

         if Run = Status.Ongoing then
            -- in "Ongoing" the game is running.

            Server.Piece_Action.Execute_Cmds (Cmd_List);

            Server.Game_Upkeep.all;

            -- should only be done ones every 3 second
            Server.Auto.Check_Opponent_Communications (Player_List_Internal);

            Server.Auto.Update_Game (Last_Update_Time, Countdown, Player_List_Internal);

            begin
               Server.Game_End.all (Game_State);
            exception
               when others =>
                  Text_IO.Put_Line (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_End");
                  raise;
            end;

            if Game_State = Status.End_Of_Game then
               Run := Status.Last_Report;
            end if;

         elsif Run = Status.Starting then
            Utilities.Delete_Starting_With
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:"));
            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:" & Run'Img));

         elsif Run = Status.Creating_Game then

            Text_IO.Put_Line
              ("Creating the game based on scenario:" &
               Utilities.RemoteString.To_String (Create_File_Name));

            Server.Archive.Creating_Game
              (Utilities.RemoteString.To_Unbounded_String ("scenarios\"), Create_File_Name,
               Scenario_Name, Player_List_Internal, Countdown);

            begin
               Server.Game_Creating.all (Create_File_Name, Scenario_Name);
            exception
               when others =>
                  Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Creating");
                  Text_IO.Put_Line
                    (Text_IO.Current_Error,
                     "Create_File_Name:" & Utilities.RemoteString.To_String (Create_File_Name) &
                     " Scenario_Name:" & Utilities.RemoteString.To_String (Scenario_Name));
                  raise;
            end;

            begin
               Server.Game_Start.all;
            exception
               when others =>
                  Text_IO.Put_Line (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Start");
                  raise;
            end;

            Run := Status.Ongoing;
            Utilities.Delete_Starting_With
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:"));
            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:" & Run'Img));

         elsif Run = Status.Joining_Game then

            Run := Status.Ongoing;

            Utilities.Delete_Starting_With
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:"));
            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:" & Run'Img));
            --

            for Player_Index in Player_List_Internal'First .. Player_List_Internal'Last loop

               Utilities.Delete_Starting_With
                 (Server_Info,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Player " & Utilities.Number_To_Fixed_String (Natural (Player_Index), 2) &
                     ":"));

               if Player_List_Internal (Player_Index).Active then
                  Utilities.RemoteString_List.Append
                    (Server_Info,
                     Utilities.RemoteString.To_Unbounded_String
                       ("Player " & Utilities.Number_To_Fixed_String (Natural (Player_Index), 2) &
                        ":"));
               end if;
            end loop;

            --
            begin
               Server.Game_Joining.all;
            exception
               when others =>
                  Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Joining");
                  raise;
            end;

            Observe_Game (1);
         elsif Run = Status.Leaving_Game then

            Run := Status.Ongoing;

            Utilities.Delete_Starting_With
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:"));
            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:" & Run'Img));
            --

            for Player_Index in Player_List_Internal'First .. Player_List_Internal'Last loop
               Utilities.Delete_Starting_With
                 (Server_Info,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Player " & Utilities.Number_To_Fixed_String (Natural (Player_Index), 2) &
                     ":"));

               if Player_List_Internal (Player_Index).Active then
                  Utilities.RemoteString_List.Append
                    (Server_Info,
                     Utilities.RemoteString.To_Unbounded_String
                       ("Player " & Utilities.Number_To_Fixed_String (Natural (Player_Index), 2) &
                        ":"));
               end if;

            end loop;

            --

            begin
               Server.Game_Leaving.all;
            exception
               when others =>
                  Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Leaving");
                  raise;
            end;

            Observe_Game (1);
         elsif Run = Status.Saving_Game then
            -- One of the players requested to save the game now.
            -- this includes file operations that should not be used in the
            -- rendez-vouz sections.
            --
            Text_IO.Put_Line
              ("Saving the game based on the file:" &
               Utilities.RemoteString.To_String (Save_File_Name));

            -- Saving can be done anytime.
            Server.Archive.Saving_Game
              (Utilities.RemoteString.To_Unbounded_String ("saved\"), Save_File_Name, Scenario_Name,
               Player_List_Internal, Countdown);

            begin
               Server.Game_Saving.all (Save_File_Name, Scenario_Name);
            exception
               when others =>
                  Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Saving");
                  Text_IO.Put_Line
                    (Text_IO.Current_Error,
                     "Save_File_Name:" & Utilities.RemoteString.To_String (Save_File_Name) &
                     " Scenario_Name:" & Utilities.RemoteString.To_String (Scenario_Name));
                  raise;
            end;

            Update_Server_Info_Maps (Server_Info);

            Run := Status.Ongoing;

         elsif Run = Status.Loading_Game then
            -- A player has requested to load a game
            -- this can only happen at the beginning of a game-session.
            -- It is a little like creating a game.
            -- You cannot load a game while you are "Ongoing" already.
            -- You can only Load game during the starting phase.
            --
            Text_IO.Put_Line
              ("Loading the game based on the file:" &
               Utilities.RemoteString.To_String (Load_File_Name));

            Server.Archive.Loading_Game
              (Utilities.RemoteString.To_Unbounded_String ("saved\"), Load_File_Name, Scenario_Name,
               Player_List_Internal, Countdown);

            begin
               Server.Game_Loading.all (Load_File_Name, Scenario_Name);
            exception
               when others =>
                  Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server.Server.Game_Engine: Game_Loading");
                  Text_IO.Put_Line
                    (Text_IO.Current_Error,
                     "Load_File_Name:" & Utilities.RemoteString.To_String (Load_File_Name) &
                     " Scenario_Name:" & Utilities.RemoteString.To_String (Scenario_Name));
                  raise;
            end;

            for Trav_Player in Player_List_Internal'First .. Player_List_Internal'Last loop
               if Player_List_Internal (Trav_Player).In_Scenario and
                 Player_List_Internal (Trav_Player).Is_Observing then

                  Utilities.RemoteString_List.Append
                    (Server_Info,
                     Utilities.RemoteString.To_Unbounded_String
                       ("Registered Player" & Trav_Player'Img & ":") &
                     Utilities.RemoteString.To_String
                       (Player_List_Internal (Trav_Player).Player_Name));
               end if;
            end loop;

            Run := Status.Ongoing;

            Utilities.Delete_Starting_With
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:"));
            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Run:" & Run'Img));

         elsif Run = Last_Report then

            -- Is game over? And have every player got the information?
            declare
               Res : Boolean := True;
            begin

               for Trav in Player_List_Internal'First .. Player_List_Internal'Last loop
                  if Player_List_Internal (Player.Type_Player_Id (Trav)).Active then
                     Res :=
                       Res and
                       Player_List_Internal (Player.Type_Player_Id (Trav)).Game_State_Reported and
                       Player_List_Internal (Player.Type_Player_Id (Trav)).Last_Report;
                  end if;
               end loop;

               if Res then
                  -- all got reports -  engine is ready to stop
                  Run := Status.Stopping;
               end if;

            end;
         elsif Run = Status.Stopping then
            -- No action to be taken- we are waiting
            -- for servers main subprocedure to
            -- stop us.
            null;
         elsif Run = Status.Client_Stopped then
            Run := Status.Last_Report;
         end if;

         delay 0.0;
      end loop;

      Text_IO.Put_Line ("Server stopped");
   exception
      when E : others =>
         Text_IO.Put_Line (Text_IO.Current_Error, "Task: " & Image (Current_Task));
         Text_IO.Put_Line (Text_IO.Current_Error, Exception_Information (E));
   end Type_Game_Engine;

   procedure Start is
   begin
      if Game_Engine = null then
         Game_Engine := new Type_Game_Engine;
         Game_Engine.Entry_Start;
      end if;
   end Start;

   procedure Stop is
   begin
      if Game_Engine /= null then
         Server.Game_Engine.Entry_Stop;
         Server.Game_Engine_Free (Server.Game_Engine);
      else
         raise Game_Engine_Doesnt_Exists;
      end if;
   end Stop;

   function Get_Game_Engine_State return Status.Type_Engine_State is
      Game_Engine_State : Status.Type_Engine_State;
   begin
      Server.Game_Engine.Entry_Get_Game_Engine_State (Game_Engine_State);

      return Game_Engine_State;
   end Get_Game_Engine_State;

   procedure Run is
      Run               : Boolean := True;
      Game_Engine_State : Status.Type_Engine_State;
      A_Char            : Character;
      Char_Avail        : Boolean;

      use Status;
   begin
      while Run loop
         Game_Engine_State := Server.Get_Game_Engine_State;

         if Game_Engine_State = Status.Stopping then
            Run := False;
         else
            Text_IO.Get_Immediate (A_Char, Char_Avail);
            if Char_Avail then

               if A_Char = '0' then
                  Run := False;
               end if;
            end if;

         end if;

      end loop;

   end Run;

end Server.Server;
