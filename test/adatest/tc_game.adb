--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013  Frank J Jorgensen
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

with AUnit.Assertions;
with Test_ServerRCI;
with Text_IO;
with Piece;
with Piece.Client_Piece;
with Landscape;
with Hexagon.Area;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Server.Server;
with Test_Piece;
with Status;
with Observation;
with Action;

package body Tc_Game is
   Verbose : constant Boolean := True;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   -----------------
   -- Set_Up Case --
   -----------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      --      Hexagon.Server_Map.Save_Scenario (Ada.Strings.Unbounded.To_Unbounded_String
      --("tc_game-Set_Up_Case_01.html"));
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down Case --
   --------------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test game infrastructure");
   end Name;

   procedure Test_Game_Start (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Start - enter");
      end if;

      Test_Class1               := new Test_Piece.Type_My_Test_Piece;
      Test_Class1.Id            := Piece.Undefined_Piece_Id;
      Test_Class1.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class1.Player_Id     := Player.Undefined_Player_Id;

      Test_Class2               := new Test_Piece.Type_My_Test_House;
      Test_Class2.Id            := Piece.Undefined_Piece_Id;
      Test_Class2.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class2.Player_Id     := Player.Undefined_Player_Id;

      Server.Server.Init
        (Test_Class1.all,
         Test_Class2.all,
         Test_Piece.Landscapes_Type_Info_List,
         Test_Piece.Pieces_Type_Info_List,
         Test_Piece.Houses_Type_Info_List,
         Test_Piece.Construction_Type_Info_List,
         Test_Piece.Effect_Type_Info_List,
         Test_Piece.Test_Creating_Game'Access,
         Test_Piece.Test_Saving_Game'Access,
         Test_Piece.Test_Loading_Game'Access,
         Test_Piece.Test_Joining_Game'Access,
         Test_Piece.Test_Leaving_Game'Access,
         Test_Piece.Test_Start_Game'Access,
         Test_Piece.Test_Upkeep_Game'Access,
         Test_Piece.Test_Start_Turn'Access,
         Test_Piece.Test_End_Turn'Access,
         Test_Piece.Test_End_Game'Access);
      Server.Server.Start;

      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User A"));
      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User B"));
      Test_ServerRCI.Create_Game
        (Utilities.RemoteString.To_Unbounded_String ("test0000.dat"),
         Player_Name_List,
         Adm_Status);

      Test_ServerRCI.Join_Game
        (Utilities.RemoteString.To_Unbounded_String ("User A"),
         Adm_Status,
         Player_Id_1);

      Test_ServerRCI.Join_Game
        (Utilities.RemoteString.To_Unbounded_String ("User B"),
         Adm_Status,
         Player_Id_2);
      AUnit.Assertions.Assert
        (Condition => Player_Id_1 /= Player_Id_2,
         Message   => "Player Id's from Game engine are equal");

      AUnit.Assertions.Assert
        (Condition => Player_Id_1 = 1 or Player_Id_2 = 1 or Player_Id_1 = 2 or Player_Id_2 = 2,
         Message   => "Expected Player Id's to be 1 or 2");

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);

      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Start - exit");
      end if;
   end Test_Game_Start;

   procedure Test_Game_Get_Player_Name (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Player_Name_1 : Utilities.RemoteString.Type_String;
      Player_Name_2 : Utilities.RemoteString.Type_String;

      Adm_Status : Status.Type_Adm_Status;

      use Utilities.RemoteString;
      use Ada.Strings.Unbounded;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Get_Player_Name - enter");
      end if;

      Player_Name_1 := Test_ServerRCI.Get_Player_Name (Player_Id_1, Adm_Status);
      Player_Name_2 := Test_ServerRCI.Get_Player_Name (Player_Id_2, Adm_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_Game_Get_Player_Name_01.html"));
      AUnit.Assertions.Assert
        (Condition => Utilities.RemoteString.To_String (Player_Name_1) = "User A",
         Message   =>
           "Player name 1 not equal to 'User A' '" &
           Utilities.RemoteString.To_String (Player_Name_1) &
           "'");

      AUnit.Assertions.Assert
        (Condition => Player_Name_2 = "User B",
         Message   =>
           "Player name 1 not equal to 'User A' '" &
           Utilities.RemoteString.To_String (Player_Name_2) &
           "'");

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Get_Player_Name - exit");
      end if;
   end Test_Game_Get_Player_Name;

   procedure Test_Get_Update_Summary_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Player;
      Current_Player_Id      : Player.Type_Player_Id;
      Countdown1, Countdown2 : Positive;
      System_Messages        : Observation.Activity.Activity_Report.Vector;
      Game_Status            : Status.Type_Game_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Get_Update_Summary_1 - enter");
      end if;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown1,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Current_Player_Id = Player_Id_1 or Current_Player_Id = Player_Id_2,
         Message   => "Current player id should be either 1 or 2");

      delay 2.0;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown2,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Countdown1 /= Countdown2,
         Message   => "Countdown should change during 0.3 sec");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_Get_Update_Summary_1_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Get_Update_Summary_1 - exit");
      end if;
   end Test_Get_Update_Summary_1;

   procedure Test_End_Turn_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Player;
      Current_Player_Id_A1,
      Current_Player_Id_B1,
      Current_Player_Id_A2,
      Current_Player_Id_B2   : Player.Type_Player_Id;
      Countdown1, Countdown2 : Positive;
      System_Messages        : Observation.Activity.Activity_Report.Vector;
      Game_Status            : Status.Type_Game_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_End_Turn_1 - enter");
      end if;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id_A1,
         Countdown1,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Current_Player_Id_A1),
         Message   => "Current player (1) id could not End_Turn");

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id_B1,
         Countdown2,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Current_Player_Id_A1 /= Current_Player_Id_B1,
         Message   => "Current player has changed");

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id_A2,
         Countdown2,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Current_Player_Id_A2),
         Message   => "Current player id could not End_Turn");

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id_B2,
         Countdown2,
         Game_Status,
         System_Messages);

      AUnit.Assertions.Assert
        (Condition => Current_Player_Id_A2 /= Current_Player_Id_B2,
         Message   => "Current player (2) id has changed");

      AUnit.Assertions.Assert
        (Condition => Current_Player_Id_A1 = Current_Player_Id_B2,
         Message   => "A1 should be equal to B2");

      AUnit.Assertions.Assert
        (Condition => Current_Player_Id_B1 = Current_Player_Id_A2,
         Message   => "B1 should be equal to A2");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_End_Turn_1_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_End_Turn_1 - exit");
      end if;
   end Test_End_Turn_1;

   procedure Test_End_Turn_When_Not_Your_Turn (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Player;
      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_End_Turn_When_Not_Your_Turn - enter");
      end if;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      if Current_Player_Id = 1 then
         Current_Player_Id := 2;
      else
         Current_Player_Id := 1;
      end if;

      AUnit.Assertions.Assert
        (Condition => not Test_ServerRCI.End_Turn (Current_Player_Id),
         Message   => "Current player id could not End_Turn");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_End_Turn_When_Not_Your_Turn_01.html"));
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_End_Turn_When_Not_Your_Turn - exit");
      end if;
   end Test_End_Turn_When_Not_Your_Turn;

   procedure Test_Movement_Capability_None (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
      A_Piece : Piece.Type_Piece;
      --      A_Piece2 : Piece.Server.House_Piece.Type_House_Access_Class;
      A_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      --      A_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 39, 39);
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;
      --      Piece_Id : Piece.Type_Piece_Id;

--      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Movement_Capability_None - enter");
      end if;

      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 39, 39);

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Movement_Capability_None - here A");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Movement_Capability_None_01.html"));
      A_Piece.Id := Piece.Undefined_Piece_Id;
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      if Verbose then
         Text_IO.Put_Line
           ("tc_game.Test_Movement_Capability_None - here A - P_Piece_Id=" & A_Piece.Id'Img);
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Movement_Capability_None_02.html"));
      if Verbose then
         Text_IO.Put_Line
           ("tc_game.Test_Movement_Capability_None - create piece status =" &
            Ret_Status'Img &
            " Piece.Id=" &
            A_Piece.Id'Img);
      end if;

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := Test_ServerRCI.Movement_Capability (4);
      begin

         AUnit.Assertions.Assert
           (Condition => Ret'Length = 0,
            Message   => "Didnt return '0' movement capability for a Tower");
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Movement_Capability_None - exit");
      end if;
   end Test_Movement_Capability_None;

   procedure Test_Attack_Capability_None (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Attack_Capability_None - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_Attack_Capability_None_01.html"));
      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 41, 41);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "tc_game-Test_Attack_Capability_None_02.html"));

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := Test_ServerRCI.Attack_Capability (5);
      begin

         AUnit.Assertions.Assert
           (Condition => Ret'Length = 0,
            Message   => "Didnt return '0' attack capability for a Tower");
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Attack_Capability_None - exit");
      end if;
   end Test_Attack_Capability_None;

   procedure Test_Movement_Capability_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Movement_Capability_Sentry - entry");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Movement_Capability_Sentry_01.html"));
      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 42, 42);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Movement_Capability_Sentry_02.html"));

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := Test_ServerRCI.Movement_Capability (6);
      begin

         AUnit.Assertions.Assert
           (Condition => Ret'Length = 6,
            Message => "Didnt return '6' movement capability for a Sentry, but " & Ret'Length'Img);
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Movement_Capability_Sentry - exit");
      end if;
   end Test_Movement_Capability_Sentry;

   procedure Test_Attack_Capability_Knight (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Attack_Capability_Knight - entry");
      end if;

      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 43, 43);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Attack_Capability_Knight_01.html"));

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := Test_ServerRCI.Attack_Capability (7);
      begin

         AUnit.Assertions.Assert
           (Condition => Ret'Length = 11,
            Message   => "Didnt return '11' attack capability for a Knight");
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Attack_Capability_Knight - exit");
      end if;
   end Test_Attack_Capability_Knight;

   procedure Test_Observation_Area_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Observation_Area_Sentry - enter");
      end if;

      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 44);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_game-Test_Observation_Area_Sentry_01.html"));

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := Test_ServerRCI.Observation_Area (8);
      begin

         AUnit.Assertions.Assert
           (Condition => Ret'Length = 13,
            Message   => "Didnt return '13' attack capability for a Sentry");
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Observation_Area_Sentry - exit");
      end if;
   end Test_Observation_Area_Sentry;

   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Stop - enter");
      end if;

      Test_ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Stop - exit");
      end if;
   end Test_Game_Stop;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Start'Access,
         Name    => "Game Test start of game engine");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Get_Player_Name'Access,
         Name    => "Game Test Get Player Name");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Update_Summary_1'Access,
         Name    => "Game Test Get Update Summary 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_End_Turn_1'Access,
         Name    => "Game Test End Turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_End_Turn_When_Not_Your_Turn'Access,
         Name    => "Game Test End Turn When not your turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Movement_Capability_None'Access,
         Name    => "Testing the movement capability returned by a Tower");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Attack_Capability_None'Access,
         Name    => "Testing the attack capability returned by a Tower");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Movement_Capability_Sentry'Access,
         Name    => "Testing the movement capability returned by a Sentry");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Attack_Capability_Knight'Access,
         Name    => "Testing the movement capability returned by a Knight");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Observation_Area_Sentry'Access,
         Name    => "Testing the observation Area returned by a Sentry");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Stop'Access,
         Name    => "Game Test stop game engine");

   end Register_Tests;

end Tc_Game;
