--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2016  Frank J Jorgensen
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
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Server.ServerAPI;
with Test_Piece;
with Status;
with Observation;
with Test_Piece;

package body Tc_Game is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List : aliased Test_Piece.Type_Test_List;

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

      Command_Line  : Utilities.RemoteString.Type_Command_Parameters;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Start - enter");
      end if;

      Test_ServerRCI.Init (Command_Line);
      Test_Piece.Test_List := Test_List'Access;

      Test_Class1               := new Test_Piece.Type_My_Test_Piece;
      Test_Class1.Id            := Piece.Undefined_Piece_Id;
      Test_Class1.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class1.Player_Id     := Player.Undefined_Player_Id;

      Test_Class2               := new Test_Piece.Type_My_Test_House;
      Test_Class2.Id            := Piece.Undefined_Piece_Id;
      Test_Class2.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class2.Player_Id     := Player.Undefined_Player_Id;

      Server.ServerAPI.Init
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
         Test_Piece.Test_End_Game'Access);
      Server.ServerAPI.Start;

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
      --Current_Player_Id      : Player.Type_Player_Id;
      Countdown1, Countdown2 : Positive;
      System_Messages        : Observation.Activity.Activity_Report.Vector;
      Game_Status            : Status.Type_Game_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Get_Update_Summary_1 - enter");
      end if;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Countdown1,
         Game_Status,
         System_Messages);

--        AUnit.Assertions.Assert
--          (Condition => Current_Player_Id = Player_Id_1 or Current_Player_Id = Player_Id_2,
--           Message   => "Current player id should be either 1 or 2");

      delay 2.0;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
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


   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_game.Test_Game_Stop - enter");
      end if;

      Server.ServerAPI.Stop;

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
         Routine => Test_Game_Stop'Access,
         Name    => "Game Test stop game engine");

   end Register_Tests;

end Tc_Game;
