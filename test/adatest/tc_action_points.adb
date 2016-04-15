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
with Piece.Server;
with Landscape;
with Hexagon.Area.Server_Area;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Server.Server;
with Test_Piece;
with Status;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Observation;
with Action;
with Effect;

package body Tc_Action_Points is

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

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format
          ("Test that moves, attacks and other actions consumes Action Points in the correct way");
   end Name;

   procedure Test_Game_Start (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Game_Start - enter");
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

      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String ("User A"));
      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String ("User B"));
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
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Game_Start - exit");
      end if;
   end Test_Game_Start;

   procedure Test_Spend_All_AP_On_Move (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_All_AP_On_Move - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 45, 43);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Could not create piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_All_AP_On_Move_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (4).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '10' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      A_Piece.Id := 4;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 46, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 43));

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_All_AP_On_Move_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected 'Ok', but got " & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (4).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 0,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '0' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_All_AP_On_Move - exit");
      end if;
   end Test_Spend_All_AP_On_Move;

   procedure Test_Spend_Too_Many_AP_On_Move (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      A_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status : Status.Type_Status;
      A_Path     : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_Too_Many_AP_On_Move - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 45, 43);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_Too_Many_AP_On_Move_01.html"));

      A_Piece.Id := 5;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 46, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 42));
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_Too_Many_AP_On_Move_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "..." & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_Too_Many_AP_On_Move - exit");
      end if;
   end Test_Spend_Too_Many_AP_On_Move;

   procedure Test_Two_Moves_One_Round (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      A_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status : Status.Type_Status;
      A_Path     : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Two_Moves_One_Round - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 45, 43);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Two_Moves_One_Round_01.html"));

      A_Piece.Id := 6;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 46, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 44));
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Two_Moves_One_Round_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "First Move did not succeed " & Ret_Status'Img);

      A_Piece.Id := 6;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 45));
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Two_Moves_One_Round_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Second move did not succeed " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Two_Moves_One_Round - exit");
      end if;
   end Test_Two_Moves_One_Round;

   procedure Test_Move_And_Attack_In_One_Round (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece          : Piece.Type_Piece;
      A_Piece_Attacked : Piece.Type_Piece;
      A_Patch          : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status       : Status.Type_Status;
      A_Path           : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Winner            : Player.Type_Player_Id;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Move_And_Attack_In_One_Round - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 42, 41);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (7).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece after creation, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_And_Attack_In_One_Round_01.html"));

      A_Piece.Id := 7;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 42, 41));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 41));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 42));
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_And_Attack_In_One_Round_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "First Move did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 8,
         Message   =>
           "Wrong number of Action Points on the piece, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      A_Piece.Id          := 7;
      A_Piece_Attacked.Id := 4;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 42));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 42));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 43));
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 6,
         Message   =>
           "Wrong number of Action Points on the piece, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_And_Attack_In_One_Round_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Second move did not succeed " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Move_And_Attack_In_One_Round - exit");
      end if;
   end Test_Move_And_Attack_In_One_Round;

   procedure Test_Attack_Using_Too_Many_AP (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece          : Piece.Type_Piece;
      A_Piece_Attacked : Piece.Type_Piece;
      A_Patch          : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status       : Status.Type_Status;
      A_Path           : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Winner            : Player.Type_Player_Id;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Attack_Using_Too_Many_AP - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (2);
      Ret := Test_ServerRCI.End_Turn (1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Attack_Using_Too_Many_AP_01.html"));

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 43, 42);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (8).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece after creation, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Attack_Using_Too_Many_AP_02.html"));

      Test_Piece.Attack_Action_Points_Example := 100;

      A_Piece.Id          := 8;
      A_Piece_Attacked.Id := 4;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 42));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 42));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 43));
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Test_Piece.Attack_Action_Points_Example := 1;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "Expected Out_Of_Moves but got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece. Should have AP as at start of turn since this action failed. Now Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Attack_Using_Too_Many_AP_03.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Attack_Using_Too_Many_AP - exit");
      end if;
   end Test_Attack_Using_Too_Many_AP;

   procedure Test_Move_Using_Too_Many_AP (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      A_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status : Status.Type_Status;
      A_Path     : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Move_Using_Too_Many_AP - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (2);
      Ret := Test_ServerRCI.End_Turn (1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_Using_Too_Many_AP_01.html"));

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 43, 42);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (8).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece after creation, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_Using_Too_Many_AP_02.html"));

      Test_Piece.Move_Action_Points_Example := 100;

      A_Piece.Id := 8;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 42));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 43, 44));
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Path,
         Player_Id_2,
         Ret_Status);

      Test_Piece.Move_Action_Points_Example := 1;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "Expected Out_Of_Moves but got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece. Should have AP as at start of turn since this action failed. Now Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Move_Using_Too_Many_AP_03.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Move_Using_Too_Many_AP - exit");
      end if;
   end Test_Move_Using_Too_Many_AP;

   procedure Test_Spend_All_AP_On_Attack (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece, A_Attacked_Piece : Piece.Type_Piece;
      A_Patch                   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                : Status.Type_Status;
      A_Path                    : Hexagon.Path.Vector;
      A_Server_Piece            : Piece.Server.Type_Piece_Access_Class := null;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;
      Winner            : Player.Type_Player_Id;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_All_AP_On_Attack - enter");
      end if;

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Text_IO.Put_Line ("Turn =" & Current_Player_Id'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 45, 50);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Could not create piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 50);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Could not create piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_All_AP_On_Attack_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (9).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '10' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      A_Piece.Id          := 9;
      A_Attacked_Piece.Id := 10;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 50));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 46, 50));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 51));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 51));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 44, 50));

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Attacked_Piece,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Spend_All_AP_On_Attack_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected 'Ok', but got " & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (9).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 0,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '0' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Spend_All_AP_On_Attack - exit");
      end if;
   end Test_Spend_All_AP_On_Attack;

   procedure Test_Perform_Patch_Effect_When_Enough_AP
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece        : Piece.Type_Piece;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Patch_Effect_When_Enough_AP - enter");
      end if;
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Text_IO.Put_Line ("Turn =" & Current_Player_Id'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 44, 50);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_action_points-Test_Perform_Patch_Effect_When_Enough_AP_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '4' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      A_Piece.Id := 10;
      Piece.Client_Piece.Perform_Patch_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Hexagon.Area.Type_Action_Capabilities_A'(1 => A_Patch.all.Pos),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Status after Perform_Patch_Effect should be OK, but Status=" & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 3,
         Message   =>
           "Wrong number of Action Points. Should have spent 1 so 3 Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Patch_Effect_When_Enough_AP - exit");
      end if;
   end Test_Perform_Patch_Effect_When_Enough_AP;

   procedure Test_Perform_Patch_Effect_When_Using_Too_Many_AP
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece        : Piece.Type_Piece;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_action_points.Test_Perform_Patch_Effect_When_Using_Too_Many_AP - enter");
      end if;
      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Text_IO.Put_Line ("Turn =" & Current_Player_Id'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 44, 50);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_action_points-Test_Perform_Patch_Effect_When_Using_Too_Many_AP_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      Text_IO.Put_Line
        ("*****" & A_Server_Piece.all.Id'Img & " ** " & A_Server_Piece.all.Player_Id'Img);
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '4' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Test_Piece.Search_Action_Points_Example := 10;
      A_Piece.Id                              := 10;

      Piece.Client_Piece.Perform_Patch_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Hexagon.Area.Type_Action_Capabilities_A'(1 => A_Patch.all.Pos),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   =>
           "Status after Perform_Patch_Effect should be Out_Of_Moves, but Status=" &
           Ret_Status'Img);

      Test_Piece.Search_Action_Points_Example := 1;

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent no action points, because last action failed(should have 4 Action Points left), but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_action_points.Test_Perform_Patch_Effect_When_Using_Too_Many_AP - exit");
      end if;
   end Test_Perform_Patch_Effect_When_Using_Too_Many_AP;

   procedure Test_Perform_Piece_When_Enough_AP (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;
      Ret            : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Piece_When_Enough_AP - enter");
      end if;
      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Text_IO.Put_Line ("Turn =" & Current_Player_Id'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 44, 50);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Perform_Piece_When_Enough_AP_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '4' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      A_Piece.Id := 10;

      Piece.Client_Piece.Grant_Piece_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Status after Grant_Piece_Effect should be Ok, but Status=" & Ret_Status'Img);

      Piece.Client_Piece.Perform_Piece_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Status after Perform_Patch_Effect should be Ok, but Status=" & Ret_Status'Img);

      Test_Piece.Perform_Piece_Effect_Action_Points_Example := 1;

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 3,
         Message   =>
           "Wrong number of Action Points. Should have spent no action points, because last action failed(should have 4 Action Points left), but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Piece_When_Enough_AP - exit");
      end if;
   end Test_Perform_Piece_When_Enough_AP;

   procedure Test_Perform_Piece_Too_Many_AP (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;
      Ret            : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Piece_Too_Many_AP - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Text_IO.Put_Line ("Turn =" & Current_Player_Id'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 44, 50);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Perform_Piece_Too_Many_AP_01.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent all so '4' Action Points left, but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Test_Piece.Perform_Piece_Effect_Action_Points_Example := 10;
      A_Piece.Id                                            := 10;

      Piece.Client_Piece.Grant_Piece_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Status after Grant_Piece_Effect should be Ok, but Status=" & Ret_Status'Img);

      Piece.Client_Piece.Perform_Piece_Effect
        (Action.Type_Action_Type (1),
         A_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   =>
           "Status after Perform_Patch_Effect should be Out_Of_Moves, but Status=" &
           Ret_Status'Img);

      Test_Piece.Perform_Piece_Effect_Action_Points_Example := 1;

      A_Server_Piece := Piece.Server.Find_Piece_In_List (10).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 4,
         Message   =>
           "Wrong number of Action Points. Should have spent no action points, because last action failed(should have 4 Action Points left), but Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Perform_Piece_Too_Many_AP - exit");
      end if;
   end Test_Perform_Piece_Too_Many_AP;

   procedure Test_Ranged_Attack_AP (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece                  : Piece.Type_Piece;
      A_Piece_Attacked         : Piece.Type_Piece;
      A_From_Patch, A_To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status               : Status.Type_Status;
--      A_Path                   : Hexagon.Path.Vector;

      Ret : Boolean;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Winner            : Player.Type_Player_Id;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Ranged_Attack_AP - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (2);
      Ret := Test_ServerRCI.End_Turn (1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Ranged_Attack_AP_01.html"));

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 12, 12);
      A_To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 11, 11);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_From_Patch.all),
         Current_Player_Id,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Didnt manage to create piece in Test Ranged Attack AP, status=" & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (11).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece after creation, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Ret := Test_ServerRCI.End_Turn (2);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type (1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_To_Patch.all),
         Current_Player_Id,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Didnt manage to create piece in Test Ranged Attack AP, status=" & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (11).Actual_Piece;
      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 10,
         Message   =>
           "Wrong number of Action Points on the piece after creation, Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Ranged_Attack_AP_02.html"));

      Ret := Test_ServerRCI.End_Turn (1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Test_Piece.Ranged_Attack_Action_Points_Example := 4;

      A_Piece.Id          := 11;
      A_Piece_Attacked.Id := 12;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Piece_Attacked,
         Landscape.Type_Patch (A_From_Patch.all),
         Landscape.Type_Patch (A_To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Test_Piece.Ranged_Attack_Action_Points_Example := 1;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected Ok but got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 6,
         Message   =>
           "Wrong number of Action Points on the piece. Now Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Test_Piece.Ranged_Attack_Action_Points_Example := 7;

      A_Piece.Id          := 11;
      A_Piece_Attacked.Id := 12;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Piece_Attacked,
         Landscape.Type_Patch (A_From_Patch.all),
         Landscape.Type_Patch (A_To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Test_Piece.Ranged_Attack_Action_Points_Example := 1;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "Expected Out_Of_Moves but got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 6,
         Message   =>
           "Wrong number of Action Points on the piece. Should have AP as at start of turn since this action failed. Now Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Test_Piece.Ranged_Attack_Action_Points_Example :=
        6;   -- in this case we consume exactly all remaining action points.

      A_Piece.Id          := 11;
      A_Piece_Attacked.Id := 12;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type (1),
         A_Piece,
         A_Piece_Attacked,
         Landscape.Type_Patch (A_From_Patch.all),
         Landscape.Type_Patch (A_To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Test_Piece.Ranged_Attack_Action_Points_Example := 1;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected Ok but got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => A_Server_Piece.all.Action_Points = 0,
         Message   =>
           "Wrong number of Action Points on the piece. Should have AP 0 as we consumed all AP exactly to 0. Now Action Point=" &
           A_Server_Piece.all.Action_Points'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_action_points-Test_Ranged_Attack_AP_03.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Ranged_Attack_AP - exit");
      end if;
   end Test_Ranged_Attack_AP;

   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Game_Stop - enter");
      end if;

      Test_ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_action_points.Test_Game_Stop - exit");
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
         Routine => Test_Spend_All_AP_On_Move'Access,
         Name    => "Spend all AP on one move");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Spend_Too_Many_AP_On_Move'Access,
         Name    => "Spend too many AP in one move");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Two_Moves_One_Round'Access,
         Name    => "Spend AP on two different moves on the same piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Move_And_Attack_In_One_Round'Access,
         Name    => "Spend AP on one move and one attack by the same piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Attack_Using_Too_Many_AP'Access,
         Name    => "Spend too many AP on one attack by the same piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Move_Using_Too_Many_AP'Access,
         Name    => "Spend too many AP on one move by the same piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Spend_All_AP_On_Attack'Access,
         Name    => "Spend all AP on one attack");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Patch_Effect_When_Enough_AP'Access,
         Name    => "Do a Perform_Patch_Effect while piece has enough AP");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Patch_Effect_When_Using_Too_Many_AP'Access,
         Name    => "Do a Perform_Patch_Effect when the expense of AP is too high");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Piece_When_Enough_AP'Access,
         Name    => "Do a Perform_Piece_Effect while piece has enough AP");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Piece_Too_Many_AP'Access,
         Name    => "Do a Perform_Piece_Effect when the expense of AP is too high");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Ranged_Attack_AP'Access,
         Name    => "Test Action Points for Ranged Attack");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Stop'Access,
         Name    => "Game Test stop of game engine");

   end Register_Tests;

end Tc_Action_Points;
