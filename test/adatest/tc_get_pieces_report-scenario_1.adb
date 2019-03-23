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

with Test_Piece;
with Piece;
with Piece.Client_Piece;
with Tc_Get_Pieces_Report.Test_Assistant;
with Hexagon;
with Hexagon.Server_Map;
with Hexagon.Client_Map;
with Text_IO;
with AUnit.Assertions;
with Ada.Strings.Unbounded;
with Utilities;
with Player;
with Landscape;
with Server.ServerAPI;
with Test_ServerRCI;
--with Server.Server.Player_Action;
with Status;
with Observation;
with Piece.Server;
with Action;

package body Tc_Get_Pieces_Report.Scenario_1 is
   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

   Verbose : constant Boolean := False;

   use Piece;
   use Player;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
      Adm_Status       : Status.Type_Adm_Status;
      Player_Name_List : Utilities.RemoteString_List.Vector;
      Test_Client_Class : Test_Piece.Type_Client_Access_Class;
      Command_Line     : Utilities.RemoteString.Type_Command_Parameters;
   begin
      Test_Client_Class := new Test_Piece.Type_Client_Piece;

      Piece.Client_Piece.Init (Test_Client_Class.all);

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
           (Test_Piece.HTML_Path & "tc_game-Test_Game_Start_01.html"));

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      Hexagon.Server_Map.Save_Map (Ada.Strings.Unbounded.To_Unbounded_String ("s0010000.dat"),
                                  Hexagon.Server_Map.A_Map);
      Hexagon.Client_Map.Save_Map
        (Ada.Strings.Unbounded.To_Unbounded_String ("c0010000.dat"),
         Map_Player_1);

   end Set_Up_Case;

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

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Server.ServerAPI.Stop;
   end Tear_Down_Case;

   procedure Test_Get_Reports_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch             : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece                  : Piece.Type_Piece;
      Result                   : Boolean;
      All_Current_Observations : Piece.Server.Type_Pieces_Report;

   begin

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s390000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c010000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c011000.html"),
         Map_Player_2);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 3);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type(1300),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1300);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);
      Piece.Server.Get_Pieces_Report (Player_Id_1, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Map is not equal with regards to Patch visibility");

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Pieces
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Map is not equal with regards to Pieces");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s400000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c012000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c013000.html"),
         Map_Player_2);
   end Test_Get_Reports_1;

   procedure Test_Get_Reports_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch             : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece                  : Piece.Type_Piece;
      Result                   : Boolean;
      All_Current_Observations : Piece.Server.Type_Pieces_Report;

   begin
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s410000.html"));

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c014000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c015000.html"),
         Map_Player_2);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 3);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type(1301),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1301);

      Text_IO.Put_Line ("2 Status=" & Test_Piece.Test_List.all(1301).Result'Img);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);
      Piece.Server.Get_Pieces_Report (Player_Id_1, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s420000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c016000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c017000.html"),
         Map_Player_2);
   end Test_Get_Reports_2;

   procedure Test_Get_Reports_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece           : Piece.Type_Piece;
      Result                   : Boolean;
      All_Current_Observations : Piece.Server.Type_Pieces_Report;

   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Get_Reports_3 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s430000.html"));

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c018000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c019000.html"),
         Map_Player_2);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 3);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 2);

      From_Piece :=
        Piece.Type_Piece'
          (4,
           Test_Piece.Sentry_Piece,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String (""),
           1);

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type(1302),
         From_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);
      Piece.Server.Get_Pieces_Report (Player_Id_1, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s440000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c020000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c021000.html"),
         Map_Player_2);

      if Verbose then
         Text_IO.Put_Line ("Test_Get_Reports_3 - exit");
      end if;
   end Test_Get_Reports_3;

   procedure Test_Get_Reports_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch             : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece                  : Piece.Type_Piece;
      Result                   : Boolean;
      All_Current_Observations : Piece.Server.Type_Pieces_Report;

   begin
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s450000.html"));

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c022000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c023000.html"),
         Map_Player_2);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 3);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1303),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1303);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_2, Map_Player_2);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_2, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_2,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 2");

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_1, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 1");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s460000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c024000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c025000.html"),
         Map_Player_2);

   end Test_Get_Reports_4;

   procedure Test_Get_Reports_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch, From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece                            : Piece.Type_Piece;
      Move_Piece                         : Piece.Type_Piece;
      Result                             : Boolean;
      All_Current_Observations           : Piece.Server.Type_Pieces_Report;

   begin
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s470000.html"));

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c026000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c027000.html"),
         Map_Player_2);


      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 6);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1304),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1304);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 4);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1305),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1305);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_2, Map_Player_2);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_2, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_2,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 2");

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_1, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_1,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 1");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s480000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c028000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c029000.html"),
         Map_Player_2);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 4);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 5);
      Move_Piece :=
        Piece.Type_Piece'
          (8,
           Test_Piece.Knight_Piece,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String (""),
           2);

      Piece.Client_Piece.Perform_Move
        (Player_Id_2,
         Action.Type_Action_Type(1306),
         Move_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server(1306);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_2, Map_Player_2);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_2, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_2,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 2");

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 4);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1307),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_2, Map_Player_2);
      Tc_Get_Pieces_Report.Test_Assistant.Clear_Pieces_Report (All_Current_Observations);

      Piece.Server.Get_Pieces_Report (Player_Id_2, All_Current_Observations);

      Result :=
        Tc_Get_Pieces_Report.Test_Assistant.Verify_Observed_Patches
          (Map_Player_2,
           All_Current_Observations);

      AUnit.Assertions.Assert (Condition => Result, Message => "Map is not equal for player 2");

   end Test_Get_Reports_5;

   procedure Test_Get_Reports_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch, From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece                            : Piece.Type_Piece;
      Move_Piece                         : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      All_Current_Observations           : Piece.Server.Type_Pieces_Report;

      use Piece.Client_Piece;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_get_pieces_report-scenario_1.Test_Get_Reports_6 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s490000.html"));

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c030000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c031000.html"),
         Map_Player_2);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 40, 40);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1308),
         A_Piece,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1308);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 46, 40);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type(1309),
         A_Piece,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1309);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 39);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type(1310),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1310);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s500000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c032000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c033000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");

      Move_Piece := Piece.Client_Piece.Find_Piece_In_List(12);
      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 39);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 43, 39);

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type(1311),
         Piece.Type_Piece(Move_Piece.all),
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server(1311);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s510000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c034000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c035000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");

      Move_Piece := Piece.Client_Piece.Find_Piece_In_List(12);
      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 43, 39);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 42, 39);

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type(1312),
         Piece.Type_Piece(Move_Piece.all),
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server(1312);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s520000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c036000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c037000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");

      --

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 46, 38);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type(1313),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server(1313);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s530000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c038000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c039000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(13) /= null, Message => "This player should see ID=13");

      Move_Piece := Piece.Client_Piece.Find_Piece_In_List(13);
      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 46, 38);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 46, 37);

      Piece.Client_Piece.Perform_Move
        (Player_Id_2,
         Action.Type_Action_Type(1314),
         Piece.Type_Piece(Move_Piece.all),
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server(1314);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s540000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c040000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c041000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(13) /= null, Message => "This player should see ID=13");

     Move_Piece := Piece.Client_Piece.Find_Piece_In_List(13);
      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 46, 37);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 46, 36);

      Piece.Client_Piece.Perform_Move
        (Player_Id_2,
         Action.Type_Action_Type(1315),
         Piece.Type_Piece(Move_Piece.all),
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server(1315);

      Tc_Get_Pieces_Report.Test_Assistant.Update_From_Server (Player_Id_1, Map_Player_1);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "s550000.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c042000.html"),
         Map_Player_1);
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String (Test_Piece.HTML_Path & "c043000.html"),
         Map_Player_2);

      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(10) = null, Message => "This player shouldn't see ID=10");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(11) /= null, Message => "This player should see ID=11");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(12) /= null, Message => "This player should see ID=12");
      AUnit.Assertions.Assert (Condition => Piece.Client_Piece.Find_Piece_In_List(13) = null, Message => "This player should not see ID=13");

      if Verbose then
         Text_IO.Put_Line("tc_get_pieces_report-scenario_1.Test_Get_Reports_6 - exit");
      end if;

   end Test_Get_Reports_6;
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Tries to place a sentry on A, B = 2, 3");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Get_Reports_1'Access,
           Name    => "Client Map Tests Test Observation Area 1");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Get_Reports_2'Access,
           Name    => "Client Map Tests Test Observation Area 2");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Get_Reports_3'Access,
           Name    => "Client Map Tests Test Observation Area 3");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Get_Reports_4'Access,
           Name    => "Client Map Tests Test Observation Area 4");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Get_Reports_5'Access,
           Name    => "Client Map Tests Test Observation Area 5");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Reports_6'Access,
         Name    => "Client Map Tests Test Observation Area 6");
   end Register_Tests;

end Tc_Get_Pieces_Report.Scenario_1;
