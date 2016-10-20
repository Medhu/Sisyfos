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
with Piece.Client_Piece;
with Piece.Server;
with Landscape;
with Hexagon.Area.Server_Area;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Server.ServerAPI;
with Test_Piece;
with Status;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Observation;
with Construction;
with Action;

package body Tc_Construction is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

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
      return Format ("Test construction procedure");
   end Name;

   procedure Test_Game_Start (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line     : Utilities.RemoteString.Type_Command_Parameters;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Game_Start - enter");
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
           (Test_Piece.HTML_Path & "tc_construction-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Game_Start - exit");
      end if;
   end Test_Game_Start;

   procedure Test_Perform_Construction_Allowed_Patch_1
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Allowed_Patch_1 - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Allowed_Patch_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1100),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1100);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Allowed_Patch_1_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1100).Result = Status.Ok,
         Message   => "Could not create construction " & Test_Piece.Test_List.all (1100).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Allowed_Patch_1 - exit");
      end if;

   end Test_Perform_Construction_Allowed_Patch_1;

   procedure Test_Perform_Construction_Occupied_Patch
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Occupied_Patch - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Occupied_Patch_01.html"));

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 5);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1101),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1101);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1101).Result = Status.Target_Patch_Occupied,
         Message   =>
           "Expected Target_Patch_Occupied got status " &
           Test_Piece.Test_List.all (1101).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Occupied_Patch - exit");
      end if;

   end Test_Perform_Construction_Occupied_Patch;

   procedure Test_Perform_Construction_Not_Before_Perform_Construction
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_construction.Test_Perform_Construction_Not_Before_Perform_Construction - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Not_Before_Perform_Construction_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 7);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1102),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1102);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Not_Before_Perform_Construction_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List (1102).Result = Status.Not_Before_Perform_Construction,
         Message   => "Expected Not_Players_Turn, got " & Test_Piece.Test_List (1102).Result'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_construction.Test_Perform_Construction_Not_Before_Perform_Construction - exit");
      end if;

   end Test_Perform_Construction_Not_Before_Perform_Construction;

   procedure Test_Perform_Construction_Construct_Two_Things
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_construction.Test_Perform_Construction_Construct_Two_Things - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1103),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1103);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_02.html"));

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1104),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall2);

      Test_Piece.Wait_For_Server (1104);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1104).Result = Status.Ok,
         Message   => "Expected Ok, got " & Test_Piece.Test_List.all (1104).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Two_Things - exit");
      end if;

   end Test_Perform_Construction_Construct_Two_Things;

   procedure Test_Perform_Construction_Construct_Twice
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Twice - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Twice_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1105),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1105);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Twice_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1105).Result = Status.Construction_Exists,
         Message   =>
           "Expected Construction_Exists, got " & Test_Piece.Test_List.all (1105).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Twice - exit");
      end if;

   end Test_Perform_Construction_Construct_Twice;

   procedure Test_Perform_Demolition_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece                       : Piece.Type_Piece;
      A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_1 - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_02.html"));

      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1106),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1106);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1106).Result = Status.Ok,
         Message   => "Expected Ok got status " & Test_Piece.Test_List.all (1106).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_1 - exit");
      end if;

   end Test_Perform_Demolition_1;

   procedure Test_Perform_Demolition_Occupied_Patch
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Occupied_Patch - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Occupied_Patch_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 5);

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1107),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1107);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1107).Result = Status.Target_Patch_Occupied,
         Message   =>
           "Expected Target_Patch_Occupied got status " &
           Test_Piece.Test_List.all (1107).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Occupied_Patch - exit");
      end if;

   end Test_Perform_Demolition_Occupied_Patch;

   procedure Test_Perform_Demolition_Not_Before_Perform_Demolition
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Not_Before_Perform_Demolition - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Not_Before_Perform_Demolition_01.html"));

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 4);

      Piece.Client_Piece.Perform_Construction
        (1,
         Action.Type_Action_Type (1108),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1108);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1108).Result = Status.Ok,
         Message   =>
           "Tried to build a construction for further testing " &
           Test_Piece.Test_List.all (1108).Result'Img);

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1109),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1109);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Not_Before_Perform_Demolition_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1109).Result = Status.Not_Before_Perform_Demolition,
         Message   =>
           "Expected Not_Before_Perform Demolition, got " &
           Test_Piece.Test_List.all (1109).Result'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_construction.Test_Perform_Demolition_Not_Before_Perform_Demolition - exit");
      end if;

   end Test_Perform_Demolition_Not_Before_Perform_Demolition;

   procedure Test_Perform_Demolition_Construct_Two_Things
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Two_Things - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1110),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1110);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_02.html"));

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1111),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1111).Result = Status.Ok,
         Message   => "Expected Ok, got " & Test_Piece.Test_List.all (1111).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Two_Things - exit");
      end if;

   end Test_Perform_Demolition_Construct_Two_Things;

   procedure Test_Perform_Demolition_Construct_Twice
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Countdown       : Positive;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Game_Status     : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Twice - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Twice_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary (Player_Id_1, Countdown, Game_Status, System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Piece.Client_Piece.Perform_Demolition
        (1,
         Action.Type_Action_Type (1112),
         A_Piece,
         Landscape.Type_Patch (A_Construction_Patch.all),
         Test_Piece.Construction_Wall1);

      Test_Piece.Wait_For_Server (1112);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Twice_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1112).Result = Status.Construction_Doesnt_Exist,
         Message   =>
           "Expected Construction_Exists, got " & Test_Piece.Test_List.all (1112).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Twice - exit");
      end if;

   end Test_Perform_Demolition_Construct_Twice;

   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_attempt_move.Test_Game_Stop - enter");
      end if;

      Test_ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_attempt_move.Test_Game_Stop - exit");
      end if;
   end Test_Game_Stop;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      --
      -- Test Attempt_Move
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Start'Access,
         Name    => "Test Start Game");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Allowed_Patch_1'Access,
         Name    => "Test Perform Construction on allowed patch 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Occupied_Patch'Access,
         Name    => "Test Perform Construction on occupied patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Not_Before_Perform_Construction'Access,
         Name    => "Test Perform Construction Not Before Construction");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Construct_Two_Things'Access,
         Name    => "Test Perform Construction on patch  where there already is a construction");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Construct_Twice'Access,
         Name    =>
           "Test Perform Construction on patch  where there already is an exisiting construction of the same type");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_1'Access,
         Name    => "Test Perform Demolition that succeeds");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Occupied_Patch'Access,
         Name    => "Test Perform Demolition on occupied patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Not_Before_Perform_Demolition'Access,
         Name    => "Test Perform Demolition Not_Before_Perform_Demolition");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Construct_Twice'Access,
         Name    => "Test Perform Demolition of same construction twice");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Stop'Access,
         Name    => "Test Stop Game");
   end Register_Tests;

end Tc_Construction;
