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

with AUnit.Assertions;
with Hexagon;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Player;
with Piece;
with Piece.Client_Piece;
with Piece.Server;
with Text_IO;
with Hexagon.Server_Map;
with Ada.Strings.Unbounded;
with Server.ServerAPI;
with Test_ServerRCI;
with Utilities;
with Landscape;
with Test_Piece;
with Status;
with Server;
with Action;
with Hexagon.Area;
with Effect;

package body TC_Piece_Client_Piece is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

   ------------
   -- Set_Up Case --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line     : Utilities.RemoteString.Type_Command_Parameters;

      Adm_Status : Status.Type_Adm_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Set_Up_Case - enter");
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

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Set_Up_Case - exit");
      end if;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Tear_Down - enter");
      end if;

      Server.ServerAPI.Stop;

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Tear_Down - exit");
      end if;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test of client part of piece logic");
   end Name;

   --------------------
   -- Test Scenarios --
   --------------------
   procedure Test_Place_Piece_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;

      use Piece;
      use Piece.Server;
      use Player;
      use Status;
   begin

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_1 - enter");
      end if;

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      A_Piece.Id   := Undefined_Piece_Id;
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1001),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));
      A_Piece.Id := 4;

      Test_Piece.Wait_For_Server (1001);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_1_01.html"));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_1_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_List (1001).Result = Status.Ok,
         Message   => "Couldn't place a piece on A, B = 1, 1 " & Test_List (1001).Result'Img);

      A_Piece.Id        := 4;
      A_Piece.Player_Id := 1;
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Element (Hexagon.Server_Map.A_Map (1, 1).Pieces_Here, 1) =
           A_Piece.Id,
         Message => "Piece on A, B is wrong ");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.all.Id = A_Piece.Id and
           Piece.Type_Piece (Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.all)
               .Category =
             A_Piece.Category and
           Piece.Type_Piece (Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.all)
               .Player_Id =
             A_Piece.Player_Id and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.all.Type_Of_Piece =
             A_Piece.Type_Of_Piece,
         Message => "Piece on A, B is wrong ");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_1 - exit");
      end if;

   end Test_Place_Piece_1;

   procedure Test_Place_5_Pieces (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece1, A_Piece2, A_Piece3, A_Piece4, A_Piece5, A_Piece6 : Piece.Type_Piece;

      use Action;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_5_Pieces - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_5_Pieces_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1002),
         A_Piece1,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1002);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1002).Result = Status.Ok,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1002).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1003),
         A_Piece2,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1003);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1003).Result = Status.Ok,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1003).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1004),
         A_Piece3,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1004);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1004).Result = Status.Ok,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1004).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1005),
         A_Piece4,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1005);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1005).Result = Status.Ok,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1005).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1006),
         A_Piece5,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1006);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1006).Result = Status.Ok,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1006).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1007),
         A_Piece6,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1007);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1007).Result = Status.Patch_Occupied,
         Message   =>
           "Failed when placing a piece together with 5 others " &
           Test_Piece.Test_List.all (1007).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_5_Pieces_02.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_5_Pieces - exit");
      end if;

   end Test_Place_5_Pieces;

   procedure Test_Place_Piece_On_Occupied (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_On_Occupied - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_On_Occupied_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1008),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1008);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1008).Result = Status.Patch_Occupied,
         Message   =>
           "Failed when placing a piece on an occupied patch " &
           Test_Piece.Test_List.all (1008).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_On_Occupied - exit");
      end if;

   end Test_Place_Piece_On_Occupied;

   procedure Test_Remove_Piece (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece : Piece.Type_Piece;

      use Piece;
      use Ada.Containers;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_01.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 4;

      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1009), A_Piece);

      Test_Piece.Wait_For_Server (1009);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1009).Result = Status.Ok,
         Message   =>
           "Couldn't remove piece from A, B =1,1 " & Test_Piece.Test_List.all (1009).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 5,
         Message => "Piece on patch A,B=1,1 doesnt contain 5 pieces after Remove_Piece");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_02.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 6;
      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1010), A_Piece);

      Test_Piece.Wait_For_Server (1010);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 4,
         Message => "Piece on patch A,B=1,1 doesnt contain 4 pieces after Remove_Piece");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_03.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 7;
      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1011), A_Piece);

      Test_Piece.Wait_For_Server (1011);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 3,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 3 pieces after Remove_Piece " &
           Test_Piece.Test_List.all (1011).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_04.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 5;
      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1012), A_Piece);

      Test_Piece.Wait_For_Server (1012);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 2,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 2 pieces after Remove_Piece " &
           Test_Piece.Test_List.all (1012).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_05.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 9;
      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1013), A_Piece);

      Test_Piece.Wait_For_Server (1013);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 1,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 1 pieces after Remove_Piece " &
           Test_Piece.Test_List.all (1013).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_06.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 8;
      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1014), A_Piece);

      Test_Piece.Wait_For_Server (1014);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 0,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 0 pieces after Remove_Piece" &
           Test_Piece.Test_List.all (1014).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_07.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece - exit");
      end if;
   end Test_Remove_Piece;

   procedure Test_Perform_Move_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_1 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_1_01.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1015),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1015);

      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);
      A_Piece.Id := 12;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_1_02.html"));

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type (1016),
         A_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1016);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1016).Result = Status.Ok,
         Message   =>
           "Perform move from A, B=1, 1 to A, B=2,1 did not succeed " &
           Test_Piece.Test_List.all (1016).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (2, 1).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Sentry_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 12,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_1 - exit");
      end if;

   end Test_Perform_Move_1;

   procedure Test_Perform_Move_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_2 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_2_01.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1017),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1017);

      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      A_Piece.Id := 13;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_2_02.html"));

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type (1018),
         A_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1018);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_2_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1018).Result = Status.Ok,
         Message   =>
           "Perform move from A,B=4, 4 to A, B=4, 7 did not succeed " &
           Test_Piece.Test_List.all (1018).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 4).all.Pieces_Here) =
           1,  -- one
      --there
      --from
      --game
      --setup
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 7).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces ");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_2 - exit");
      end if;

   end Test_Perform_Move_2;

   procedure Test_Perform_Move_To_Occupied_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece,
      To_Piece1,
      To_Piece2,
      To_Piece3,
      To_Piece4,
      To_Piece5,
      To_Piece6 : Piece.Type_Piece;

      use Piece;
      use Hexagon.Client_Map;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_To_Occupied_Patch - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_01.html"));

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 3, 1);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1019),
         To_Piece1,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1019);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1019).Result = Status.Ok,
         Message   => "Failed while placing piece " & Test_Piece.Test_List.all (1019).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1020),
         To_Piece2,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1020);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1020).Result = Status.Ok,
         Message   => "Failed while placing piece " & Test_Piece.Test_List.all (1020).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_03.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1021),
         To_Piece3,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1021);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1021).Result = Status.Ok,
         Message   => "Failed while placing piece " & Test_Piece.Test_List.all (1021).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_04.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1022),
         To_Piece4,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1022);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1022).Result = Status.Ok,
         Message   => "Failed while placing piece " & Test_Piece.Test_List.all (1022).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_05.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1023),
         To_Piece5,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1023);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_06.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1024),
         To_Piece6,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1024);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1024).Result = Status.Ok,
         Message   =>
           "Failed to place piece on 3, 1 for player_id_2 " &
           Test_Piece.Test_List.all (1024).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_07.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1025),
         From_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1025);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);

      From_Piece.Id := 12;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_08.html"));

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type (1026),
         From_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1026);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_09.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1026).Result = Status.No_Path_Found,
         Message   =>
           "Should have received the status Target_Patch_Occupied " &
           Test_Piece.Test_List.all (1026).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_To_Occupied_Patch - exit");
      end if;

   end Test_Perform_Move_To_Occupied_Patch;

   procedure Test_Place_Piece_Not_Before_Create_Piece
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Place_Piece_Not_Before_Create_Piece - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Place_Piece_Not_Before_Create_Piece_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 8);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type (1027),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1027);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_Not_Before_Create_Piece");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Place_Piece_Not_Before_Create_Piece_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1027).Result = Status.Not_Before_Create_Piece,
         Message   =>
           "Failed on status when trying to place a piece and it is not your turn " &
           Test_Piece.Test_List.all (1027).Result'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_Not_Before_Create_Piece - exit");
      end if;

   end Test_Place_Piece_Not_Before_Create_Piece;

   procedure Test_Remove_Piece_Not_Before_Remove_Piece
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Remove_Piece_Not_Before_Remove_Piece - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Before_Remove_Piece_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 6, 7);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1028),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      A_Piece.Id        := 21;
      A_Piece.Player_Id := 1;

      Test_Piece.Wait_For_Server (1028);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Before_Remove_Piece_02.html"));

      Piece.Client_Piece.Remove_Piece (Player_Id_1, Action.Type_Action_Type (1029), A_Piece);

      Test_Piece.Wait_For_Server (1029);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Before_Remove_Piece_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1029).Result = Status.Not_Before_Remove_Piece,
         Message   =>
           "Failed on status when tried to remove a piece when not your turn " &
           Test_Piece.Test_List.all (1029).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 21,
         Message =>
           "Piece on patch A, B=6, 7 doesnt contain 'Knight' after Remove_Piece and not players turn (should have kept the knight there) " &
           Test_Piece.Test_List.all (1029).Result'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Remove_Piece_Not_Before_Remove_Piece - exit");
      end if;

   end Test_Remove_Piece_Not_Before_Remove_Piece;

   procedure Test_Perform_Move_Not_Before_Perform_Move
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Not_Before_Perform_Move - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Before_Perform_Move_01.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1030),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1030);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Before_Perform_Move_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1030).Result = Status.Ok,
         Message   =>
           "Failed on status when tried to create a piece " &
           Test_Piece.Test_List.all (1030).Result'Img);

      A_Piece.Id        := 22;
      A_Piece.Player_Id := 1;

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 10);

      Piece.Client_Piece.Perform_Move
        (Player_Id_1,
         Action.Type_Action_Type (1031),
         A_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1031);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Before_Perform_Move_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1031).Result = Status.Not_Before_Perform_Move,
         Message   =>
           "Failed on status when tried to perform move a piece when not valid " &
           Test_Piece.Test_List.all (1031).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 22,
         Message =>
           "Piece on patch A, B=4, 7 doesnt contain 'Knight' after Remove_Piece and not players turn (should have kept the knight there) " &
           Test_Piece.Test_List.all (1031).Result'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Not_Before_Perform_Move - exit");
      end if;

   end Test_Perform_Move_Not_Before_Perform_Move;

   procedure Test_Perform_Attack (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;

      use Piece;
      use Player;
      use Status;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_01.html"));

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1032),
         To_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1032);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1032).Result = Status.Ok,
         Message   =>
           "Failed to place piece on 4,4 for player_id_1 " &
           Test_Piece.Test_List.all (1032).Result'Img);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 4);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type (1033),
         From_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1033);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1033).Result = Status.Ok,
         Message   =>
           "Failed to place piece on 3,4 for player_id_2 " &
           Test_Piece.Test_List.all (1033).Result'Img);

      From_Piece.Id := 24;
      To_Piece.Id   := 23;
      Piece.Client_Piece.Perform_Attack
        (Player_Id_2,
         Action.Type_Action_Type (1034),
         From_Piece,
         To_Piece);

      Test_Piece.Wait_For_Server (1034);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_04.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1034).Result = Status.Ok,
         Message   =>
           "Failed on status when tried to remove a piece when not your turn " &
           Test_Piece.Test_List.all (1034).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (From_Piece.Id).Actual_Pos =
           Hexagon.Type_Hexagon_Position'(True, 3, 4) and
           Piece.Server.Find_Piece_In_List (From_Piece.Id).Actual_Piece.Id = 24 and
           Piece.Server.Find_Piece_In_List (To_Piece.Id).Actual_Pos =
             Hexagon.Type_Hexagon_Position'(P_Valid => False),
         Message => "Battle outcome is wrong");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack - exit");
      end if;

   end Test_Perform_Attack;

   procedure Test_Create_Piece_Enemy_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Create_Piece_Enemy_Patch - enter");
      end if;

      -- Piece 13 belongs to player 1 - and this new piece belongs to player two
      -- this  new pieve shall not be allowed to be places on same paths as the first.
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 7);
      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type (1035),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1035);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1036),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all));

      Test_Piece.Wait_For_Server (1036);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Create_Piece_Enemy_Patch_01.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1036).Result = Status.Patch_Occupied,
         Message   =>
           "Create Piece on enemy patch should fail with Patch_Occupied, we got " &
           Test_Piece.Test_List.all (1036).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Create_Piece_Enemy_Patch_02.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Create_Piece_Enemy_Patch - exit");
      end if;
   end Test_Create_Piece_Enemy_Patch;

   procedure Test_Perform_Ranged_Attack_And_Win (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;

      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      To_Server_Patch, From_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Win - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 18);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 16, 16);

      From_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 18);
      To_Server_Patch   := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 16);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1037),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1037);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Win_01.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1037).Result = Status.Ok,
         Message   =>
           "Perform Ranged Attack test attacking and win, create piece " &
           Test_Piece.Test_List.all (1037).Result'Img);

      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type (1038),
         A_Piece_Attacked,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1038);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1038).Result = Status.Ok,
         Message   =>
           "Perform Ranged Attack test attacking and win, create piece " &
           Test_Piece.Test_List.all (1038).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Win_02.html"));

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (27))),
         Message => "Didn't find expected piece on From patch in server map");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (28))),
         Message => "Didn't find expected piece on To patch in server map");

      A_Piece_Attacking.Id := 27;
      A_Piece_Attacked.Id  := 28;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Player_Id_1,
         Action.Type_Action_Type (1039),
         A_Piece_Attacking,
         A_Piece_Attacked);

      Test_Piece.Wait_For_Server (1039);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Win_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1039).Result = Status.Ok,
         Message   =>
           "Perform Attack and trying to win, we got " &
           Test_Piece.Test_List.all (1039).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           not Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (28))),
         Message => "Perform Ranged Attack losing piece still standing - it should have been gone");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (27))),
         Message =>
           "Perform Ranged Attack winning piece is gone - it should have remained on the position");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Win - exit");
      end if;

   end Test_Perform_Ranged_Attack_And_Win;

   procedure Test_Perform_Ranged_Attack_And_Loose
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;

      To_Patch, From_Patch               : Hexagon.Client_Map.Type_Client_Patch_Adress;
      To_Server_Patch, From_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Loose - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 18);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 16, 16);

      From_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 18);
      To_Server_Patch   := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 16);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1040),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all));

      Test_Piece.Wait_For_Server (1040);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1040).Result = Status.Ok,
         Message   =>
           "Perform Ranged Attack test attacking and win, create piece " &
           Test_Piece.Test_List.all (1040).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Loose_01.html"));

      Piece.Client_Piece.Create_Piece
        (Player_Id_2,
         Action.Type_Action_Type (1041),
         A_Piece_Attacked,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all));

      Test_Piece.Wait_For_Server (1041);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1041).Result = Status.Ok,
         Message   =>
           "Perform Ranged Attack test attacking and win, create piece " &
           Test_Piece.Test_List.all (1041).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Loose_02.html"));

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (29))),
         Message => "Didn't find expected piece on From patch");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (30))),
         Message => "Didn't find expected piece on To patch");

      A_Piece_Attacking.Id := 29;
      A_Piece_Attacked.Id  := 30;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Player_Id_1,
         Action.Type_Action_Type (1042),
         A_Piece_Attacking,
         A_Piece_Attacked);

      Test_Piece.Wait_For_Server (1042);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Loose_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1042).Result = Status.Ok,
         Message   =>
           "Perform Attack and loosing, we got " & Test_Piece.Test_List.all (1042).Result'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (29))),
         Message => "Didn't find expected piece on From patch");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (30))),
         Message => "Didn't find expected piece on To patch");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Loose - exit");
      end if;
   end Test_Perform_Ranged_Attack_And_Loose;

   procedure Test_Perform_Patch_Effect_Find_Effect
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)

   is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;

      A_Client_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      An_Area        : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1) :=
        Hexagon.Area.Type_Action_Capabilities_A'
          (1 =>
             Hexagon.Type_Hexagon_Position'
               (True, Hexagon.Type_Hexagon_Numbers'(44), Hexagon.Type_Hexagon_Numbers'(50)));

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Patch_Effect_Find_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 50);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1043),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1043);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1043).Result = Status.Ok,
         Message   =>
           "Perform Patch Effect, create piece " & Test_Piece.Test_List.all (1043).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Patch_Effect_Find_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 31;
      Piece.Client_Piece.Perform_Patch_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1044),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         An_Area);

      Test_Piece.Wait_For_Server (1044);

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Patch.all.Effects_Here) = 0,
         Message   => "Test Perform Patch Effect when you do find an effect");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Patch_Effect_Find_Effect - exit");
      end if;
   end Test_Perform_Patch_Effect_Find_Effect;

   procedure Test_Perform_Patch_Effect_Dont_Find_Effect
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)

   is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;

      A_Client_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      An_Area        : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1) :=
        Hexagon.Area.Type_Action_Capabilities_A'
          (1 =>
             Hexagon.Type_Hexagon_Position'
               (True, Hexagon.Type_Hexagon_Numbers'(44), Hexagon.Type_Hexagon_Numbers'(51)));

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Patch_Effect_Dont_Find_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 51);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1045),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1045);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1045).Result = Status.Ok,
         Message   =>
           "Perform Patch Effect, create piece " & Test_Piece.Test_List.all (1045).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Patch_Effect_Dont_Find_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 32;
      Piece.Client_Piece.Perform_Patch_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1045),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         An_Area);

      Test_Piece.Wait_For_Server (1045);

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Patch.all.Effects_Here) = 0,
         Message   => "Test Perform Patch Effect when you dont find an effect");

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Patch_Effect_Dont_Find_Effect - exit");
      end if;
   end Test_Perform_Patch_Effect_Dont_Find_Effect;

   procedure Test_Perform_Piece_Effect_Find_Effect
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)

   is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;

      A_Client_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Piece_Effect_Find_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 50);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1046),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1046);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1046).Result = Status.Ok,
         Message   =>
           "Perform Piece Effect, create piece " & Test_Piece.Test_List.all (1046).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Piece_Effect_Find_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 33;
      Piece.Client_Piece.Grant_Piece_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1047),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      Test_Piece.Wait_For_Server (1047);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (A_Piece_Performing_Effect.Id).Actual_Piece;

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 1,
         Message   => "Test Perform Piece Effect - that you manage to grant an effect");

      Piece.Client_Piece.Perform_Piece_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1048),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));

      Test_Piece.Wait_For_Server (1048);

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 0,
         Message   => "Test Perform Piece Effect when you do find an effect");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Piece_Effect_Find_Effect - exit");
      end if;
   end Test_Perform_Piece_Effect_Find_Effect;

   --
   procedure Test_Perform_Piece_Effect_Dont_Find_Effect
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)

   is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;

      A_Client_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Server_Piece : Piece.Server.Type_Piece_Access_Class;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Piece_Effect_Dont_Find_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 50);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1049),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1049);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1049).Result = Status.Ok,
         Message   =>
           "Perform Piece Effect, create piece " & Test_Piece.Test_List.all (1049).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Piece_Effect_Dont_Find_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 33;
      A_Server_Piece := Piece.Server.Find_Piece_In_List (A_Piece_Performing_Effect.Id).Actual_Piece;

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 0,
         Message   => "Test Perform Piece Effect - that you manage to grant an effect");

      Piece.Client_Piece.Perform_Piece_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1050),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));

      Test_Piece.Wait_For_Server (1050);

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 0,
         Message   => "Test Perform Piece Effect when you do find an effect");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Piece_Effect_Dont_Find_Effect - exit");
      end if;
   end Test_Perform_Piece_Effect_Dont_Find_Effect;

   procedure Test_Grant_Revoke_Patch_Effect (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
 is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;

      A_Client_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      An_Area        : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1) :=
        Hexagon.Area.Type_Action_Capabilities_A'
          (1 =>
             Hexagon.Type_Hexagon_Position'
               (True, Hexagon.Type_Hexagon_Numbers'(46), Hexagon.Type_Hexagon_Numbers'(53)));

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Grant_Revoke_Patch_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 46, 53);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1051),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1051);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1051).Result = Status.Ok,
         Message   =>
           "Test Grant Revoke Patch effect, create piece " &
           Test_Piece.Test_List.all (1051).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Grant_Revoke_Patch_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 34;
      Piece.Client_Piece.Grant_Patch_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1052),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         An_Area);

      Test_Piece.Wait_For_Server (1052);

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (46, 53);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Patch.all.Effects_Here) = 1,
         Message   => "Test Grant Patch effect");

      Piece.Client_Piece.Revoke_Patch_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1053),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0),
         An_Area);

      Test_Piece.Wait_For_Server (1053);
      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (46, 53);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Patch.all.Effects_Here) = 0,
         Message   => "Test Revoke Patch effect");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Grant_Revoke_Patch_Effect - exit");
      end if;
   end Test_Grant_Revoke_Patch_Effect;

   procedure Test_Grant_Revoke_Piece_Effect (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
 is
      use Hexagon;
      A_Piece_Performing_Effect : Piece.Type_Piece;
      A_Client_Patch            : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Piece            : Piece.Server.Type_Piece_Access_Class;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Grant_Revoke_Piece_Effect - enter");
      end if;

      A_Client_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 46, 53);

      Piece.Client_Piece.Create_Piece
        (Player_Id_1,
         Action.Type_Action_Type (1054),
         A_Piece_Performing_Effect,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Client_Patch.all));

      Test_Piece.Wait_For_Server (1054);

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1054).Result = Status.Ok,
         Message   =>
           "Test Grant Revoke Piece, create piece " & Test_Piece.Test_List.all (1054).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Grant_Revoke_Piece_Effect_01.html"));

      A_Piece_Performing_Effect.Id := 35;
      Piece.Client_Piece.Grant_Piece_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1055),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      Test_Piece.Wait_For_Server (1055);

      A_Server_Piece := Piece.Server.Find_Piece_In_List (A_Piece_Performing_Effect.Id).Actual_Piece;

      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 1,
         Message   => "Test Grant Piece Effect");

      Piece.Client_Piece.Revoke_Piece_Effect
        (Player_Id_1,
         Action.Type_Action_Type (1056),
         A_Piece_Performing_Effect,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));

      Test_Piece.Wait_For_Server (1056);

      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (A_Server_Piece.all.Effects_On_Piece) = 0,
         Message   => "Test Revoke Piece Effect");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Grant_Revoke_Piece_Effect - exit");
      end if;
   end Test_Grant_Revoke_Piece_Effect;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_Piece_1'Access,
         Name    => "Place a piece (sentry) on A, B = 1, 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_5_Pieces'Access,
         Name    => "Place 4 piece (knight) on A, B = 1, 1- to become 5 pieces");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_Piece_On_Occupied'Access,
         Name    => "Place a piece (knight) on A, B = 1, 1- an occupied spot.");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Piece'Access,
         Name    => "Remove a piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_1'Access,
         Name    => "Perform a move - Sentry A, B=1, 1 to A, B=2, 1.");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_2'Access,
         Name    => "Perform a move - Knight A, B=4, 4 to A, B=4,7.");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_To_Occupied_Patch'Access,
         Name    => "Perform a move - Trying to move to an occupied patch");

      --Place Piece not your turn
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_Piece_Not_Before_Create_Piece'Access,
         Name    => "Place a piece when it is not before create piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Piece_Not_Before_Remove_Piece'Access,
         Name    => "Remove a piece when it is not valid");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Not_Before_Perform_Move'Access,
         Name    => "Perform a move when it is not valid");

      -- from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack'Access,
         Name    => "Perform an attack on neighbour patch - Sentry A, B = 3,4 to A, B = 4, 4");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Create_Piece_Enemy_Patch'Access,
         Name    => "Test Create Piece and place on enemy Patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_And_Win'Access,
         Name    => "Perform Ranged Attack test an attack when player wins");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_And_Loose'Access,
         Name    => "Perform Ranged Attack test an attack when player loose");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Patch_Effect_Find_Effect'Access,
         Name    => "Perform Patch Effect when you find effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Patch_Effect_Dont_Find_Effect'Access,
         Name    => "Perform Patch Effect when you dont find effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Piece_Effect_Find_Effect'Access,
         Name    => "Perform Piece Effect when you find effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Piece_Effect_Dont_Find_Effect'Access,
         Name    => "Perform Piece Effect when you dont find effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Grant_Revoke_Patch_Effect'Access,
         Name    => "Test Grant Revoke Patch effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Grant_Revoke_Piece_Effect'Access,
         Name    => "Test Grant Revoke Piece effect");

   end Register_Tests;

end TC_Piece_Client_Piece;
