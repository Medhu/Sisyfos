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
with Hexagon;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Player;
with Piece;
with Piece.Client_Piece;
with Piece.Server;
with Text_IO;
with Hexagon.Server_Map;
with Hexagon.Area.Server_Area;
with Ada.Strings.Unbounded;
with Server.Server;
with Test_ServerRCI;
with Utilities;
with Landscape;
with Test_Piece;
with Status;
with Server;
with Observation;
with Action;

package body TC_Piece_Client_Piece is
   Verbose : constant Boolean := True;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;

   ------------
   -- Set_Up Case --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);

      Player_Name_List : Utilities.RemoteString_List.Vector;

      Adm_Status : Status.Type_Adm_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Set_Up - enter");
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

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Set_Up - exit");
      end if;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Client_Piece.Tear_Down - enter");
      end if;

      Server.Server.Stop;

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
      Ret_Status   : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      A_Piece.Id := 4;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_1_01.html"));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_1_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Couldn't place a piece on A, B = 1, 1 " & Ret_Status'Img);

      Piece.Server.Print_Pieces_In_Game;
      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).Pieces_Here);
      A_Piece.Id        := 4;
      A_Piece.Player_Id := 1;
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Element (Hexagon.Server_Map.A_Map (1, 1).Pieces_Here, 1) =
           A_Piece.Id,
         Message => "Piece on A, B is wrong " & Ret_Status'Img);

      Piece.Put (A_Piece);
      Piece.Put (Piece.Type_Piece (Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.all));
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
         Message => "Piece on A, B is wrong " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_1 - exit");
      end if;

   end Test_Place_Piece_1;

   procedure Test_Place_5_Pieces (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece1, A_Piece2, A_Piece3, A_Piece4, A_Piece5, A_Piece6 : Piece.Type_Piece;
      Ret_Status                                                 : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         A_Piece1,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece2,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece3,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece4,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece5,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece6,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Occupied,
         Message   => "Failed when placing a piece together with 5 others " & Ret_Status'Img);

      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here);

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
      Ret_Status   : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Place_Piece_On_Occupied - Status=" & Ret_Status'Img);
      end if;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Occupied,
         Message   => "Failed when placing a piece on an occupied patch " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_On_Occupied - exit");
      end if;

   end Test_Place_Piece_On_Occupied;

   procedure Test_Remove_Piece (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;
      Ret_Status   : Status.Type_Status;

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

      Active_Patch          := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 4;

      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here);

      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Couldn't remove piece from A, B =1,1 " & Ret_Status'Img);

      Text_IO.Put_Line ("frank b");
      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 5,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 5 pieces after Remove_Piece" & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_02.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 6;
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 4,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 4 pieces after Remove_Piece" & Ret_Status'Img);

      Text_IO.Put_Line ("frank c");
      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_03.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 7;
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 3,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 3 pieces after Remove_Piece" & Ret_Status'Img);

      Text_IO.Put_Line ("frank d");
      Landscape.Put_Pieces_Here (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_04.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 5;
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 2,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 2 pieces after Remove_Piece" & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_05.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 9;
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 1,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 1 pieces after Remove_Piece" & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_06.html"));

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;
      A_Piece.Id            := 8;
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 0,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 0 pieces after Remove_Piece" & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Remove_Piece_07.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece - exit");
      end if;
   end Test_Remove_Piece;

   procedure Test_Remove_Piece_From_Empty_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;
      Ret_Status   : Status.Type_Status;

      use Ada.Containers;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_From_Empty_Patch - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_From_Empty_Patch_01.html"));

      Active_Patch          := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      A_Piece.Id            := 1;
      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Player_Id     := 1;

      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Empty,
         Message   =>
           "Failed on status when tried to remove a piece from an empty patch " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (1, 1).all.Pieces_Here) = 0,
         Message =>
           "Piece on patch A,B=1,1 doesnt contain 'None' after Remove_Piece" & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_From_Empty_Patch_02.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_From_Empty_Patch - exit");
      end if;

   end Test_Remove_Piece_From_Empty_Patch;

   procedure Test_Perform_Move_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);
      A_Piece.Id := 12;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_1_02.html"));

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=1, 1 to A, B=2,1 did not succeed " & Ret_Status'Img);

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
      Ret_Status           : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      A_Piece.Id := 13;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_2_02.html"));

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_2_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A,B=4, 4 to A, B=4, 7 did not succeed " & Ret_Status'Img);

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

      Piece.Server.Print_Pieces_In_Game;

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

   procedure Test_Perform_Move_Not_Reachable (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Not_Reachable - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);

      To_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 11);
      A_Piece.Type_Of_Piece := Test_Piece.Knight_Piece;
      A_Piece.Player_Id     := Player_Id_1;
      A_Piece.Id            := 13;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Reachable_01.html"));

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Reachable_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Reachable,
         Message   =>
           "Perform move from A,B=4, 4 to A, B=4, 11. Shall not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "From patch does not have expected value");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 11).all.Pieces_Here) = 0,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Not_Reachable - exit");
      end if;

   end Test_Perform_Move_Not_Reachable;

   procedure Test_Place_Piece_Not_Your_Turn (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;
      Ret_Status   : Status.Type_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_Not_Your_Turn - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_Not_Your_Turn_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 8);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Place_Piece_Not_Your_Turn - Status=" & Ret_Status'Img);
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Place_Piece_Not_Your_Turn_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   =>
           "Failed on status when trying to place a piece and it is not your turn " &
           Ret_Status'Img);
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Place_Piece_Not_Your_Turn - exit");
      end if;

   end Test_Place_Piece_Not_Your_Turn;

   procedure Test_Remove_Piece_Not_Your_Turn (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;
      Ret_Status   : Status.Type_Status;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_Not_Your_Turn - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Your_Turn_01.html"));

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      A_Piece.Id        := 8;
      A_Piece.Player_Id := 1;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Your_Turn_02.html"));

      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_Not_Your_Turn_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   =>
           "Failed on status when tried to remove a piece when not your turn " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 8,
         Message =>
           "Piece on patch A, B=4, 7 doesnt contain 'Knight' after Remove_Piece and not players turn (should have kept the knight there) " &
           Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_Not_Your_Turn - exit");
      end if;

   end Test_Remove_Piece_Not_Your_Turn;

   procedure Test_Perform_Move_Not_Your_Turn (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece              : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Not_Your_Turn - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Your_Turn_01.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed on status when tried to create a piece " & Ret_Status'Img);

      A_Piece.Id        := 23;
      A_Piece.Player_Id := 1;

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 10);

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Not_Your_Turn_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   =>
           "Failed on status when tried to perform move a piece when not your turn " &
           Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 23,
         Message =>
           "Piece on patch A, B=4, 7 doesnt contain 'Knight' after Remove_Piece and not players turn (should have kept the knight there) " &
           Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Not_Your_Turn - exit");
      end if;

   end Test_Perform_Move_Not_Your_Turn;

   procedure Test_Perform_Attack_Neighbour (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      -- Ret : Boolean;
      Win : Player.Type_Player_Id;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Neighbour - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Neighbour_01.html"));

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Neighbour_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to place piece on 4,4 for player_id_1 " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Player_Id_1),
         Message   => "Failed to perform End Turn");

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 4);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Neighbour_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to place piece on 3,4 for player_id_2 " & Ret_Status'Img);

      From_Piece.Id := 25;
      To_Piece.Id   := 24;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         From_Piece,
         To_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Win,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Neighbour_04.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Failed on status when tried to remove a piece when not your turn " & Ret_Status'Img);

      AUnit.Assertions.Assert (Condition => Win = 2, Message => "Player_Id = 2 should win");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Neighbour - exit");
      end if;

   end Test_Perform_Attack_Neighbour;

   procedure Test_Perform_Attack_To_Not_Reachable_Patch
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      -- Ret : Boolean;
      Win : Player.Type_Player_Id;

      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Attack_To_Not_Reachable_Patch - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_To_Not_Reachable_Patch_01.html"));

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 10);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to place piece on 4, 10 for player_id_2 " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Player_Id_2),
         Message   => "Failed to perform End Turn");

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_To_Not_Reachable_Patch_02.html"));

      From_Piece.Id := 13;
      To_Piece.Id   := 26;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         From_Piece,
         To_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Win,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_To_Not_Reachable_Patch_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Reachable,
         Message   => "Should have received the status Not_Reachable " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Attack_To_Not_Reachable_Patch - exit");
      end if;

   end Test_Perform_Attack_To_Not_Reachable_Patch;

   procedure Test_Perform_Move_To_Occupied_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece,
      To_Piece1,
      To_Piece2,
      To_Piece3,
      To_Piece4,
      To_Piece5,
      To_Piece6  : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;

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
        (Action.Type_Action_Type(1),
         To_Piece1,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed while placing piece " & Ret_Status'Img);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece2,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed while placing piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_03.html"));

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece3,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed while placing piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_04.html"));

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece4,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed while placing piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_05.html"));

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece5,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_06.html"));

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece6,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to place piece on 3, 1 for player_id_2 " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_07.html"));

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);

      From_Piece.Id := 12;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_08.html"));

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         From_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_To_Occupied_Patch_09.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Should have received the status Target_Patch_Occupied " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_To_Occupied_Patch - exit");
      end if;

   end Test_Perform_Move_To_Occupied_Patch;

   procedure Test_Perform_Move_Inconsistent_Parameters
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece           : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Inconsistent_Parameters - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Inconsistent_Parameters_01.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 2);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      From_Piece.Id := 18;
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         From_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Inconsistent_Parameters_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Inconsistent_Parameters,
         Message   => "Should have received the status Inconsistent_Parameters " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Inconsistent_Parameters - exit");
      end if;

   end Test_Perform_Move_Inconsistent_Parameters;

   procedure Test_Perform_Attack_Several_Steps_Away
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      Win                  : Player.Type_Player_Id;

      use Player;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Several_Steps_Away - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Several_Steps_Away_01.html"));

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Player_Id_1),
         Message   => "Failed to perform End Turn");

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 8);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to place piece on 5, 8 for player_id_2 " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Player_Id_2),
         Message   => "Failed to perform End Turn");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Several_Steps_Away_02.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      From_Piece.Id := 13;
      To_Piece.Id   := 28;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         From_Piece,
         To_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Win,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Several_Steps_Away_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed to perform due to Status returned " & Ret_Status'Img);

      AUnit.Assertions.Assert (Condition => Win = 1, Message => "Player_Id = 1 should win");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Several_Steps_Away - exit");
      end if;

   end Test_Perform_Attack_Several_Steps_Away;

   procedure Test_Perform_Attack_Inconsistent_Parameters
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      Win                  : Player.Type_Player_Id;

      use Player;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Attack_Inconsistent_Parameters - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Inconsistent_Parameters_01.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Test_ServerRCI.End_Turn (Player_Id_1),
         Message   => "Failed to perform End Turn");

      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 2);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all), -- makes inconsistent
      --parameter
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Inconsistent_Parameters_02.html"));

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all), -- makes inconsistent
      --parameter
         Player_Id_2,
         Ret_Status);

      From_Piece.Id := 9;
      To_Piece.Id   := 18;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         From_Piece,
         To_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Win,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Inconsistent_Parameters_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Inconsistent_Parameters,
         Message   => "Failed. This call was with inconsistend parameters " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Attack_Inconsistent_Parameters - exit");
      end if;

   end Test_Perform_Attack_Inconsistent_Parameters;

   procedure Test_Perform_Attack_When_Not_Your_Turn
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      From_Piece, To_Piece : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      Win                  : Player.Type_Player_Id;

      Ret : Boolean;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_When_Not_Your_Turn - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_When_Not_Your_Turn_01.html"));

      Text_IO.Put_Line ("Halloiluken 1");
      To_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 38, 4);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         To_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed creating piece " & Ret_Status'Img);

      Text_IO.Put_Line ("Halloiluken 2");
      Ret := Test_ServerRCI.End_Turn (Player_Id_2);

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 38, 3);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         From_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      Ret := Test_ServerRCI.End_Turn (Player_Id_1);

      --

      Text_IO.Put_Line ("Halloiluken 3");

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Failed creating piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_When_Not_Your_Turn_02.html"));

      From_Piece.Id := 33;
      To_Piece.Id   := 32;

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         From_Piece,
         To_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Win,
         Ret_Status);

      Text_IO.Put_Line ("Halloiluken 4");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_When_Not_Your_Turn_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   => "Failed. It was not this players turn " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_When_Not_Your_Turn - exit");
      end if;

   end Test_Perform_Attack_When_Not_Your_Turn;

   procedure Test_Remove_Piece_When_Not_Your_Piece
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece      : Piece.Type_Piece;
      Ret_Status   : Status.Type_Status;

      Ret : Boolean;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_When_Not_Your_Piece - enter");
      end if;

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 38, 4);
--        Piece.Client_Piece.Create_Piece
--          (A_Piece,
--           Test_Piece.Sentry_Piece,
--           Piece.Fighting_Piece,
--           Landscape.Type_Patch (Active_Patch.all),
--           Player_Id_2,
--           Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_When_Not_Your_Piece001.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Remove_Piece_When_Not_Your_Piece002.html"),
         Map_Player_2);

      Ret        := Test_ServerRCI.End_Turn (Player_Id_2);
      A_Piece.Id := 32;
      Text_IO.Put_Line
        ("tc_piece_client_piece.Test_Remove_Piece_When_Not_Your_Piece før remove_piece");
      Piece.Client_Piece.Remove_Piece
        (Action.Type_Action_Type(1),
         A_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);
      Text_IO.Put_Line
        ("tc_piece_client_piece.Test_Remove_Piece_When_Not_Your_Piece etter remove_piece " &
         Ret_Status'Img);
      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Piece,
         Message   =>
           "Failed, got wrong status while removing piece while it is not your piece A, B = 38,4 " &
           Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Remove_Piece_When_Not_Your_Piece - exit");
      end if;

   end Test_Remove_Piece_When_Not_Your_Piece;

   procedure Test_Observation_Area (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;
      A_Piece_Server : Test_Piece.Type_My_Test_Piece;
      Ret_Status     : Status.Type_Status;
      Result         : Boolean;
      Res            : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      Ret : Boolean;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Observation_Area - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (Player_Id_1);

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 9, 2);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Observation_Area - After piece has been created");
      end if;

      A_Piece_Server.Id :=
        Landscape.Pieces_Here_List.Element (Hexagon.Server_Map.A_Map (9, 2).Pieces_Here, 1);
      A_Piece_Server.Type_Of_Piece := Test_Piece.Sentry_Piece;

      Res := Piece.Server.Observation_Area (Piece.Server.Type_Piece'Class (A_Piece_Server));

      if Res.all =
        Hexagon.Area.Type_Action_Capabilities'
          (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
         --
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
           Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2))
      then
         Result := True;
      else
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Observation Area returned was not as expected ");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Observation_Area - exit");
      end if;

   end Test_Observation_Area;

   procedure Test_Perform_Move_Path_Patches_Not_Neighbours
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;
      Ret        : Boolean;
      A_Path     : Hexagon.Path.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Path_Patches_Not_Neighbours - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn (2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Path_Patches_Not_Neighbours_01.html"));

      A_Piece.Id := 13;

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 8));
      -- These patches are not close to eachother
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 10));
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patches_Not_Connected,
         Message   =>
           "Perform move from A, B=5, 8 to A, B=5, 10 should get Patched_Not_Connected, but got " &
           Ret_Status'Img);

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 8));
      -- These patches are not close to eachother
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 9));
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patches_Not_Connected,
         Message   =>
           "Perform move from A, B=5, 8 to A, B=6, 9 should get Patched_Not_Connected, but got " &
           Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 8).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value - no move should have been done");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 10).all.Pieces_Here) = 0,
         Message =>
           "To patch does not have expected number of pieces  - no move should have been done");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 9).all.Pieces_Here) = 0,
         Message =>
           "To patch does not have expected number of pieces  - no move should have been done");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Move_Path_Patches_Not_Neighbours - exit");
      end if;

   end Test_Perform_Move_Path_Patches_Not_Neighbours;

   procedure Test_Perform_Move_Path_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;
      Ret        : Boolean;
      A_Path     : Hexagon.Path.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_1 - enter");
      end if;

      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 8));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 9));
      Ret := Test_ServerRCI.End_Turn (2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_1_01.html"));

      A_Piece.Id := 13;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_1_02.html"));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=5, 8 to A, B=5, 9 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 8).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 9).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_1 - exit");
      end if;

   end Test_Perform_Move_Path_1;

   procedure Test_Perform_Move_Path_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;
      Ret        : Boolean;
      A_Path     : Hexagon.Path.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_2 - enter");
      end if;

      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 9));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 10));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 11));
      Ret := Test_ServerRCI.End_Turn (2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_2_01.html"));

      A_Piece.Id := 13;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_2_02.html"));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_2_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Perform move from A, B=5, 9 to A, B=4, 11 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 9).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 11).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

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

   end Test_Perform_Move_Path_2;

   procedure Test_Perform_Move_Path_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;
      Ret        : Boolean;
      A_Path     : Hexagon.Path.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_3 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_01.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 11));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 11));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 11));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 10));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_02.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 10));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 9));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_03.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 9));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 8));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_04.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 8));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 7));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_05.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_06.html"));

      Piece.Server.Print_Pieces_In_Game;

      Ret := Test_ServerRCI.End_Turn (1);
      Ret := Test_ServerRCI.End_Turn (2);

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_3_07.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=4, 6 to A, B=4, 7 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_3 - exit");
      end if;

   end Test_Perform_Move_Path_3;

   procedure Test_Perform_Move_Path_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      Ret_Status     : Status.Type_Status;
      A_Path         : Hexagon.Path.Vector;
      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_4 - enter");
      end if;

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));

      A_Piece.Id := 13;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      A_Piece.Id := 13;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_4_01.html"));

      --
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 7);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      --
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_4_02.html"));

      A_Piece.Id := 13;
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));

      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_4_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Perform move from A, B=4, 6 to A, B=4, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 6).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 8).all.Pieces_Here) = 0,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_4 - exit");
      end if;

   end Test_Perform_Move_Path_4;

   procedure Test_Perform_Move_Path_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      Ret_Status     : Status.Type_Status;
      Ret            : Boolean;
      A_Path         : Hexagon.Path.Vector;
      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_5 - enter");
      end if;

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 6));

      A_Piece.Id := 13;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Ret          := Test_ServerRCI.End_Turn (1);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_5_01.html"));

      Ret := Test_ServerRCI.End_Turn (2);
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 8));

      A_Piece.Id := 13;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_5_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Perform move from A, B=5, 6 to A, B=5, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 6).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 8).all.Pieces_Here) = 0,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_5 - exit");
      end if;

   end Test_Perform_Move_Path_5;

   procedure Test_Perform_Move_Path_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece        : Piece.Type_Piece;
      Ret_Status     : Status.Type_Status;
      Ret            : Boolean;
      A_Path         : Hexagon.Path.Vector;
      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_6 - enter");
      end if;

      Ret          := Test_ServerRCI.End_Turn (1);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 45, 43);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.A_Map (45, 45).all.Landscape_Here := Test_Piece.Landscape_Water;
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_6_01.html"));

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 43));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 44));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 45));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 45, 46));

      A_Piece.Id := 43;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_2, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_Path_6_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Bad_Terrain,
         Message   =>
           "Perform move from A, B=45, 43 to A, B=45, 46 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (45, 43).all.Pieces_Here) =
           1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (45, 44).all.Pieces_Here) =
           0,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (45, 45).all.Pieces_Here) =
           0,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (45, 46).all.Pieces_Here) =
           0,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece.Id).Actual_Piece.Id = 43,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_6 - exit");
      end if;

   end Test_Perform_Move_Path_6;

   procedure Test_Perform_Attack_Path_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacker, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                         : Status.Type_Status;
      Ret                                : Boolean;
      A_Path                             : Hexagon.Path.Vector;
      Winner                             : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_1 - enter");
      end if;

      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 7));
      Ret := Test_ServerRCI.End_Turn (2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_1_01.html"));

      A_Piece_Attacker.Id := 13;
      A_Piece_Attacked.Id := 37;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_1_02.html"));

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacker,
         A_Piece_Attacked,
         A_Path,
         Player_Id_1,
         Winner,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=5, 8 to A, B=5, 9 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 7).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_1 - exit");
      end if;

   end Test_Perform_Attack_Path_1;

   procedure Test_Create_Piece_Enemy_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Ret_Status     : Status.Type_Status;
      Ret            : Boolean;
      A_Path         : Hexagon.Path.Vector;
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

      Ret := Test_ServerRCI.End_Turn (1);
      AUnit.Assertions.Assert (Condition => Ret, Message => "Could not end turn");

      -- Piece 13 belongs to player 1 - and this new piece belongs to player two
      -- this  new pieve shall not be allowed to be places on same paths as the first.
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 7);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Occupied,
         Message   =>
           "Create Piece on enemy patch should fail with Patch_Occupied, we got " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Create_Piece_Enemy_Patch_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Create_Piece_Enemy_Patch - exit");
      end if;
   end Test_Create_Piece_Enemy_Patch;

   procedure Test_Perform_Attack_Path_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacker, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                         : Status.Type_Status;
      --Ret                                 : Boolean ;
      A_Path         : Hexagon.Path.Vector;
      Winner         : Player.Type_Player_Id;
      Active_Patch   : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_2 - enter");
      end if;

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 6);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Create Piece did not succeed " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_2_01.html"));

      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 5, 7));

      A_Piece_Attacker.Id := 39;
      A_Piece_Attacked.Id := 13;
      Active_Patch        := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 7);

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacker,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_2_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=5, 6 to A, B=5, 7 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (5, 7).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 13,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_2 - exit");
      end if;

   end Test_Perform_Attack_Path_2;

   procedure Test_Perform_Attack_Path_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacker, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                         : Status.Type_Status;
      Ret                                : Boolean;
      A_Path                             : Hexagon.Path.Vector;
      Winner                             : Player.Type_Player_Id;
      Active_Patch                       : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client                     : Piece.Type_Piece;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_3 - enter");
      end if;

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 8);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_3_01.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Create Piece did not succeed " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (2);
      AUnit.Assertions.Assert (Condition => Ret, Message => "Could not end turn");

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 6);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_3_02.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Create Piece did not succeed " & Ret_Status'Img);

      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));

      Ret := Test_ServerRCI.End_Turn (2);

      A_Piece_Attacker.Id := 41;
      A_Piece_Attacked.Id := 40;

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacker,
         A_Piece_Attacked,
         A_Path,
         Player_Id_1,
         Winner,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_3_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Perform move from A, B=5, 6 to A, B=5, 7 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 6).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Id = 41 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
             Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 40,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_3 - exit");
      end if;

   end Test_Perform_Attack_Path_3;

   procedure Test_Perform_Attack_Path_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacker, A_Piece_Attacked, A_Piece : Piece.Type_Piece;
      Ret_Status                                  : Status.Type_Status;
      A_Path                                      : Hexagon.Path.Vector;
      Winner                                      : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_4 - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_4_01.html"));

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 3, 7));
      A_Piece.Id := 27;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece, A_Path, Player_Id_1, Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_4_02.html"));

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));

      A_Piece_Attacker.Id := 41;
      A_Piece_Attacked.Id := 40;

      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacker,
         A_Piece_Attacked,
         A_Path,
         Player_Id_1,
         Winner,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_4_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=5, 6 to A, B=5, 7 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacker.Id).Actual_Piece.Id = 41,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_4 - exit");
      end if;

   end Test_Perform_Attack_Path_4;

   procedure Test_Perform_Attack_Path_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;
      Ret                                 : Boolean;
      A_Path                              : Hexagon.Path.Vector;
      Active_Patch                        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client                      : Piece.Type_Piece;
      Winner                              : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_5 - enter");
      end if;

      Ret          := Test_ServerRCI.End_Turn (1);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 6);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_5_01.html"));

      --    Ret := Test_ServerRCI.End_Turn(2);
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 4, 8));

      A_Piece_Attacking.Id := 42;
      A_Piece_Attacked.Id  := 41;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_5_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Perform move from A, B=5, 6 to A, B=5, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 6).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (4, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 41 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Type_Of_Piece =
             Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Id = 42,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_5 - exit");
      end if;

   end Test_Perform_Attack_Path_5;

   procedure Test_Perform_Attack_Path_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;
      Ret                                 : Boolean;
      A_Path                              : Hexagon.Path.Vector;
      Active_Patch                        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client                      : Piece.Type_Piece;
      Winner                              : Player.Type_Player_Id;

      Vis_Frame_1, Vis_Frame_2 : Observation.Frames.Piece_Visibility_Frames.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_6 - enter");
      end if;

      Hexagon.Server_Map.A_Map (6, 7).all.Landscape_Here := Test_Piece.Landscape_Water;
      Ret                                                := Test_ServerRCI.End_Turn (1);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 6, 6);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_6_01.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Created a piece " & Ret_Status'Img);

      Ret          := Test_ServerRCI.End_Turn (2);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 6, 8);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_6_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Created a piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (1);
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 8));

      A_Piece_Attacking.Id := 44;
      A_Piece_Attacked.Id  := 45;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_6_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Patch_Bad_Terrain,
         Message   => "Perform move from A, B=6, 6 to A, B=6, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 6).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 45 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Type_Of_Piece =
             Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Id = 44,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_6 - exit");
      end if;

   end Test_Perform_Attack_Path_6;

   procedure Test_Perform_Attack_Path_7 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;
      Ret                                 : Boolean;
      A_Path                              : Hexagon.Path.Vector;
      Active_Patch                        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client                      : Piece.Type_Piece;
      Winner                              : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_7 - enter");
      end if;

      Hexagon.Server_Map.A_Map (6, 7).all.Landscape_Here := Test_Piece.Landscape_Grass;

      Ret          := Test_ServerRCI.End_Turn (2);
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 6, 8);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_1,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_7_01.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Created a piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (1);
      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 8));

      A_Piece_Attacking.Id := 44;
      A_Piece_Attacked.Id  := 46;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_7_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=6, 6 to A, B=6, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 7).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 8).all.Pieces_Here) = 2,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 46 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Type_Of_Piece =
             Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Id = 44,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_7 - exit");
      end if;

   end Test_Perform_Attack_Path_7;

   procedure Test_Perform_Attack_Path_8 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;
      A_Path                              : Hexagon.Path.Vector;
      Active_Patch                        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Piece_Client                      : Piece.Type_Piece;
      Winner                              : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_8 - enter");
      end if;

      Hexagon.Server_Map.A_Map (6, 7).all.Landscape_Here := Test_Piece.Landscape_Grass;

      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 6, 6);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Client,
         Test_Piece.Knight_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (Active_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_8_01.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Created a piece " & Ret_Status'Img);

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 6, 8));

      A_Piece_Attacking.Id := 47;
      A_Piece_Attacked.Id  := 46;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_Path_8_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform move from A, B=6, 6 to A, B=6, 8 did not succeed " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 6).all.Pieces_Here) = 0,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 7).all.Pieces_Here) = 1,
         Message => "From patch does not have expected value ");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Length (Hexagon.Server_Map.A_Map (6, 8).all.Pieces_Here) = 1,
         Message => "To patch does not have expected number of pieces");

      AUnit.Assertions.Assert
        (Condition =>
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Type_Of_Piece =
           Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Player_Id = 1 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacked.Id).Actual_Piece.Id = 46 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Type_Of_Piece =
             Test_Piece.Knight_Piece and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Player_Id = 2 and
           Piece.Server.Find_Piece_In_List (A_Piece_Attacking.Id).Actual_Piece.Id = 47,
         Message => "To patch does not have expected value");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_8 - exit");
      end if;

   end Test_Perform_Attack_Path_8;

   procedure Test_Perform_Move_With_Tower (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Tower_Moving : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      From_Patch, To_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_With_Tower - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 10, 6);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 9, 7);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Tower_Moving,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Move with tower did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_With_Tower_01.html"));

      A_Piece_Tower_Moving.Id := 48;
      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type(1),
         A_Piece_Tower_Moving,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Move_With_Tower_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Move with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_With_Tower - exit");
      end if;

   end Test_Perform_Move_With_Tower;

   procedure Test_Perform_Attack_With_Tower (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Tower_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                                : Status.Type_Status;
      From_Patch, To_Patch                      : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Winner                                    : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_With_Tower - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 10, 6);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 9, 7);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacked,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Move with tower did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_With_Tower_01.html"));

      A_Piece_Tower_Attacking.Id := 48;
      A_Piece_Attacked.Id        := 49;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Tower_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_piece_client_piece-Test_Perform_Attack_With_Tower_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Move with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_With_Tower - exit");
      end if;

   end Test_Perform_Attack_With_Tower;

   procedure Test_Perform_Move_Path_With_Tower (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Tower_Moving : Piece.Type_Piece;
      Ret_Status           : Status.Type_Status;
      A_Path               : Hexagon.Path.Vector;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_With_Tower - enter");
      end if;

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 10, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 9, 7));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Path_With_Tower_01.html"));

      A_Piece_Tower_Moving.Id := 48;
      Piece.Client_Piece.Perform_Move (Action.Type_Action_Type(1), A_Piece_Tower_Moving, A_Path, Player_Id_2, Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Move_Path_With_Tower_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Move with tower using path did not return expected result, we got " &
           Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Move_Path_With_Tower - exit");
      end if;

   end Test_Perform_Move_Path_With_Tower;

   procedure Test_Perform_Attack_Path_With_Tower (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Tower_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                                : Status.Type_Status;
      A_Path                                    : Hexagon.Path.Vector;
      Winner                                    : Player.Type_Player_Id;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_With_Tower - enter");
      end if;

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 10, 6));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 9, 7));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Path_With_Tower_01.html"));

      -- Use a tower during attack
      A_Piece_Tower_Attacking.Id := 48;
      A_Piece_Attacked.Id        := 49;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Tower_Attacking,
         A_Piece_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Path_With_Tower_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Attack with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Path_With_Tower - exit");
      end if;

   end Test_Perform_Attack_Path_With_Tower;

   procedure Test_Perform_Attack_Tower_With_Path (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Tower_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;
      A_Path                              : Hexagon.Path.Vector;
      Winner                              : Player.Type_Player_Id;
      To_Patch, From_Patch                : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Tower_With_Path - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 9, 7);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 10, 6);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Attack on a tower did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Path.Clear (A_Path);
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 9, 7));
      Hexagon.Path.Append (A_Path, Hexagon.Type_Hexagon_Position'(True, 10, 6));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Tower_With_Path_01.html"));

      -- Use a sentry to attack a tower
      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 50;
      A_Tower_Attacked.Id  := 48;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Tower_Attacked,
         A_Path,
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Tower_With_Path_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Attack with tower did not return expected result, we got " & Ret_Status'Img);

      A_Piece_Attacking.Id := 50;
      A_Tower_Attacked.Id  := 48;
      Piece.Client_Piece.Perform_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Tower_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Attack_Tower_With_Path_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   =>
           "Perform Attack with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Attack_Tower_With_Path - exit");
      end if;

   end Test_Perform_Attack_Tower_With_Path;

   procedure Test_Perform_Ranged_Attack_Inconsistent_Parameter
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Tower_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Inconsistent_Parameter - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 28);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 31);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Ranged Attack test did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Inconsistent_Parameter_01.html"));

      -- Use a sentry to attack a tower
      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 51;
      A_Tower_Attacked.Id  := 48; -- purposly wrong piece, that does not stand os (A, B) = (5, 31)
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Tower_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Inconsistent_Parameter_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Inconsistent_Parameters,
         Message   =>
           "Perform Attack with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Inconsistent_Parameter - exit");
      end if;

   end Test_Perform_Ranged_Attack_Inconsistent_Parameter;

   procedure Test_Perform_Ranged_Attack_Attacking_Tower
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Tower_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Ret : Boolean;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Attacking_Tower - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 3, 28);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 31);

      Ret := Test_ServerRCI.End_Turn (Player_Id_2);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Tower_House,
         Piece.House_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Ranged Attack test did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Attacking_Tower_01.html"));

      -- Use a sentry to attack a tower
      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 51;
      A_Tower_Attacked.Id  := 52;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Tower_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Attacking_Tower_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Expected_Fighting_Piece,
         Message   => "Perform Attack when attacking a tower, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Attacing_Tower - exit");
      end if;

   end Test_Perform_Ranged_Attack_Attacking_Tower;

   procedure Test_Perform_Ranged_Attack_Not_Players_Piece
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Ret : Boolean;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Not_Players_Piece - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 29);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 32);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Ranged Attack test did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (Player_Id_1);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacked,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Ranged Attack test did not return expected result when creating piece, we got " &
           Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Not_Players_Piece_01.html"));

      -- Use a sentry to attack a tower
      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 53;
      A_Piece_Attacked.Id  := 54;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Not_Players_Piece_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Piece,
         Message   => "Perform Attack not using players piece we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Not_Players_Piece - exit");
      end if;

   end Test_Perform_Ranged_Attack_Not_Players_Piece;

   procedure Test_Perform_Ranged_Attack_Not_Players_Turn
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Ret : Boolean;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Not_Players_Turn - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 29);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 32);

      Ret := Test_ServerRCI.End_Turn (Player_Id_2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Not_Players_Turn_01.html"));

      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 53;
      A_Piece_Attacked.Id  := 54;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Not_Players_Turn_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   => "Perform Attack when not players turn, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Not_Players_Turn - exit");
      end if;

   end Test_Perform_Ranged_Attack_Not_Players_Turn;

   procedure Test_Perform_Ranged_Attack_Players_Own_Piece
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Players_Own_Piece - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 29);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 30);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   =>
           "Perform Ranged Attack test attacking your own piece, create piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Players_Own_Piece_01.html"));

      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 53;
      A_Piece_Attacked.Id  := 55;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_Players_Own_Piece_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Players_Attacks_Himself,
         Message   =>
           "Perform Attack with tower did not return expected result, we got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line
           ("tc_piece_client_piece.Test_Perform_Ranged_Attack_Players_Own_Piece - exit");
      end if;

   end Test_Perform_Ranged_Attack_Players_Own_Piece;

   procedure Test_Perform_Ranged_Attack_And_Win (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner               : Player.Type_Player_Id;
      To_Patch, From_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      To_Server_Patch, From_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Ret : Boolean;
      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Win - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 14, 18);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 16, 16);

      From_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 18);
      To_Server_Patch   := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 16);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Perform Ranged Attack test attacking and win, create piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (Player_Id_1);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacked,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Perform Ranged Attack test attacking and win, create piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Win_01.html"));

      Ret := Test_ServerRCI.End_Turn (Player_Id_2);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (56))),
         Message => "Didn't find expected piece on From patch in server map");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (57))),
         Message => "Didn't find expected piece on To patch in server map");

      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 56;
      A_Piece_Attacked.Id  := 57;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Win_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform Attack and trying to win, we got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Winner = Player_Id_1,
         Message   => "Perform Attack the winner is " & Winner'Img);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (56))),
         Message =>
           "Didn't find expected piece on From patch in server map after perform ranged attack");

      AUnit.Assertions.Assert
        (Condition =>
           not Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (57))),
         Message =>
           "Didn't find expected piece on To patch in server map after perform ranged attack");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Win - exit");
      end if;

   end Test_Perform_Ranged_Attack_And_Win;

   procedure Test_Perform_Ranged_Attack_And_Loose
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece_Attacking, A_Piece_Attacked : Piece.Type_Piece;
      Ret_Status                          : Status.Type_Status;

      Winner                             : Player.Type_Player_Id;
      To_Patch, From_Patch               : Hexagon.Client_Map.Type_Client_Patch_Adress;
      To_Server_Patch, From_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Ret : Boolean;
      use Ada.Containers;
      use Piece;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Loose - enter");
      end if;

      From_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 14, 18);
      To_Patch   := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 16, 16);

      From_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 18);
      To_Server_Patch   := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 16);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         Test_Piece.Bowman_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (From_Patch.all),
         Player_Id_1,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Perform Ranged Attack test attacking and win, create piece " & Ret_Status'Img);

      Ret := Test_ServerRCI.End_Turn (Player_Id_1);

      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         A_Piece_Attacked,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_2,
         Ret_Status);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message => "Perform Ranged Attack test attacking and win, create piece " & Ret_Status'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Loose_01.html"));

      Ret := Test_ServerRCI.End_Turn (Player_Id_2);

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (58))),
         Message => "Didn't find expected piece on From patch");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (59))),
         Message => "Didn't find expected piece on To patch");

      Piece.Server.Print_Pieces_In_Game;
      A_Piece_Attacking.Id := 58;
      A_Piece_Attacked.Id  := 59;
      Piece.Client_Piece.Perform_Ranged_Attack
        (Action.Type_Action_Type(1),
         A_Piece_Attacking,
         A_Piece_Attacked,
         Landscape.Type_Patch (From_Patch.all),
         Landscape.Type_Patch (To_Patch.all),
         Player_Id_1,
         Winner,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_piece_client_piece-Test_Perform_Ranged_Attack_And_Loose_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Perform Attack and loosing, we got " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Winner = Player.Undefined_Player_Id, -- no winner in this situaion
         Message   =>
           "Perform Attack the winner is " & Winner'Img & " we expected undefined winner");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (From_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (58))),
         Message => "Didn't find expected piece on From patch");

      AUnit.Assertions.Assert
        (Condition =>
           Landscape.Pieces_Here_List.Has_Element
             (Landscape.Pieces_Here_List.Find
                (To_Server_Patch.all.Pieces_Here,
                 Piece.Type_Piece_Id (59))),
         Message => "Didn't find expected piece on To patch");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_client_piece.Test_Perform_Ranged_Attack_And_Loose - exit");
      end if;
   end Test_Perform_Ranged_Attack_And_Loose;

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
         Routine => Test_Remove_Piece_From_Empty_Patch'Access,
         Name    => "Remove a piece from an empty patch");
      --
      -- Perform Move
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
         Routine => Test_Perform_Move_Not_Reachable'Access,
         Name    => "Perform a move - Knight A, B=4, 7 to A, B=4,10. Not reachable.");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_To_Occupied_Patch'Access,
         Name    => "Perform a move - Trying to move to an occupied patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Inconsistent_Parameters'Access,
         Name    => "Perform a move with inconsistent parameters - Knight instead of sentry");
      --Place Piece not your turn
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_Piece_Not_Your_Turn'Access,
         Name    => "Place a piece when it is not your turn");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Piece_Not_Your_Turn'Access,
         Name    => "Remove a piece when it is not your turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Not_Your_Turn'Access,
         Name    => "Perform a move when it is not your turn");

      -- from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Neighbour'Access,
         Name    => "Perform an attack on neighbour patch - Sentry A, B = 3,4 to A, B = 4, 4");

      --from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_To_Not_Reachable_Patch'Access,
         Name    =>
           "Perform an attack on patch that is not reachable - Knight A, B = 4, 7 to A, B = 4, 10");

      -- from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Several_Steps_Away'Access,
         Name    =>
           "Perform an attack on patch several steps away - Knight A, B = 4, 7 to A, B = 5, 8");

      -- from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Inconsistent_Parameters'Access,
         Name    =>
           "Perform an attack with inconsistent parameters. Sending parameter with Knight instead of Sentry");

      -- from, to
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_When_Not_Your_Turn'Access,
         Name    => "Perform an attack when it is not your turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Piece_When_Not_Your_Piece'Access,
         Name    => "Remove a piece when it is not your turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Observation_Area'Access,
         Name    => "Test Observation Area");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_Patches_Not_Neighbours'Access,
         Name    =>
           "Test Perform Move check that patches are neighbour in paths that are sent to Perform_Move");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_1'Access,
         Name    => "Test Perform Move with a path 1 - one step");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_2'Access,
         Name    => "Test Perform Move with a path 2 - several steps");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_3'Access,
         Name    =>
           "Test Perform Move with a path 3 - several steps - through populated patch (not full)");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_4'Access,
         Name    =>
           "Test Perform Move with a path 4 - several steps - through full patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_5'Access,
         Name    =>
           "Test Perform Move with a path 5 - several steps - through enemy patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_1'Access,
         Name    => "Test Perform Attack with a path 1 - one step");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Create_Piece_Enemy_Patch'Access,
         Name    => "Test Create Piece and place on enemy Patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_2'Access,
         Name    => "Test Perform Attack with a path 2 - several steps");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_3'Access,
         Name    =>
           "Test Perform Attack with a path 3 - several steps - through populated patch (full)");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_4'Access,
         Name    =>
           "Test Perform Attack with a path 4 - several steps - through populated patch (not full)");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_5'Access,
         Name    =>
           "Test Perform Attack with a path 5 - several steps - through enemy patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_6'Access,
         Name    => "Test Perform Move trying to cross water - several steps");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_6'Access,
         Name    => "Test Perform Attack trying to cross water - several steps");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_7'Access,
         Name => "Test Perform Attack against a patch with two pieces, and lose");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_8'Access,
         Name => "Test Perform Attack against a patch with two pieces, and win");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_With_Tower'Access,
         Name    =>
           "Test Perform Move (from, to) with a tower. This should return a Status about the problem and not cause crash");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_With_Tower'Access,
         Name    =>
           "Test Perform Attack (from, to) with a tower. This should return a Status about the problem and not cause crash");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Move_Path_With_Tower'Access,
         Name    =>
           "Test Perform Move (path) with a tower. This should return a Status about the problem and not cause crash");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Path_With_Tower'Access,
         Name    =>
           "Test Perform Attack (path) with a tower (use a tower during attack)). This should return a Status about the problem and not cause crash");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Attack_Tower_With_Path'Access,
         Name    =>
           "Test Perform Attack on a tower with a path (use a sentry to attack a tower). This should return a Status about the problem and not cause crash");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_Inconsistent_Parameter'Access,
         Name    => "Perform Ranged Attack test Inconsistent parameters");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_Attacking_Tower'Access,
         Name    => "Perform Ranged Attack test an attack on a tower");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_Not_Players_Piece'Access,
         Name    => "Perform Ranged Attack test an attack Not using players piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_Not_Players_Turn'Access,
         Name    => "Perform Ranged Attack test an attack Not on players turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_Players_Own_Piece'Access,
         Name    => "Perform Ranged Attack test an attack when player attacks his own piece");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_And_Win'Access,
         Name    => "Perform Ranged Attack test an attack when player wins");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Ranged_Attack_And_Loose'Access,
         Name    => "Perform Ranged Attack test an attack when player loose");

   end Register_Tests;

end TC_Piece_Client_Piece;
