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

with Text_IO;
with AUnit.Assertions;
with Hexagon.Server_Map;
with Player;
with Piece;
with Piece.Server;
with Ada.Strings.Unbounded;
with Ada.Containers;
with Landscape;
with Status;
with Test_Piece;
with Utilities;
with Test_ServerRCI;
with Server.Server;
with Action;
with Effect;

package body TC_Hexagon_Server_Map is

   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2 : Player.Type_Player_Id;
   Test_List : aliased Test_Piece.Type_Test_List;
   Command_Line : Utilities.RemoteString.Type_Command_Parameters;

   type Type_Control_Map is array (1 .. 100, 1 .. 100) of Integer;
   Control_Array : Type_Control_Map;
   Visit_Counter : Integer;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);

      Test_Class1 : Test_Piece.Type_My_Test_Piece_Access_Class;
      Test_Class2 : Test_Piece.Type_My_Test_House_Access_Class;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line  : Utilities.RemoteString.Type_Command_Parameters;

      Adm_Status : Status.Type_Adm_Status;
   begin

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
   end Set_Up_Case;

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

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      Server.Server.Stop;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test that the server side map is working");
   end Name;

   --------------------
   -- Test Scenarios --
   --------------------

   procedure Test_Numerate_1_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
   begin

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Server_Map.A_Map (1, 1).all.Pos.P_Valid and
           Hexagon.Server_Map.A_Map (1, 1).all.Pos.A = 1 and
           Hexagon.Server_Map.A_Map (1, 1).all.Pos.B = 1,
         Message => "Sample test of A,B = 1,1");
   end Test_Numerate_1_1;

   procedure Test_Numerate_100_100 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
   begin
      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Server_Map.A_Map (100, 100).all.Pos.P_Valid and
           Hexagon.Server_Map.A_Map (100, 100).all.Pos.A = 100 and
           Hexagon.Server_Map.A_Map (100, 100).all.Pos.B = 100,
         Message => "Sample test of A,B = 100,100");
   end Test_Numerate_100_100;

   procedure Test_Numerate_1_100 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
   begin
      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Server_Map.A_Map (100, 100).all.Pos.P_Valid and
           Hexagon.Server_Map.A_Map (1, 100).all.Pos.A = 1 and
           Hexagon.Server_Map.A_Map (1, 100).all.Pos.B = 100,
         Message => "Sample test of A,B = 1,100");
   end Test_Numerate_1_100;

   procedure Test_Numerate_100_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
   begin
      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Server_Map.A_Map (100, 1).all.Pos.P_Valid and
           Hexagon.Server_Map.A_Map (1, 100).all.Pos.A = 100 and
           Hexagon.Server_Map.A_Map (1, 100).all.Pos.B = 1,
         Message => "Sample test of A,B = 100,1");
   end Test_Numerate_100_1;

   procedure Test_Numerate_4_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
   begin
      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Server_Map.A_Map (4, 4).all.Pos.P_Valid and
           Hexagon.Server_Map.A_Map (4, 4).all.Pos.A = 4 and
           Hexagon.Server_Map.A_Map (4, 4).all.Pos.B = 4,
         Message => "Sample test of A,B = 4,4");
   end Test_Numerate_4_4;

   procedure Test_Numerate_Neighbours_Middle (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (4, 4).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 5 and
           Neighbours (1).all.Pos.B = 4 and
           Neighbours (2).all.Pos.P_Valid and
           Neighbours (2).all.Pos.A = 5 and
           Neighbours (2).all.Pos.B = 3 and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 4 and
           Neighbours (3).all.Pos.B = 3 and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 3 and
           Neighbours (4).all.Pos.B = 4 and
           Neighbours (5).all.Pos.P_Valid and
           Neighbours (5).all.Pos.A = 3 and
           Neighbours (5).all.Pos.B = 5 and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 4 and
           Neighbours (6).all.Pos.B = 5,
         Message => "Sample test of neighbours for A, B = 4, 4");
   end Test_Numerate_Neighbours_Middle;

   procedure Test_Numerate_Neighbours_Left_Edge (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (1, 8).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 2 and
           Neighbours (1).all.Pos.B = 8 and
           Neighbours (2).all.Pos.P_Valid and
           Neighbours (2).all.Pos.A = 2 and
           Neighbours (2).all.Pos.B = 7 and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 1 and
           Neighbours (3).all.Pos.B = 7 and
           Neighbours (4) = null and
           Neighbours (5) = null and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 1 and
           Neighbours (6).all.Pos.B = 9,
         Message => "Sample test of neighbours for A, B = 1, 8");
   end Test_Numerate_Neighbours_Left_Edge;

   procedure Test_Numerate_Neighbours_Bottom_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (9, 1).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 10 and
           Neighbours (1).all.Pos.B = 1 and
           Neighbours (2) = null and
           Neighbours (3) = null and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 8 and
           Neighbours (4).all.Pos.B = 1 and
           Neighbours (5).all.Pos.P_Valid and
           Neighbours (5).all.Pos.A = 8 and
           Neighbours (5).all.Pos.B = 2 and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 9 and
           Neighbours (6).all.Pos.B = 2,
         Message => "Sample test of neighbours for A, B = 9, 1");
   end Test_Numerate_Neighbours_Bottom_Edge;

   procedure Test_Numerate_Neighbours_Right_Edge (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (100, 97).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1) = null and
           Neighbours (2) = null and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 100 and
           Neighbours (3).all.Pos.B = 96 and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 99 and
           Neighbours (4).all.Pos.B = 97 and
           Neighbours (5).all.Pos.P_Valid and
           Neighbours (5).all.Pos.A = 99 and
           Neighbours (5).all.Pos.B = 98 and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 100 and
           Neighbours (6).all.Pos.B = 98,
         Message => "Sample test of neighbours for A, B = 100, 97");
   end Test_Numerate_Neighbours_Right_Edge;

   procedure Test_Numerate_Neighbours_Top_Edge (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (95, 100).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 96 and
           Neighbours (1).all.Pos.B = 100 and
           Neighbours (2).all.Pos.P_Valid and
           Neighbours (2).all.Pos.A = 96 and
           Neighbours (2).all.Pos.B = 99 and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 95 and
           Neighbours (3).all.Pos.B = 99 and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 94 and
           Neighbours (4).all.Pos.B = 100 and
           Neighbours (5) = null and
           Neighbours (6) = null,
         Message => "Sample test of neighbours for A, B = 95, 100");
   end Test_Numerate_Neighbours_Top_Edge;

   procedure Test_Numerate_Neighbours_Bottom_Left_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (1, 1).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 2 and
           Neighbours (1).all.Pos.B = 1 and
           Neighbours (2) = null and
           Neighbours (3) = null and
           Neighbours (4) = null and
           Neighbours (5) = null and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 1 and
           Neighbours (6).all.Pos.B = 2,
         Message => "Sample test of neighbours for A, B = 1, 1");
   end Test_Numerate_Neighbours_Bottom_Left_Edge;

   procedure Test_Numerate_Neighbours_Bottom_Right_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (100, 1).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1) = null and
           Neighbours (2) = null and
           Neighbours (3) = null and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 99 and
           Neighbours (4).all.Pos.B = 1 and
           Neighbours (5).all.Pos.P_Valid and
           Neighbours (5).all.Pos.A = 99 and
           Neighbours (5).all.Pos.B = 2 and
           Neighbours (6).all.Pos.P_Valid and
           Neighbours (6).all.Pos.A = 100 and
           Neighbours (6).all.Pos.B = 2,
         Message => "Sample test of neighbours for A, B = 100, 1");
   end Test_Numerate_Neighbours_Bottom_Right_Edge;

   procedure Test_Numerate_Neighbours_Top_Left_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (1, 100).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1).all.Pos.P_Valid and
           Neighbours (1).all.Pos.A = 2 and
           Neighbours (1).all.Pos.B = 100 and
           Neighbours (2).all.Pos.P_Valid and
           Neighbours (2).all.Pos.A = 2 and
           Neighbours (2).all.Pos.B = 99 and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 1 and
           Neighbours (3).all.Pos.B = 99 and
           Neighbours (4) = null and
           Neighbours (5) = null and
           Neighbours (6) = null,
         Message => "Sample test of neighbours for A, B = 1, 100");
   end Test_Numerate_Neighbours_Top_Left_Edge;

   procedure Test_Numerate_Neighbours_Top_Right_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours : Hexagon.Server_Map.Type_Neighbour_List;
   begin
      Neighbours := Hexagon.Server_Map.A_Map (100, 100).Neighbours;

      AUnit.Assertions.Assert
        (Condition =>
           Neighbours (1) = null and
           Neighbours (2) = null and
           Neighbours (3).all.Pos.P_Valid and
           Neighbours (3).all.Pos.A = 100 and
           Neighbours (3).all.Pos.B = 99 and
           Neighbours (4).all.Pos.P_Valid and
           Neighbours (4).all.Pos.A = 99 and
           Neighbours (4).all.Pos.B = 100 and
           Neighbours (5) = null and
           Neighbours (6) = null,
         Message => "Sample test of neighbours for A, B = 100, 100");
   end Test_Numerate_Neighbours_Top_Right_Edge;

   procedure Visit_Patches (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch_Adress) is
   begin

      if Control_Array (Integer (P_Patch.Pos.A), Integer (P_Patch.Pos.B)) = 0 then
         Control_Array (Integer (P_Patch.Pos.A), Integer (P_Patch.Pos.B)) := Visit_Counter;
      else
         Control_Array (Integer (P_Patch.Pos.A), Integer (P_Patch.Pos.B)) := -1;
      end if;
      Visit_Counter := Visit_Counter + 1;

   end Visit_Patches;

   procedure Test_Traverse_Map (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Neighbours                 : Hexagon.Server_Map.Type_Neighbour_List;
      All_Visited, Visited_Twice : Boolean;
      Sum, A_Sum                 : Integer;
      Scramble                   : Integer;
   begin

      for Trav_A in Control_Array'First (1) .. Control_Array'Last (1) loop
         for Trav_B in Control_Array'First (2) .. Control_Array'Last (2) loop
            Control_Array (Trav_A, Trav_B) := 0;
         end loop;
      end loop;
      Visit_Counter := 1;
      Hexagon.Server_Map.Reset_Visit;
      Hexagon.Server_Map.Traverse (Hexagon.Server_Map.A_Map (1, 1), Visit_Patches'Access);

      All_Visited   := True;
      Visited_Twice := False;
      Sum           := 0;
      A_Sum         := 0;
      Scramble      := 1;
      for Trav_A in Control_Array'First (1) .. Control_Array'Last (1) loop
         for Trav_B in Control_Array'First (2) .. Control_Array'Last (2) loop
            if Control_Array (Trav_A, Trav_B) = 0 then
               All_Visited := False;
            elsif Control_Array (Trav_A, Trav_B) = -1 then
               Visited_Twice := True;
            end if;
            Sum   := Sum + Control_Array (Trav_A, Trav_B);
            A_Sum := A_Sum + Control_Array (Trav_A, Trav_B) * Scramble;
            if Scramble = 1 then
               Scramble := 2;
            elsif Scramble = 2 then
               Scramble := 3;
            elsif Scramble = 3 then
               Scramble := 1;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert (Condition => All_Visited, Message => "All patches not visited");
      AUnit.Assertions.Assert
        (Condition => not Visited_Twice,
         Message   => "Some patches visited twice");
      AUnit.Assertions.Assert
        (Condition => Float (Sum / 2) = Float (25002500.0),
         Message   => "Patches not visited according to pattern Sum= " & Sum'Img);
      AUnit.Assertions.Assert
        (Condition => A_Sum = 100006732,
         Message   => "Patches not visited according to pattern A_Sum=" & A_Sum'Img);

   end Test_Traverse_Map;

   procedure Test_Get_Patch_Adress_From_AB_1_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Active_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (1, 1);

      AUnit.Assertions.Assert (Condition => Active_Patch /= null, Message => "Got a null-patch");
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch.all.Pos.P_Valid and
           Active_Patch.all.Pos.A = 1 and
           Active_Patch.all.Pos.B = 1,
         Message => "Patch has wrong A, B");
   end Test_Get_Patch_Adress_From_AB_1_1;

   procedure Test_Get_Patch_Adress_From_AB_100_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Active_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (100, 1);

      AUnit.Assertions.Assert (Condition => Active_Patch /= null, Message => "Got a null-patch");
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch.all.Pos.P_Valid and
           Active_Patch.all.Pos.A = 100 and
           Active_Patch.all.Pos.B = 1,
         Message => "Patch has wrong A, B");
   end Test_Get_Patch_Adress_From_AB_100_1;

   procedure Test_Get_Patch_Adress_From_AB_1_100 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Server_Map;
      Active_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (1, 100);

      AUnit.Assertions.Assert (Condition => Active_Patch /= null, Message => "Got a null-patch");
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch.all.Pos.P_Valid and
           Active_Patch.all.Pos.A = 1 and
           Active_Patch.all.Pos.B = 100,
         Message => "Patch has wrong A, B");
   end Test_Get_Patch_Adress_From_AB_1_100;

   procedure Test_Get_Patch_Adress_From_AB_100_100
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon;
      use Hexagon.Server_Map;
      Active_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (100, 100);

      AUnit.Assertions.Assert (Condition => Active_Patch /= null, Message => "Got a null-patch");
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch.all.Pos.P_Valid and
           Active_Patch.all.Pos.A = 100 and
           Active_Patch.all.Pos.B = 100,
         Message => "Patch has wrong A, B");
   end Test_Get_Patch_Adress_From_AB_100_100;

   procedure Test_Are_Neighbours (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Active_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Neighbour    : Hexagon.Server_Map.Type_Server_Patch_Adress;

   begin
      if Verbose then
         Text_IO.Put_Line ("tc_hexagon_server_map-Test_Are_Neighbours - enter");
      end if;

      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 8);
      Neighbour    := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 8);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   =>
           "Return as if patches are neighbours, but they are not - patch is not its own neighbour");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 9);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 5,9 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (6, 8);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 6,8 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (6, 7);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 6,7 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 7);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 5,7 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 8);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 4,8 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 9);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 4,9 are supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 10);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 5,10 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (6, 9);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 6,9 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (7, 8);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 7,8 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (7, 7);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 7,7 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (7, 6);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 7,6 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (6, 6);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 6,6 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 6);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 5,6 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 7);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 4,7 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (3, 8);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 3,8 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (3, 9);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 3,9 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (3, 10);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 3,10 are not supposed to be neighbours");

      Neighbour := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 10);

      AUnit.Assertions.Assert
        (Condition => not Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 5,8 and 4,10 are not supposed to be neighbours");

      Active_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (11, 16);
      Neighbour    := Hexagon.Server_Map.Get_Patch_Adress_From_AB (12, 15);

      AUnit.Assertions.Assert
        (Condition => Hexagon.Server_Map.Are_Neighbours (Active_Patch.all, Neighbour.all),
         Message   => "Patch 11,16 and 12,15 are supposed to be neighbours");

      if Verbose then
         Text_IO.Put_Line ("tc_hexagon_server_map-Test_Are_Neighbours - exit");
      end if;
   end Test_Are_Neighbours;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_1_1'Access,
         Name    => "Server Map coordinate 1,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_100_100'Access,
         Name    => "Server Map coordinate 100,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_1_100'Access,
         Name    => "Server Map coordinate 1,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_1_100'Access,
         Name    => "Server Map coordinate 100,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_4_4'Access,
         Name    => "Server Map coordinate 4,4");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Middle'Access,
         Name    => "Server Map Tests neighbours of A,B 4,4");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Left_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 1,8");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Bottom_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 9,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Right_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 100,97");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Top_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 95,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Bottom_Left_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 1,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Bottom_Right_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 100,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Top_Left_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 1,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Numerate_Neighbours_Top_Right_Edge'Access,
         Name    => "Server Map Tests neighbours of A,B 100,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Traverse_Map'Access,
         Name    => "Server Map Tests Traversal (too simple check)");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_1_1'Access,
         Name    => "Server Map Tests Get_Patch_Adress_From_AB_1_1 (1, 1)");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_100_1'Access,
         Name    => "Server Map Tests Get_Patch_Adress_From_AB_100_1 (100, 1)");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_1_100'Access,
         Name    => "Server Map Tests Get_Patch_Adress_From_AB_1_100 (1, 100)");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_100_100'Access,
         Name    => "Server Map Tests Get_Patch_Adress_From_AB_100_100 (100, 100)");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Are_Neighbours'Access,
         Name    => "Server Map Tests If patches are returned as neighbours or not");

   end Register_Tests;

end TC_Hexagon_Server_Map;
