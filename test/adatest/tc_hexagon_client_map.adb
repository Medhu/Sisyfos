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

with Hexagon;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Server.ServerAPI;
with Text_IO;
with AUnit.Assertions;
with Landscape;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Piece;
with Piece.Client_Piece;
with Hexagon.Area.Client_Area;
with Test_Piece;
with Status;
with Test_ServerRCI;
with Observation;
with Action;

package body TC_Hexagon_Client_Map is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line : Utilities.RemoteString.Type_Command_Parameters;

      Adm_Status : Status.Type_Adm_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Set_Up_Case - enter");
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

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Set_Origo_Patch (Map_Player_1, 1, 1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);
      Hexagon.Client_Map.Set_Origo_Patch (Map_Player_2, 1, 1);

      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Set_Up_Case - exit");
      end if;
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
      Server.ServerAPI.Stop;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test of Hexagon map client side ");
   end Name;

   procedure Test_Get_Map (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Client_Map;
      use Landscape;
      Equal : Boolean := True;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Test_Get_Map - enter");
      end if;

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (2, Map_Player_1);

      for Trav_A in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         for Trav_B in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            if Map_Player_1.Map (Trav_A, Trav_B).all.Pos /=
              Hexagon.Server_Map.A_Map (Trav_A, Trav_B).all.Pos or
              Map_Player_1.Map (Trav_A, Trav_B).all.Landscape_Here /=
                Hexagon.Server_Map.A_Map (Trav_A, Trav_B).all.Landscape_Here
            then
               Equal := False;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert (Condition => Equal, Message => "Server map not equal to client map");

      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Test_Get_Map - exit");
      end if;

   end Test_Get_Map;

   procedure Test_Get_Patch_Adress_From_AB_1_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);

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
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 100, 1);

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
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 100);

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
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 100, 100);

      AUnit.Assertions.Assert (Condition => Active_Patch /= null, Message => "Got a null-patch");
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch.all.Pos.P_Valid and
           Active_Patch.all.Pos.A = 100 and
           Active_Patch.all.Pos.B = 100,
         Message => "Patch has wrong A, B");
   end Test_Get_Patch_Adress_From_AB_100_100;

   procedure Test_Reset_Visible (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Reset : Boolean;
   begin
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            Map_Player_1.Map (ArrayX, ArrayY).Visible := True;
         end loop;
      end loop;

      Hexagon.Client_Map.Reset_Visible (Map_Player_1);
      Reset := True;
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            if Map_Player_1.Map (ArrayX, ArrayY).Visible = True then
               Reset := False;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert (Condition => Reset, Message => "Visible not reset");

   end Test_Reset_Visible;

   procedure Test_Reset_Draw_Action (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Reset : Boolean;
      use Hexagon.Client_Map;
   begin
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            Map_Player_1.Map (ArrayX, ArrayY).Draw_Action := Hexagon.Client_Map.Selected;
         end loop;
      end loop;

      Hexagon.Client_Map.Reset_Draw_Action (Map_Player_1);
      Reset := True;
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            if Map_Player_1.Map (ArrayX, ArrayY).Draw_Action /= Hexagon.Client_Map.Unselected then
               Reset := False;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert (Condition => Reset, Message => "Draw_Action not reset");

   end Test_Reset_Draw_Action;

   procedure Test_Select_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            Map_Player_1.Map (ArrayX, ArrayY).Draw_Action := Hexagon.Client_Map.Unselected;
         end loop;
      end loop;
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 6, 7);
      Hexagon.Client_Map.Select_Patch (Active_Patch.all);

      AUnit.Assertions.Assert
        (Condition => Map_Player_1.Map (6, 7).Draw_Action = Selected,
         Message   => "Did not succeed in selecting patch 6, 7");

   end Test_Select_Patch;

   procedure Test_Unselect_Patch (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            Map_Player_1.Map (ArrayX, ArrayY).Draw_Action := Hexagon.Client_Map.Unselected;
         end loop;
      end loop;
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 6, 7);
      Hexagon.Client_Map.Select_Patch (Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => Map_Player_1.Map (6, 7).Draw_Action = Selected,
         Message   => "Selected patch 6, 7");
      Hexagon.Client_Map.Unselect_Patch (Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => Map_Player_1.Map (6, 7).Draw_Action = Unselected,
         Message   => "Unselected patch 6, 7");

   end Test_Unselect_Patch;

   procedure Test_Unselect_All_Patches (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      Reset : Boolean;
   begin
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            Map_Player_1.Map (ArrayX, ArrayY).Draw_Action := Hexagon.Client_Map.Selected;
         end loop;
      end loop;

      Hexagon.Client_Map.Unselect_All_Patches (Map_Player_1);
      Reset := True;
      for ArrayX in Map_Player_1.Map'First (1) .. Map_Player_1.Map'Last (1) loop
         -- Horisontal
         for ArrayY in Map_Player_1.Map'First (2) .. Map_Player_1.Map'Last (2) loop
            -- Vertical
            if Map_Player_1.Map (ArrayX, ArrayY).Draw_Action /= Hexagon.Client_Map.Unselected then
               Reset := False;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert (Condition => Reset, Message => "Unselected all patches");

   end Test_Unselect_All_Patches;

   procedure Test_Set_Reports_On_Map_01 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      use Piece;
      use Player;
      use Ada.Containers;
      Map_Test1, Map_Test2, Map_Test3 : Boolean;
      Player_Changed_Observed_Patches : Observation.Observation_Of_Patches.Changes_To_Patches
        .Vector;
      Player_Changed_Observed_Pieces : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;
      Player_Changed_Observed_Patched_Effects : Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Vector;
      Player_Changed_Observed_Construction : Observation.Observation_Of_Construction.Changes_To_Construction.Vector;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Test_Set_Reports_On_Map_01 - enter");
      end if;

      Observation.Observation_Of_Patches.Changes_To_Patches.Clear (Player_Changed_Observed_Patches);
      Observation.Observation_Of_Pieces.Changes_To_Pieces.Clear (Player_Changed_Observed_Pieces);

      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 2, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 3, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 4, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 2, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 3, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 4, True));

      Observation.Observation_Of_Pieces.Changes_To_Pieces.Append
        (Player_Changed_Observed_Pieces,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'
           (Hexagon.Type_Hexagon_Position'(True, 1, 2), Piece.Type_Piece_Id'(1)));

      Hexagon.Client_Map.Set_Reports_On_Map
        (Map_Player_1,
         Player_Changed_Observed_Patches,
         Player_Changed_Observed_Pieces,
         Player_Changed_Observed_Patched_Effects,
        Player_Changed_Observed_Construction);

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_hexagon_client_map-Test_Set_Reports_On_Map_01_01.html"),
         Map_Player_1);

      Map_Test1 :=
        Landscape.Pieces_Here_List.Element (Map_Player_1.Map (1, 2).Pieces_Here, 1) = 1 and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 2).Pieces_Here) = 1 and
        Map_Player_1.Map (1, 2).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 3).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 3).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 5).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 3).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 3).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 5).Visible = False;

      AUnit.Assertions.Assert (Condition => Map_Test1, Message => "Patches not as expected 1");

      Observation.Observation_Of_Patches.Changes_To_Patches.Clear (Player_Changed_Observed_Patches);
      Observation.Observation_Of_Pieces.Changes_To_Pieces.Clear (Player_Changed_Observed_Pieces);

      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 2, False));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 2, False));

      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 5, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 5, True));

      Observation.Observation_Of_Pieces.Changes_To_Pieces.Append
        (Player_Changed_Observed_Pieces,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'
           (Hexagon.Type_Hexagon_Position'(True, 1, 3), Piece.Type_Piece_Id'(1)));

      Hexagon.Client_Map.Set_Reports_On_Map
        (Map_Player_1,
         Player_Changed_Observed_Patches,
         Player_Changed_Observed_Pieces,
         Player_Changed_Observed_Patched_Effects,
         Player_Changed_Observed_Construction);

      Map_Test2 :=
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 2).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 2).Visible = False and
        Landscape.Pieces_Here_List.Element (Map_Player_1.Map (1, 3).Pieces_Here, 1) = 1 and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 3).Pieces_Here) = 1 and
        Map_Player_1.Map (1, 3).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 5).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 2).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 2).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 3).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 3).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 5).Visible = True;

      AUnit.Assertions.Assert (Condition => Map_Test2, Message => "Patches not as expected 2");

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_hexagon_client_map-Test_Set_Reports_On_Map_01_02.html"),
         Map_Player_1);

      Observation.Observation_Of_Patches.Changes_To_Patches.Clear (Player_Changed_Observed_Patches);
      Observation.Observation_Of_Pieces.Changes_To_Pieces.Clear (Player_Changed_Observed_Pieces);
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 3, False));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 4, False));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(1, 5, False));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 3, False));

      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 4, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(2, 6, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(3, 4, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(3, 5, True));
      Observation.Observation_Of_Patches.Changes_To_Patches.Append
        (Player_Changed_Observed_Patches,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(3, 6, True));

      Observation.Observation_Of_Pieces.Changes_To_Pieces.Append
        (Player_Changed_Observed_Pieces,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'
           (Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(1)));

      Hexagon.Client_Map.Set_Reports_On_Map
        (Map_Player_1,
         Player_Changed_Observed_Patches,
         Player_Changed_Observed_Pieces,
         Player_Changed_Observed_Patched_Effects,
         Player_Changed_Observed_Construction);

      Map_Test3 :=
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 2).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 2).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 3).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 3).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 4).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (1, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (1, 5).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 2).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 2).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 3).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 3).Visible = False and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 4).Pieces_Here) = 1 and
        Map_Player_1.Map (2, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 5).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (2, 6).Pieces_Here) = 0 and
        Map_Player_1.Map (2, 6).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (3, 4).Pieces_Here) = 0 and
        Map_Player_1.Map (3, 4).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (3, 5).Pieces_Here) = 0 and
        Map_Player_1.Map (3, 5).Visible = True and
        Landscape.Pieces_Here_List.Length (Map_Player_1.Map (3, 6).Pieces_Here) = 0 and
        Map_Player_1.Map (3, 6).Visible = True;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_hexagon_client_map-Test_Set_Reports_On_Map_01_03.html"),
         Map_Player_1);

      AUnit.Assertions.Assert (Condition => Map_Test3, Message => "Patches not as expected 3");

      if Verbose then
         Text_IO.Put_Line ("TC_Hexagon_Client_Map.Test_Set_Reports_On_Map_01 - exit");
      end if;

   end Test_Set_Reports_On_Map_01;

   procedure Test_Get_X_From_AB_1_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 1);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 0 and Y = 0,
         Message   =>
           "X, Y not equal to 0, 0 for patch A, B = 1, 1 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_1_1;

   procedure Test_Get_X_From_AB_1_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 1, 2);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 0 and Y = 42,
         Message   =>
           "X, Y not equal to 0, 42 for patch A, B = 1, 2 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_1_2;

   procedure Test_Get_X_From_AB_2_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 1);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 36 and Y = 21,
         Message   =>
           "X, Y not equal to 36, 21 for patch A, B = 2, 1 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_2_1;

   procedure Test_Get_X_From_AB_2_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 2, 2);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 36 and Y = 63,
         Message   =>
           "X, Y not equal to 36, 63 for patch A, B = 2, 2 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_2_2;

   procedure Test_Get_X_From_AB_4_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 109 and Y = 189,
         Message   =>
           "X, Y not equal to 109, 189 for patch A, B = 4, 4 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_4_4;

   procedure Test_Get_X_From_AB_4_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 5);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 109 and Y = 231,
         Message   =>
           "X, Y not equal to 109, 231 for patch A, B = 4, 5 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_4_5;

   procedure Test_Get_X_From_AB_5_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon.Client_Map;
      X, Y         : Integer;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 4);
      X            := Hexagon.Client_Map.Get_X_From_AB (Map_Player_1, Active_Patch.all);
      Y            := Hexagon.Client_Map.Get_Y_From_AB (Map_Player_1, Active_Patch.all);
      AUnit.Assertions.Assert
        (Condition => X = 145 and Y = 210,
         Message   =>
           "X, Y not equal to 145, 210 for patch A, B = 5, 4 (got " & X'Img & " " & Y'Img & ")");

   end Test_Get_X_From_AB_5_4;

   procedure Test_Get_Patch_From_XY_0_0 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 0, 0);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 1 and Active_Patch.Pos.B = 1,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 0, 0");

   end Test_Get_Patch_From_XY_0_0;

   procedure Test_Get_Patch_From_XY_6_8 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 6, 8);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 1 and Active_Patch.Pos.B = 1,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 6, 8");

   end Test_Get_Patch_From_XY_6_8;

   procedure Test_Get_Patch_From_XY_30_70 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 10, 52);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 1 and Active_Patch.Pos.B = 2,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 10, 52");

   end Test_Get_Patch_From_XY_30_70;

   procedure Test_Get_Patch_From_XY_40_82 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 40, 82);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 2 and Active_Patch.Pos.B = 2,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 40, 82");

   end Test_Get_Patch_From_XY_40_82;

   procedure Test_Get_Patch_From_XY_135_200 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 135, 200);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 5 and Active_Patch.Pos.B = 4,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 135, 200");

   end Test_Get_Patch_From_XY_135_200;

   procedure Test_Get_Patch_From_XY_108_148 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Active_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 108, 148);

      AUnit.Assertions.Assert
        (Condition => Active_Patch.Pos.A = 4 and Active_Patch.Pos.B = 3,
         Message   => "Patch is not A, B = 1, 1 for X,Y = 108, 248");

   end Test_Get_Patch_From_XY_108_148;

   procedure Test_Get_Patch_From_XY_108_148_and_AB_4_3
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Hexagon.Client_Map;
      Active_Patch_XY, Active_Patch_AB : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      Active_Patch_XY := Hexagon.Client_Map.Get_Patch_Adress_From_XY (Map_Player_1, 108, 148);
      Active_Patch_AB := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 3);
      AUnit.Assertions.Assert
        (Condition =>
           Active_Patch_XY = Active_Patch_AB and Active_Patch_XY.all = Active_Patch_AB.all,
         Message => "Patch we got from XY 108,148 is not same/equal to patch from AB=4,3");

   end Test_Get_Patch_From_XY_108_148_and_AB_4_3;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Map'Access,
         Name    => "Client Map Tests Get_Map");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_1_1'Access,
         Name    => "Client Map Tests Get_Patch_Adress From AB 1,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_1_100'Access,
         Name    => "Client Map Tests Get_Patch_Adress From AB 1,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_100_1'Access,
         Name    => "Client Map Tests Get_Patch_Adress From AB 100,1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_Adress_From_AB_100_100'Access,
         Name    => "Client Map Tests Get_Patch_Adress From AB 100,100");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Reset_Visible'Access,
         Name    => "Client Map Tests Reset_Visible");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Reset_Draw_Action'Access,
         Name    => "Client Map Tests Reset_Draw_Action");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Select_Patch'Access,
         Name    => "Client Map Tests Select_Patch");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Unselect_Patch'Access,
         Name    => "Client Map Tests Unselect_Patch");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Unselect_All_Patches'Access,
         Name    => "Client Map Tests Unselect_All_Patches");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Set_Reports_On_Map_01'Access,
         Name    => "Client Map Tests Test_Set_Reports_On_Map_01");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_1_1'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_1_2'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_2_1'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_2_2'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_4_4'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_4_5'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_X_From_AB_5_4'Access,
         Name    => "Client Map Tests Test_Get_X_From_AB");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_0_0'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_6_8'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_30_70'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_40_82'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_135_200'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_108_148'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Get_Patch_From_XY_108_148_and_AB_4_3'Access,
         Name    => "Client Map Tests Test_Get_Patch_From_XY");

   end Register_Tests;

end TC_Hexagon_Client_Map;
