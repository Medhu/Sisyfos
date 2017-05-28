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
with Effect;
with Ada.Containers.Ordered_Sets;
with Action;

package body Tc_Patch_Effects is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Command_Line : Utilities.RemoteString.Type_Command_Parameters;
   Test_List : aliased Test_Piece.Type_Test_List;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Set_Up - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Set_Up - exit");
      end if;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);

   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Tear_Down - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Tear_Down - exit");
      end if;
   end Tear_Down;

   -----------------
   -- Set_Up Case --
   -----------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
      Adm_Status : Status.Type_Adm_Status;
      Command_Line : Utilities.RemoteString.Type_Command_Parameters;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Set_Up_Case - enter");
      end if;

      Test_ServerRCI.Init(Command_Line);
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
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effects-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Set_Up_Case - exit");
      end if;

   end Set_Up_Case;

   --------------------
   -- Tear_Down Case --
   --------------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Tear_Down_Case - enter");
      end if;

      Test_ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Tear_Down_Case - exit");
      end if;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test game infrastructure");
   end Name;

   procedure Test_Pre_Placed_Patch_Effect (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      All_Frames         : Observation.Frames.Piece_Visibility_Frames.Vector;
      Frame_Cursor       : Observation.Frames.Piece_Visibility_Frames.Cursor;
      Test_Effect        : Effect.Effect_List.Cursor;
      A_Patch1, A_Patch2 : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Effect;
      use Hexagon;
      use Player;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Pre_Placed_Patch_Effect - enter");
      end if;

      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Pre_Placed_Patch_Effect_01.html"),
         Map_Player_1);
      Piece.Client_Piece.Get_Pieces_Report (Player_Id_1, All_Frames);
      Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.First (All_Frames);
      while Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) loop
         Text_IO.Put_Line ("Frame loop");
         Hexagon.Client_Map.Set_Reports_On_Map
           (Map_Player_1,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Patches,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Pieces,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Patches_Effects_Info,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Constructions_Info);

         Text_IO.Put_Line
           ("Length Patches Effects=" &
            Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Length
              (Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Patches_Effects_Info)'
              Img);

         Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
      end loop;

      A_Patch1 := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);
      A_Patch2 := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 6);
      AUnit.Assertions.Assert
        (Condition =>
           Effect.Effect_List.Length (A_Patch1.all.Effects_Here) = 1 and
           Effect.Effect_List.Length (A_Patch2.all.Effects_Here) = 1,
         Message => "Not expected number of effects in patch-effect list");

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Pre_Placed_Patch_Effect_02.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Pre_Placed_Patch_Effect_03.html"),
         Map_Player_1);

      Test_Effect := Effect.Effect_List.First (A_Patch2.all.Effects_Here);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Treasure,
         "Didn't find expected effect on patch");

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Treasure,
         "Didn't find patch effect on the expected position");

      Test_Effect := Effect.Effect_List.First (A_Patch2.all.Effects_Here);
      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Treasure,
         "Didn't find expected effect on patch");

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Treasure,
         "Didn't find patch effect on the expected position");
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Pre_Placed_Patch_Effect - exit");
      end if;
   end Test_Pre_Placed_Patch_Effect;

   procedure Test_Place_During_Game_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      All_Frames   : Observation.Frames.Piece_Visibility_Frames.Vector;
      Frame_Cursor : Observation.Frames.Piece_Visibility_Frames.Cursor;
      Test_Effect  : Effect.Effect_List.Cursor;

      A_Patch1, A_Patch2 : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Server_Patch     : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Ada.Containers;
      use Effect;
      use Hexagon;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Place_During_Game_1 - enter");
      end if;

      A_Patch1 := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);
      A_Patch2 := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 6);
      AUnit.Assertions.Assert
        (Condition =>
           Effect.Effect_List.Length (A_Patch1.all.Effects_Here) = 1 and
           Effect.Effect_List.Length (A_Patch2.all.Effects_Here) = 1,
         Message => "Not expected number of effects in patch-effect list");

      -- Suppose a server sets an effect in customized code.
      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 5);
      Effect.Effect_List.Include
        (A_Server_Patch.all.Effects_Here,
         Test_Piece.Effect_Hunger,
         Effect.Type_Effect'(Test_Piece.Effect_Hunger, 1));
      Server.ServerAPI.Observe_Game (1); -- We must do this from here. This is a server side responsibility.

      Piece.Client_Piece.Get_Pieces_Report (Player_Id_1, All_Frames);

      Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.First (All_Frames);
      while Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) loop
         Text_IO.Put_Line ("Frame loop");
         Hexagon.Client_Map.Set_Reports_On_Map
           (Map_Player_1,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Patches,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Pieces,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Patches_Effects_Info,
            Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Constructions_Info);

         Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
      end loop;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Place_During_Game_1_01.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Place_During_Game_1_02.html"),
         Map_Player_1);

      A_Patch1 := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 6);

      Test_Effect := Effect.Effect_List.First (A_Patch1.all.Effects_Here);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Treasure,
         "Didn't find expected effect on (4, 6)");

      A_Patch1    := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 5);
      Test_Effect := Effect.Effect_List.First (A_Patch1.all.Effects_Here);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Hunger,
         "Didn't find expected effect on (5, 5)");

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Place_During_Game_1 - exit");
      end if;
   end Test_Place_During_Game_1;

   procedure Test_Grant_During_Game_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Test_Effect : Effect.Effect_List.Cursor;

      A_Server_Patch1, A_Server_Patch2 : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Pos   : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 4, 4);
      An_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 2) :=
        (1 => Hexagon.Type_Hexagon_Position'(True, 4, 4),
         2 => Hexagon.Type_Hexagon_Position'(True, 4, 6));
      A_Piece : Piece.Type_Piece;

      use Ada.Containers;
      use Effect;
      use Hexagon;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Grant_During_Game_1 - enter");
      end if;
      A_Server_Patch2 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here) = 1,
         "Expected 1 effects on (4, 6) but got " &
         Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here)'Img);

      A_Piece.Id := 1;
      A_Piece.Type_Of_Piece := 9;
      A_Piece.Category := Piece.Fighting_Piece;
      A_Piece.Player_Id := 1;

      Piece.Client_Piece.Grant_Patch_Effect
        (Player.Type_Player_Id(1),
         Action.Type_Action_Type(1200),
         A_Piece,
         Effect.Type_Effect'(Test_Piece.Effect_Weather, 0),
         An_Area);

      Test_Piece.Wait_For_Server(1200);

      AUnit.Assertions.Assert
        (Test_Piece.Test_List.all(1200).Result = Status.Ok,
         "Expected status Ok but got" & Test_Piece.Test_List.all(1200).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Grant_During_Game_2_01.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Grant_During_Game_2_02.html"),
         Map_Player_1);

      A_Server_Patch1 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 4);

      Test_Effect := Effect.Effect_List.First (A_Server_Patch1.all.Effects_Here);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Weather,
         "Didn't find expected effect on (4, 4)");

      A_Server_Patch2 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);

      Test_Effect := Effect.Effect_List.First (A_Server_Patch2.all.Effects_Here);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Element (Test_Effect).Effect_Name = Test_Piece.Effect_Weather,
         "Didn't find expected effect on (4, 6)");

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Grant_During_Game_1 - exit");
      end if;
   end Test_Grant_During_Game_1;

   procedure Test_Revoke_During_Game_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Test_Effect : Effect.Effect_List.Cursor;

      A_Server_Patch1, A_Server_Patch2 : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Pos   : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 4, 4);
      An_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 2) :=
        (1 => Hexagon.Type_Hexagon_Position'(True, 4, 4),
         2 => Hexagon.Type_Hexagon_Position'(True, 4, 6));
      A_Piece : Piece.Type_Piece;

      use Ada.Containers;
      use Effect;
      use Hexagon;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Revoke_During_Game_1 - enter");
      end if;

      A_Server_Patch2 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here) = 2,
         "Expected 2 effects on (4, 6) but got " &
         Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here)'Img);

      A_Piece.Id := 1;
      A_Piece.Type_Of_Piece := 9;
      A_Piece.Category := Piece.Fighting_Piece;
      A_Piece.Player_Id := 1;

      Piece.Client_Piece.Revoke_Patch_Effect
        (Player.Type_Player_Id(1),
         Action.Type_Action_Type(1201),
         A_Piece,
         Effect.Type_Effect'(Test_Piece.Effect_Weather, 0),
         An_Area);

      Test_Piece.Wait_For_Server(1201);

      AUnit.Assertions.Assert
        (Test_Piece.Test_List.all(1201).Result = Status.Ok,
         "Expected status Ok but got" & Test_Piece.Test_List.all(1201).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Revoke_During_Game_1_01.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_patch_effect-Test_Revoke_During_Game_1_02.html"),
         Map_Player_1);

      A_Server_Patch1 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 4);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Length (A_Server_Patch1.all.Effects_Here) = 0,
         "Expected no patch effects on (4, 4) but found " &
         Effect.Effect_List.Length (A_Server_Patch1.all.Effects_Here)'Img);

      A_Server_Patch2 := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);

      AUnit.Assertions.Assert
        (Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here) = 1,
         "Expected 1 patch effects on (4, 6) but found " &
         Effect.Effect_List.Length (A_Server_Patch2.all.Effects_Here)'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Revoke_During_Game_1 - exit");
      end if;
   end Test_Revoke_During_Game_1;

   procedure Test_Grant_On_Patched_Not_Belonging_To_Player
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test_Effect : Effect.Effect_List.Cursor;

      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Pos   : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 4, 4);
      An_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 2) :=
        (1 => Hexagon.Type_Hexagon_Position'(True, 5, 5),
         2 => Hexagon.Type_Hexagon_Position'(True, 4, 6));
      A_Piece : Piece.Type_Piece;

      use Ada.Containers;
      use Effect;
      use Hexagon;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_patch_effects.Test_Grant_On_Patched_Not_Belonging_To_Player - enter");
      end if;

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 4);

      A_Piece.Id := 1;
      A_Piece.Type_Of_Piece := 9;
      A_Piece.Category := Piece.Fighting_Piece;
      A_Piece.Player_Id := 1;

      Piece.Client_Piece.Grant_Patch_Effect
        (Player.Type_Player_Id(1),
         Action.Type_Action_Type(1210),
         A_Piece,
         Effect.Type_Effect'(Test_Piece.Effect_Weather, 0),
         An_Area);

      Test_Piece.Wait_For_Server(1210);

      AUnit.Assertions.Assert
        (Test_Piece.Test_List.all(1210).Result = Status.Target_Patch_Occupied,
         "Expected status Target_Patch_Occupied but got " & Test_Piece.Test_List.all(1210).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_patch_effect-Test_Grant_On_Patched_Not_Belonging_To_Player_01.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_patch_effect-Test_Grant_On_Patched_Not_Belonging_To_Player_02.html"),
         Map_Player_1);

      if Verbose then
         Text_IO.Put_Line ("tc_patch_effects.Test_Grant_On_Patched_Not_Belonging_To_Player - exit");
      end if;
   end Test_Grant_On_Patched_Not_Belonging_To_Player;

   procedure Test_Revoke_On_Patched_Not_Belonging_To_Player
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test_Effect : Effect.Effect_List.Cursor;

      A_Server_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      A_Pos   : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 4, 4);
      An_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 2) :=
        (1 => Hexagon.Type_Hexagon_Position'(True, 5, 5),
         2 => Hexagon.Type_Hexagon_Position'(True, 4, 6));

      use Ada.Containers;
      use Effect;
      use Hexagon;
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_patch_effects.Test_Revoke_On_Patched_Not_Belonging_To_Player - enter");
      end if;

      A_Server_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 4);

      Test_ServerRCI.Grant_Patch_Effect
        (Player.Type_Player_Id(1),
         Action.Type_Action_Type(1212),
         1,
         Effect.Type_Effect'(Test_Piece.Effect_Weather, 0),
         An_Area);

      Test_Piece.Wait_For_Server(1212);

      AUnit.Assertions.Assert
        (Test_Piece.Test_List.all(1212).Result = Status.Target_Patch_Occupied,
         "Expected status Target_Patch_Occupied but got " & Test_Piece.Test_List.all(1212).Result'Img);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_patch_effect-Test_Revoke_On_Patched_Not_Belonging_To_Player_01.html"));
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_patch_effect-Test_Revoke_On_Patched_Not_Belonging_To_Player_02.html"),
         Map_Player_1);

      if Verbose then
         Text_IO.Put_Line
           ("tc_patch_effects.Test_Revoke_On_Patched_Not_Belonging_To_Player - exit");
      end if;
   end Test_Revoke_On_Patched_Not_Belonging_To_Player;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Pre_Placed_Patch_Effect'Access,
         Name    => "Test effect placed during Game Startup");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Place_During_Game_1'Access,
         Name    => "Test effect placed during game");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Grant_During_Game_1'Access,
         Name    => "Test effect granted during game");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Revoke_During_Game_1'Access,
         Name    => "Test effect revoke during game");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Grant_On_Patched_Not_Belonging_To_Player'Access,
         Name    => "Test effect grant effect on a patch not belonging to the player");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Revoke_On_Patched_Not_Belonging_To_Player'Access,
         Name    => "Test effect revoke effect on a patch not belonging to the player");
   end Register_Tests;

end Tc_Patch_Effects;
