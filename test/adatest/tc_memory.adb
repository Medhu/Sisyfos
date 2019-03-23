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
with AUnit.Assertions;

with Text_IO;
with Player;
with Hexagon.Client_Map;
with Test_Piece;
with Piece;
with Server.Server;
with Server.ServerRCI;
with Utilities;
with Hexagon.Server_Map;
with Ada.Strings.Unbounded;
with Status;
with Hexagon.Area;
with Piece.Client_Piece;
with Landscape;

package body Tc_Memory is

   Verbose : constant Boolean := False;

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

   procedure Test_Game_Start (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Game_Start - enter");
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
         Test_Piece.Pieces_Type_Info_List,
         Test_Piece.Houses_Type_Info_List,
         Ada.Strings.Unbounded.To_Unbounded_String ("resources\test0000.dat"),
         Test_Piece.Test_Start_Game'Access,
         Test_Piece.Test_Upkeep_Game'Access,
         Test_Piece.Test_Start_Turn'Access,
         Test_Piece.Test_End_Turn'Access,
         Test_Piece.Test_End_Game'Access);
      Server.Server.Start;

      Player_Id_1 :=
         Server.ServerRCI.Get_A_Player_Id (Utilities.RemoteString.To_Unbounded_String ("User A"));
      Player_Id_2 :=
         Server.ServerRCI.Get_A_Player_Id (Utilities.RemoteString.To_Unbounded_String ("User B"));
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
        (Ada.Strings.Unbounded.To_Unbounded_String ("tc_game-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Game_Start - exit");
      end if;
   end Test_Game_Start;

   procedure Test_Engine_Only (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      --        use Piece;
      --        A_Piece           : Piece.Type_Piece;
      --        A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      --        Ret_Status        : Status.Type_Status;
      --        Current_Player_Id : Player.Type_Player_Id;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memroy.Test_Movement_Capability_Sentry - entry");
      end if;

      --        Hexagon.Server_Map.Save_Scenario
      --          (Ada.Strings.Unbounded.To_Unbounded_String
      --("tc_memory-Test_Movement_Capability_Sentry_01.html"));
      --        Current_Player_Id := 1;
      --        A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 42,
      --42);
      --
      --        Piece.Client_Piece.Create_Piece
      --          (A_Piece,
      --           Test_Piece.Sentry_Piece,
      --           Piece.Fighting_Piece,
      --           Landscape.Type_Patch (A_Patch.all),
      --           Current_Player_Id,
      --           Ret_Status);
      --
      --        Hexagon.Server_Map.Save_Scenario
      --          (Ada.Strings.Unbounded.To_Unbounded_String
      --("tc_memory-Test_Movement_Capability_Sentry_02.html"));
      --        A_Piece.Id := 4;

      while True loop
         --        declare
         --           Ret : Hexagon.Area.Type_Action_Capabilities :=
         --Server.ServerRCI.Movement_Capability (A_Piece.Id);
         --        begin
         --
         --           AUnit.Assertions.Assert
         --             (Condition => Ret'Length = 6,
         --              Message   => "Didnt return '6' movement capability for a Sentry, but " &
         --Ret'Length'Img);
         --
         --           end;
         null;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Movement_Capability_Sentry - exit");
      end if;
   end Test_Engine_Only;

   procedure Test_Movement_Capability_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memroy.Test_Movement_Capability_Sentry - entry");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            ("tc_memory-Test_Movement_Capability_Sentry_01.html"));
      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 42, 42);

      Piece.Client_Piece.Create_Piece
        (A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            ("tc_memory-Test_Movement_Capability_Sentry_02.html"));
      A_Piece.Id := 4;

      while True loop
         declare
            Ret : Hexagon.Area.Type_Action_Capabilities :=
              Server.ServerRCI.Movement_Capability (A_Piece.Id);
         begin

            AUnit.Assertions.Assert
              (Condition => Ret'Length = 6,
               Message   => "Didnt return '6' movement capability for a Sentry, but " &
                            Ret'Length'Img);

         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Movement_Capability_Sentry - exit");
      end if;
   end Test_Movement_Capability_Sentry;

   procedure Test_Attack_Capability_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memroy.Test_Attack_Capability_Sentry - entry");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            ("tc_memory-Test_Attack_Capability_Sentry_01.html"));
      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 42, 42);

      Piece.Client_Piece.Create_Piece
        (A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            ("tc_memory-Test_Attack_Capability_Sentry_02.html"));
      A_Piece.Id := 4;

      while True loop
         declare
            Ret : Hexagon.Area.Type_Action_Capabilities :=
              Server.ServerRCI.Attack_Capability (A_Piece.Id);
         begin

            AUnit.Assertions.Assert
              (Condition => Ret'Length = 6,
               Message   => "Didnt return '6' Attack capability for a Sentry, but " &
                            Ret'Length'Img);

         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Attack_Capability_Sentry - exit");
      end if;
   end Test_Attack_Capability_Sentry;

   procedure Test_Observation_Area_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece           : Piece.Type_Piece;
      A_Patch           : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status        : Status.Type_Status;
      Current_Player_Id : Player.Type_Player_Id;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Observation_Area_Sentry - enter");
      end if;

      Current_Player_Id := 1;
      A_Patch           := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 44, 44);
      Piece.Client_Piece.Create_Piece
        (A_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         Current_Player_Id,
         Ret_Status);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            ("tc_memory-Test_Observation_Area_Sentry_01.html"));

      while True loop
         declare
            Ret : Hexagon.Area.Type_Action_Capabilities := Server.ServerRCI.Observation_Area (8);
         begin

            AUnit.Assertions.Assert
              (Condition => Ret'Length = 13,
               Message   => "Didnt return '13' attack capability for a Sentry");
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Observation_Area_Sentry - exit");
      end if;
   end Test_Observation_Area_Sentry;

   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_memroy.Test_Game_Stop - enter");
      end if;

      Server.ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_memory.Test_Game_Stop - exit");
      end if;
   end Test_Game_Stop;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test if we have memory leaks");
   end Name;

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
         Routine => Test_Attack_Capability_Sentry'Access,
         Name    => "Testing the attack capability returned by a Sentry");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Engine_Only'Access,
         Name    => "Testing the Engine Only");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Movement_Capability_Sentry'Access,
         Name    => "Testing the movement capability returned by a Sentry");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Stop'Access,
         Name    => "Game Test stop game engine");
   end Register_Tests;

end Tc_Memory;
