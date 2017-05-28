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
with Piece;
with Piece.Server;
with Ada.Containers;
with Hexagon;
with Utilities;
with Test_Piece;
with Effect;
with Server.ServerAPI;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Player;

package body Tc_Pieces_Save_Load is

   Verbose     : constant Boolean := False;
   Test_Class1 : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2 : Test_Piece.Type_My_Test_House_Access_Class;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Testing functionality for saving and loading pieces");
   end Name;

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Pieces_Save_Load.Set_Up_Case - enter");
      end if;

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
   end Set_Up_Case;

   procedure Test_Pieces_Save_And_Load_Empty_List
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Pieces_List, Another_Pieces_List : Piece.Server.Pieces_Server_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Effects_Save_And_Load_Empty_List - enter");
      end if;

      Piece.Server.Save_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_Empty_List.dat"), A_Pieces_List);

      Piece.Server.Load_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_Empty_List.dat"), Another_Pieces_List);

      AUnit.Assertions.Assert
        (Condition => Piece.Server.Pieces_Server_List.Length (Another_Pieces_List) = 0,
         Message   => "Did not save effects as expected");

      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Effects_Save_And_Load_Empty_List - exit");
      end if;

   end Test_Pieces_Save_And_Load_Empty_List;

   procedure Test_Pieces_Save_And_Load_1_Element (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Pieces_List, Another_Pieces_List : Piece.Server.Pieces_Server_List.Vector;

      A_Piece_Position, Another_Piece_Position : Piece.Server.Type_Piece_Position;

      Some_Effects, Other_Effects : Effect.Effect_List.Map;
      Other_Effect                : Effect.Type_Effect;

      use Utilities.RemoteString;
      use Ada.Containers;
      use Piece;
      use Hexagon;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Pieces_Save_And_Load_1_Element - enter");
      end if;

      Another_Piece_Position.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (1));
      Another_Piece_Position.Actual_Piece := null;

      Effect.Effect_List.Include
        (Some_Effects,
         Test_Piece.Effect_Raining_Arrows,
         Effect.Type_Effect'(Test_Piece.Effect_Raining_Arrows, 21));
      A_Piece_Position.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (5), Hexagon.Type_Hexagon_Numbers (5));
      A_Piece_Position.Actual_Piece :=
        new Test_Piece.Type_My_Test_Piece'
          (1911,
           1,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String ("The name"),
           1,
--           6,
           Some_Effects,
           56);

      Piece.Server.Pieces_Server_List.Append (A_Pieces_List, A_Piece_Position);

      Piece.Server.Save_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_1_Element.dat"), A_Pieces_List);

      Piece.Server.Load_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_1_Element.dat"), Another_Pieces_List);

      AUnit.Assertions.Assert
        (Condition => Piece.Server.Pieces_Server_List.Length (Another_Pieces_List) = 1,
         Message   => "Did not save effects as expected");

      Another_Piece_Position :=
        Piece.Server.Pieces_Server_List.Element
          (Piece.Server.Pieces_Server_List.First (Another_Pieces_List));

      AUnit.Assertions.Assert
        (Condition =>
           Another_Piece_Position.Actual_Pos = Hexagon.Type_Hexagon_Position'(True, 5, 5),
         Message => "Loaded position not as expected");

      -- mess up data in original list to make sure
      -- we are not chacking on the same memory
      A_Piece_Position.Actual_Piece.all.Id := 999;
      A_Piece_Position.Actual_Pos.A        := 99;
      A_Piece_Position.Actual_Pos.B        := 99;
      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position.Actual_Piece.all.Id = 1911,
         Message   => "Loaded piece does not have id as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position.Actual_Piece.all.Name = "The name",
         Message   => "Loaded piece does not have name as expected");

      AUnit.Assertions.Assert
        (Condition =>
           Test_Piece.Type_My_Test_Piece'Class (Another_Piece_Position.Actual_Piece.all).Test = 56,
         Message => "Loaded piece does not have id as expected");

      Other_Effects := Another_Piece_Position.Actual_Piece.all.Effects_On_Piece;

      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (Other_Effects) = 1,
         Message   => "Loaded piece did not give effects");

      Other_Effect :=
        Effect.Effect_List.Element (Effect.Effect_List.First (Other_Effects));

      AUnit.Assertions.Assert
        (Condition => Other_Effect.Effect_Name = Test_Piece.Effect_Raining_Arrows,
         Message   => "Loaded piece did not give effects");

      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Pieces_Save_And_Load_1_Element - exit");
      end if;

   end Test_Pieces_Save_And_Load_1_Element;

   procedure Test_Pieces_Save_And_Load_3_Element (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Pieces_List, Another_Pieces_List : Piece.Server.Pieces_Server_List.Vector;
--        The_Stream                         : Ada.Streams.Stream_IO.Stream_Access;
--        A_File                             : Ada.Streams.Stream_IO.File_Type;

      A_Piece_Position1,
      A_Piece_Position2,
      A_Piece_Position3,
      Another_Piece_Position1,
      Another_Piece_Position2,
      Another_Piece_Position3 : Piece.Server.Type_Piece_Position;

      Some_Effects, Other_Effects : Effect.Effect_List.Map;
      Other_Effect                : Effect.Type_Effect;

      use Utilities.RemoteString;
      use Ada.Containers;
      use Piece;
      use Hexagon;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Pieces_Save_And_Load_1_Element - enter");
      end if;

      Another_Piece_Position1.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (1));
      Another_Piece_Position1.Actual_Piece := null;

      Another_Piece_Position2.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (1));
      Another_Piece_Position2.Actual_Piece := null;

      Another_Piece_Position3.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (1), Hexagon.Type_Hexagon_Numbers (1));
      Another_Piece_Position3.Actual_Piece := null;

      -- 1
      Effect.Effect_List.Include
        (Some_Effects,
         Test_Piece.Effect_Versatile,
         Effect.Type_Effect'(Test_Piece.Effect_Versatile, 54));
      A_Piece_Position1.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (7), Hexagon.Type_Hexagon_Numbers (9));
      A_Piece_Position1.Actual_Piece :=
        new Test_Piece.Type_My_Test_Piece'
          (1912,
           1,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String ("The name 1"),
           1,
           --6,
           Some_Effects,
           52);

      Piece.Server.Pieces_Server_List.Append (A_Pieces_List, A_Piece_Position1);

      -- 2
      Effect.Effect_List.Clear(Some_Effects);
      Effect.Effect_List.Include
        (Some_Effects,
         Test_Piece.Effect_Hunger,
         Effect.Type_Effect'(Test_Piece.Effect_Hunger, 87));
      A_Piece_Position2.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (9), Hexagon.Type_Hexagon_Numbers (11));
      A_Piece_Position2.Actual_Piece :=
        new Test_Piece.Type_My_Test_Piece'
          (1913,
           1,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String ("The name 2"),
           1,
--           2,
           Some_Effects,
           57);

      Piece.Server.Pieces_Server_List.Append (A_Pieces_List, A_Piece_Position2);

      -- 3
      Effect.Effect_List.Clear (Some_Effects);
      Effect.Effect_List.Include
        (Some_Effects,
         Test_Piece.Effect_Versatile,
         Effect.Type_Effect'(Test_Piece.Effect_Versatile, 524));
      Effect.Effect_List.Include
        (Some_Effects,
         Test_Piece.Effect_First_Attack,
         Effect.Type_Effect'(Test_Piece.Effect_First_Attack, 524));
      A_Piece_Position3.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'
          (True, Hexagon.Type_Hexagon_Numbers (2), Hexagon.Type_Hexagon_Numbers (4));
      A_Piece_Position3.Actual_Piece :=
        new Test_Piece.Type_My_Test_Piece'
          (1914,
           2,
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String ("The name 3"),
           1,
--           6,
           Some_Effects,
           54);

      Piece.Server.Pieces_Server_List.Append (A_Pieces_List, A_Piece_Position3);

      Piece.Server.Save_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_3_Element.dat"), A_Pieces_List);

      -- mess up data in original list to make sure
      -- we are not chacking on the same memory
      A_Piece_Position1.Actual_Piece.all.Id := 9991;
      A_Piece_Position1.Actual_Pos.A        := 99;
      A_Piece_Position1.Actual_Pos.B        := 99;

      A_Piece_Position2.Actual_Piece.all.Id := 9992;
      A_Piece_Position2.Actual_Pos.A        := 99;
      A_Piece_Position2.Actual_Pos.B        := 99;

      A_Piece_Position3.Actual_Piece.all.Id := 9991;
      A_Piece_Position3.Actual_Pos.A        := 99;
      A_Piece_Position3.Actual_Pos.B        := 99;

      Piece.Server.Load_Pieces (Ada.Strings.Unbounded.To_Unbounded_String("Test_Pieces_Save_And_Load_3_Element.dat"), Another_Pieces_List);

      AUnit.Assertions.Assert
        (Condition => Piece.Server.Pieces_Server_List.Length (Another_Pieces_List) = 3,
         Message   => "Did not have the number of pieces as expected");

      -- Check first piece in list
      Another_Piece_Position1 :=
        Piece.Server.Pieces_Server_List.Element
          (Piece.Server.Pieces_Server_List.First (Another_Pieces_List));

      AUnit.Assertions.Assert
        (Condition =>
           Another_Piece_Position1.Actual_Pos = Hexagon.Type_Hexagon_Position'(True, 7, 9),
         Message => "Loaded position not as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position1.Actual_Piece.all.Id = 1912,
         Message   => "Loaded piece does not have id as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position1.Actual_Piece.all.Name = "The name 1",
         Message   => "Loaded piece does not have name as expected");

      AUnit.Assertions.Assert
        (Condition =>
           Test_Piece.Type_My_Test_Piece'Class (Another_Piece_Position1.Actual_Piece.all).Test = 52,
         Message => "Loaded piece does not have 'Test' as expected");

      Other_Effects := Another_Piece_Position1.Actual_Piece.all.Effects_On_Piece;

      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (Other_Effects) = 1,
         Message   => "Loaded piece did not give effects");

      Other_Effect :=
        Effect.Effect_List.Element (Effect.Effect_List.First (Other_Effects));

      AUnit.Assertions.Assert
        (Condition => Other_Effect.Effect_Name = Test_Piece.Effect_Versatile,
         Message   => "Loaded piece did not give effects");

      -- Check second piece in list
      Another_Piece_Position2 :=
        Piece.Server.Pieces_Server_List.Element
          (Piece.Server.Pieces_Server_List.Next
             (Piece.Server.Pieces_Server_List.First (Another_Pieces_List)));

      AUnit.Assertions.Assert
        (Condition =>
           Another_Piece_Position2.Actual_Pos = Hexagon.Type_Hexagon_Position'(True, 9, 11),
         Message => "Loaded position not as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position2.Actual_Piece.all.Id = 1913,
         Message   => "Loaded piece does not have id as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position2.Actual_Piece.all.Name = "The name 2",
         Message   => "Loaded piece does not have name as expected");

      AUnit.Assertions.Assert
        (Condition =>
           Test_Piece.Type_My_Test_Piece'Class (Another_Piece_Position2.Actual_Piece.all).Test = 57,
         Message => "Loaded piece does not have 'Test' as expected");

      Effect.Effect_List.Clear (Other_Effects);
      Other_Effects := Another_Piece_Position2.Actual_Piece.all.Effects_On_Piece;

      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (Other_Effects) = 1,
         Message   => "Loaded piece did not give effects, it gave " & Effect.Effect_List.Length (Other_Effects)'Img );

      Other_Effect :=
        Effect.Effect_List.Element (Effect.Effect_List.First (Other_Effects));

      AUnit.Assertions.Assert
        (Condition => Other_Effect.Effect_Name = Test_Piece.Effect_Hunger,
         Message   => "Loaded piece did not give effects");

      -- Check third piece in list
      Another_Piece_Position3 :=
        Piece.Server.Pieces_Server_List.Element
          (Piece.Server.Pieces_Server_List.Last (Another_Pieces_List));

      AUnit.Assertions.Assert
        (Condition =>
           Another_Piece_Position3.Actual_Pos = Hexagon.Type_Hexagon_Position'(True, 2, 4),
         Message => "Loaded position not as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position3.Actual_Piece.all.Id = 1914,
         Message   => "Loaded piece does not have id as expected");

      AUnit.Assertions.Assert
        (Condition => Another_Piece_Position3.Actual_Piece.all.Name = "The name 3",
         Message   => "Loaded piece does not have name as expected");

      AUnit.Assertions.Assert
        (Condition =>
           Test_Piece.Type_My_Test_Piece'Class (Another_Piece_Position3.Actual_Piece.all).Test = 54,
         Message => "Loaded piece does not have 'Test' as expected");

      Effect.Effect_List.Clear (Other_Effects);
      Other_Effects := Another_Piece_Position3.Actual_Piece.all.Effects_On_Piece;

      Text_IO.Put_Line ("LENGTH=" & Effect.Effect_List.Length (Other_Effects)'Img);
      AUnit.Assertions.Assert
        (Condition => Effect.Effect_List.Length (Other_Effects) = 2,
         Message   => "Loaded piece did not give effects");

      declare
         Other_Effect1, Other_Effect2 : Effect.Type_Effect;
      begin

         Other_Effect1 :=
           Effect.Effect_List.Element (Effect.Effect_List.First (Other_Effects));
         Other_Effect2 :=
           Effect.Effect_List.Element
             (Effect.Effect_List.Next (Effect.Effect_List.First (Other_Effects)));

         AUnit.Assertions.Assert
           (Condition => (Other_Effect1.Effect_Name /= Other_Effect2.Effect_Name),
            Message   => "Loaded piece did not give effects");

         AUnit.Assertions.Assert
           (Condition =>
              (Other_Effect1.Effect_Name = Test_Piece.Effect_First_Attack or
               Other_Effect1.Effect_Name = Test_Piece.Effect_Versatile) and
              (Other_Effect2.Effect_Name = Test_Piece.Effect_First_Attack or
               Other_Effect2.Effect_Name = Test_Piece.Effect_Versatile),
            Message => "Loaded piece did not give effects");

         AUnit.Assertions.Assert
           (Condition =>
              (Other_Effect1.Effect_Name = Test_Piece.Effect_First_Attack or
               Other_Effect1.Effect_Name = Test_Piece.Effect_Versatile),
            Message => "Loaded piece did not give effects");
      end;

      if Verbose then
         Text_IO.Put_Line ("tc_pieces_save_and_load.Test_Pieces_Save_And_Load_1_Element - exit");
      end if;

   end Test_Pieces_Save_And_Load_3_Element;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Pieces_Save_And_Load_Empty_List'Access,
         Name    => "Test saving a list of effects to a file");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Pieces_Save_And_Load_1_Element'Access,
         Name    => "Test saving and loading of 1 effect");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Pieces_Save_And_Load_3_Element'Access,
         Name    => "Test saving and loading of 3 effect");
   end Register_Tests;

end Tc_Pieces_Save_Load;
