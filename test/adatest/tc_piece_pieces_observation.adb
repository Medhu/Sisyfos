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

with Ada.Containers;
with Piece;
with Hexagon;
with AUnit.Assertions;
with Text_IO;
with Observation;

package body Tc_Piece_Pieces_Observation is

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test that the set used for observations works as intended");
   end Name;

   procedure Test_Observation_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;

      Test        : Observation.Observation_Of_Patches.Observations_Of_Patches.Set;
      use Ada.Containers;
   begin

      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(1), Hexagon.Type_Hexagon_Numbers'(3), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 1,
         Message   => "Pieces observations doesnt work");

      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(1), Hexagon.Type_Hexagon_Numbers'(5), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 2,
         Message   => "Pieces observations doesnt work");

      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(1), Hexagon.Type_Hexagon_Numbers'(6), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 3,
         Message   => "Pieces observations doesnt work");

      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(1), Hexagon.Type_Hexagon_Numbers'(8), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 4,
         Message   => "Pieces observations doesnt work");

      --
      --
      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(3), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 5,
         Message   => "Pieces observations doesnt work");

      Observation.Observation_Of_Patches.Observations_Of_Patches.Include
        (Test,
         Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(6), True));
      AUnit.Assertions.Assert
        (Condition => Observation.Observation_Of_Patches.Observations_Of_Patches.Length (Test) = 6,
         Message   => "Pieces observations doesnt work");

   end Test_Observation_1;

   procedure Test_Less_Than_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result : Boolean := True;

      use Ada.Containers;
   begin

      Result :=
         Observation.Observation_Of_Patches.Left_Less_Observation
           (Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(4), True),
            Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(6), True));

      AUnit.Assertions.Assert (Condition => Result, Message => "Pieces observations doesnt work");

   end Test_Less_Than_1;

   procedure Test_Less_Than_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result : Boolean := True;

      use Ada.Containers;
   begin

      Result :=
         Observation.Observation_Of_Patches.Left_Less_Observation
           (Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(4), True),
            Observation.Observation_Of_Patches.Type_Observed_Patch'(Hexagon.Type_Hexagon_Numbers'(3), Hexagon.Type_Hexagon_Numbers'(6), True));

      AUnit.Assertions.Assert (Condition => Result, Message => "Pieces observations doesnt work");

   end Test_Less_Than_2;

   procedure Test_Find_Delta_Observed_Pieces_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = False and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 1 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.P_Valid = False and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Piece_Here_Id = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.P_Valid = False and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Piece_Here_Id = 4 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 3;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Pieces observations of piece 1, 3 and 4 in previous didnr work");

   end Test_Find_Delta_Observed_Pieces_1;

   procedure Test_Find_Delta_Observed_Pieces_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.A = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.B = 1 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 1 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.A = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.B = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Piece_Here_Id = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.A = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.B = 4 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Piece_Here_Id = 4 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 3;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Pieces observations piece 1, 3 and 4 only in current observation");

   end Test_Find_Delta_Observed_Pieces_2;

   procedure Test_Find_Delta_Observed_Pieces_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
     Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 3, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 3, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 3, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.A = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.B = 1 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 1 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.A = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.B = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Piece_Here_Id = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.A = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Pos.B = 4 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 3).Piece_Here_Id = 4 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 3;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Pieces observations of piece  1, 3 and get new observations");

   end Test_Find_Delta_Observed_Pieces_3;

   procedure Test_Find_Delta_Observed_Pieces_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 2), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result := Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = True and
               Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.A = 2 and
                Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.B = 2 and
                Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 3 and
                Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 1;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Pieces observations piece 1, 3 and 4 and piece 3 moves");

   end Test_Find_Delta_Observed_Pieces_4;

   procedure Test_Find_Delta_Observed_Pieces_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 2), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 10, 14), Piece.Type_Piece_Id'(5)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.A = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.B = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.A = 10 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.B = 14 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Piece_Here_Id = 5 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 2;

      AUnit.Assertions.Assert (Condition => Result, Message => "Pieces observations doesnt work");

   end Test_Find_Delta_Observed_Pieces_5;

   procedure Test_Find_Delta_Observed_Pieces_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Hexagon;
      Result             : Boolean := True;
      Current, Previous  : Observation.Observation_Of_Pieces.Observations_Of_Pieces.Set;
      Observation_Result : Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;

      use Piece;
      use Ada.Containers;
   begin
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 3), Piece.Type_Piece_Id'(3)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Previous,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 4), Piece.Type_Piece_Id'(4)));

      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 1), Piece.Type_Piece_Id'(1)));
      Observation.Observation_Of_Pieces.Observations_Of_Pieces.Include
        (Current,
         Observation.Observation_Of_Pieces.Type_Observed_Piece'(Hexagon.Type_Hexagon_Position'(True, 2, 2), Piece.Type_Piece_Id'(3)));

      Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces (Current, Previous, Observation_Result);

      Result :=
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.P_Valid = True and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.A = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Pos.B = 2 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 1).Piece_Here_Id = 3 and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Pos.P_Valid = False and
        Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Observation_Result, 2).Piece_Here_Id = 4 and
       Observation.Observation_Of_Pieces.Changes_To_Pieces.Length (Observation_Result) = 2;

      AUnit.Assertions.Assert (Condition => Result, Message => "Pieces observations doesnt work");

   end Test_Find_Delta_Observed_Pieces_6;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Observation_1'Access,
         Name    => "Inserting a list of observations and checking length of list");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Less_Than_1'Access,
         Name    => "Testing A=3, B=4, piece id=2 vs A=3, B=6, piece id=1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Less_Than_2'Access,
         Name    => "Testing A=3, B=4, piece id=3 vs A=3, B=6, piece id=1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_1'Access,
         Name    => "Test Find_Delta_Observed_Pieces_1 Only pieces in previous observation");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_2'Access,
         Name    => "Test Find_Delta_Observed_Pieces_2 Only pieces in current observation");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_3'Access,
         Name    => "Test Find_Delta_Observed_Pieces_3 Every pieces in current observation replace previous pieces");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_4'Access,
         Name    => "Test Find_Delta_Observed_Pieces_4 Only piece 3 moves");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_5'Access,
         Name    => "Test Find_Delta_Observed_Pieces_5 Only piece 3 moves and a new piece occur");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Delta_Observed_Pieces_6'Access,
         Name    => "Test Find_Delta_Observed_Pieces_6 Only piece 3 moves and piece 4 disappear");

   end Register_Tests;

end Tc_Piece_Pieces_Observation;
