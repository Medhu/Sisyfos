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

with Text_IO;
with AUnit.Assertions;
with Effect;
with Effect.Server;
with Ada.Containers;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Test_Piece;

package body Tc_Effects_List is

   Verbose : constant Boolean := True;
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Testing functionality for including/excluding and updating in Effect_Lists");
   end Name;

   procedure Test_Effects_Include_One_Effect
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List : Effect.Effect_List.Map;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_One_Effect - enter");
      end if;

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Weather, Effect.Type_Effect'(Test_Piece.Effect_Weather,1));

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(An_Effect_List) = 1,
           Message   => "Did not include effects as expected");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_One_Effect - exit");
      end if;

   end Test_Effects_Include_One_Effect;

   procedure Test_Effects_Include_Three_Effects
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List : Effect.Effect_List.Map;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_Three_Effects - enter");
      end if;

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Weather, Effect.Type_Effect'(Test_Piece.Effect_Weather,1));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Courage, Effect.Type_Effect'(Test_Piece.Effect_Courage,1));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_First_Attack, Effect.Type_Effect'(Test_Piece.Effect_First_Attack,1));

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(An_Effect_List) = 3,
           Message   => "Expected 3 items");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_Three_Effects - exit");
      end if;

   end Test_Effects_Include_Three_Effects;

   procedure Test_Effects_Include_Three_Effects_Update
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List : Effect.Effect_List.Map;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_Three_Effects_Update - enter");
      end if;

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Weather, Effect.Type_Effect'(Test_Piece.Effect_Weather,1));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Courage, Effect.Type_Effect'(Test_Piece.Effect_Courage,1));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_First_Attack, Effect.Type_Effect'(Test_Piece.Effect_First_Attack,1));

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(An_Effect_List) = 3,
           Message   => "Expected 3 items (a)");

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_First_Attack, Effect.Type_Effect'(Test_Piece.Effect_First_Attack,1));

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(An_Effect_List) = 3,
           Message   => "Expected 3 items (b)");

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_First_Attack, Effect.Type_Effect'(Test_Piece.Effect_First_Attack,6));

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(An_Effect_List) = 3,
           Message   => "Expected 3 items (b)");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Include_Three_Effects_Update - exit");
      end if;

   end Test_Effects_Include_Three_Effects_Update;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Effects_Include_One_Effect'Access,
         Name    => "Test including one effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Effects_Include_Three_Effects'Access,
         Name    => "Test including three effect");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Effects_Include_Three_Effects_Update'Access,
         Name    => "Test including three effect");

   end Register_Tests;

end Tc_Effects_List;
