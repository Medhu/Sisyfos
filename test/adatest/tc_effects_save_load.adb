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

with Text_IO;
with AUnit.Assertions;
with Effect;
with Effect.Server;
with Ada.Containers;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Test_Piece;

package body Tc_Effects_Save_Load is

   Verbose : constant Boolean := False;
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Testing functionality for saving and loading effects");
   end Name;

   procedure Test_Effects_Save_And_Load_Empty_List
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List, Another_Effects_List : Effect.Effect_List.Map;
      The_Stream : Ada.Streams.Stream_IO.Stream_Access;
      A_File : Ada.Streams.Stream_IO.File_Type;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_Empty_List - enter");
      end if;

      Ada.Streams.Stream_IO.Create(A_File, Ada.Streams.Stream_IO.Out_File,"Test_Effects_Save_And_Load_Empty_List.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Save_Effects(The_Stream, An_Effect_List);

      Ada.Streams.Stream_IO.Close(A_File);


      Ada.Streams.Stream_IO.Open(A_File, Ada.Streams.Stream_IO.In_File, "Test_Effects_Save_And_Load_Empty_List.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Load_Effects(The_Stream, Another_Effects_List);

      Ada.Streams.Stream_IO.Close(A_File);

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(Another_Effects_List) = 0,
           Message   => "Did not save effects as expected");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_Empty_List - exit");
      end if;

   end Test_Effects_Save_And_Load_Empty_List;

   procedure Test_Effects_Save_And_Load_List_1_Element
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List, Another_Effects_List : Effect.Effect_List.Map;
      The_Stream : Ada.Streams.Stream_IO.Stream_Access;
      A_File : Ada.Streams.Stream_IO.File_Type;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_List_1_Element - enter");
      end if;

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Weather, Effect.Type_Effect'(Test_Piece.Effect_Weather,1));
      Ada.Streams.Stream_IO.Create(A_File, Ada.Streams.Stream_IO.Out_File,"Test_Effects_Save_And_Load_List_1_Element.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Save_Effects(The_Stream, An_Effect_List);

      Ada.Streams.Stream_IO.Close(A_File);


      Ada.Streams.Stream_IO.Open(A_File, Ada.Streams.Stream_IO.In_File, "Test_Effects_Save_And_Load_List_1_Element.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Load_Effects(The_Stream, Another_Effects_List);

      Ada.Streams.Stream_IO.Close(A_File);

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(Another_Effects_List) = 1,
           Message   => "Did not save effects as expected");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_List_1_Element - exit");
      end if;

   end Test_Effects_Save_And_Load_List_1_Element;

   procedure Test_Effects_Save_And_Load_List_3_Element
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      An_Effect_List, Another_Effects_List : Effect.Effect_List.Map;
      The_Stream : Ada.Streams.Stream_IO.Stream_Access;
      A_File : Ada.Streams.Stream_IO.File_Type;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_List_3_Element - enter");
      end if;

      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Weather, Effect.Type_Effect'(Test_Piece.Effect_Weather,1));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_Hunger, Effect.Type_Effect'(Test_Piece.Effect_Hunger,2));
      Effect.Effect_List.Include(An_Effect_List, Test_Piece.Effect_First_Attack, Effect.Type_Effect'(Test_Piece.Effect_First_Attack,3));

      Ada.Streams.Stream_IO.Create(A_File, Ada.Streams.Stream_IO.Out_File,"Test_Effects_Save_And_Load_List_3_Element.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Save_Effects(The_Stream, An_Effect_List);

      Ada.Streams.Stream_IO.Close(A_File);


      Ada.Streams.Stream_IO.Open(A_File, Ada.Streams.Stream_IO.In_File, "Test_Effects_Save_And_Load_List_3_Element.dat");

      The_Stream := Ada.Streams.Stream_IO.Stream(A_File);

      Effect.Server.Load_Effects(The_Stream, Another_Effects_List);

      Ada.Streams.Stream_IO.Close(A_File);

        AUnit.Assertions.Assert
          (Condition => Effect.Effect_List.Length(Another_Effects_List) = 3,
           Message   => "Did not save effects as expected");

      if Verbose then
         Text_IO.Put_Line("tc_effect_save_load.Test_Effects_Save_And_Load_List_3_Element - exit");
      end if;

   end Test_Effects_Save_And_Load_List_3_Element;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Effects_Save_And_Load_Empty_List'Access,
         Name    => "Test saving a list of effects to a file");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Effects_Save_And_Load_List_1_Element'Access,
           Name    => "Test saving and loading of 1 effect");
        AUnit.Test_Cases.Registration.Register_Routine
          (Test    => T,
           Routine => Test_Effects_Save_And_Load_List_3_Element'Access,
           Name    => "Test saving and loading of 3 effect");
   end Register_Tests;

end Tc_Effects_Save_Load;
