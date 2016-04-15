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
with Landscape;
with Piece;
with Hexagon;
with Ada.Containers;
with Test_Piece;
with Utilities;
with Construction;
with Effect;
with Landscape.Server;

package body Tc_Landscape is

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Testing functionality in the Landscape package");
   end Name;

   procedure Test_Landscape_Prepare_Pieces_Here_Index_1
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is

      use Ada.Containers;
      use Piece;

      A_Patch : Landscape.Type_Patch :=
        Landscape.Type_Patch'
        (True,
         Hexagon.Type_Hexagon_Position'(True, 99, 99),
         Test_Piece.Landscape_Grass,
         Landscape.Pieces_Here_List.Empty_Vector,
         Construction.Construction_List.Empty_Set,
         Effect.Effect_List.Empty_Map);

      A_Piece : Piece.Type_Piece;
   begin
      A_Piece :=
        Piece.Type_Piece'
        (1,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("test"),
         1);
      Landscape.Pieces_Here_List.Append (A_Patch.Pieces_Here, A_Piece.Id);
      Landscape.Put_Pieces_Here (A_Patch.Pieces_Here);
      Landscape.Pieces_Here_Sort.Sort (A_Patch.Pieces_Here);

      AUnit.Assertions.Assert
        (Condition => Landscape.Pieces_Here_List.Length (A_Patch.Pieces_Here) = 1 and
                      Landscape.Pieces_Here_List.Element (A_Patch.Pieces_Here, 1) = 1,
         Message   => "Sorting of the pieces in pieces here are not as expected");

   end Test_Landscape_Prepare_Pieces_Here_Index_1;

   procedure Test_Landscape_Prepare_Pieces_Here_Index_2
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Containers;
      use Piece;

      A_Patch : Landscape.Type_Patch :=
        Landscape.Type_Patch'
        (True,
         Hexagon.Type_Hexagon_Position'(True, 99, 99),
         Test_Piece.Landscape_Grass,
         Landscape.Pieces_Here_List.Empty_Vector,
         Construction.Construction_List.Empty_Set,
         Effect.Effect_List.Empty_Map);
      A_Piece : Piece.Type_Piece;
   begin
      A_Piece :=
        Piece.Type_Piece'
        (2,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("test"),
         1);
      Landscape.Pieces_Here_List.Append (A_Patch.Pieces_Here, A_Piece.Id);
      Landscape.Put_Pieces_Here (A_Patch.Pieces_Here);

      AUnit.Assertions.Assert
        (Condition => Landscape.Pieces_Here_List.Length (A_Patch.Pieces_Here) = 1 and
                      Landscape.Pieces_Here_List.Element (A_Patch.Pieces_Here, 1) = 2,
         Message   => "Sorting of the pieces in pieces here are not as expected");

   end Test_Landscape_Prepare_Pieces_Here_Index_2;

   procedure Test_Landscape_Prepare_Pieces_Here_Index_3
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Containers;
      use Piece;

      A_Patch : Landscape.Type_Patch :=
        Landscape.Type_Patch'
        (True,
         Hexagon.Type_Hexagon_Position'(True, 99, 99),
         Test_Piece.Landscape_Grass,
         Landscape.Pieces_Here_List.Empty_Vector,
         Construction.Construction_List.Empty_Set,
         Effect.Effect_List.Empty_Map);

      A_Piece : Piece.Type_Piece;
   begin
      A_Piece :=
        Piece.Type_Piece'
        (6,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("test"),
         1);
      Landscape.Pieces_Here_List.Append (A_Patch.Pieces_Here, A_Piece.Id);
      A_Piece :=
        Piece.Type_Piece'
        (3,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("test"),
         1);
      Landscape.Pieces_Here_List.Append (A_Patch.Pieces_Here, A_Piece.Id);
      A_Piece :=
        Piece.Type_Piece'
        (1,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Utilities.RemoteString.To_Unbounded_String ("test"),
         1);
      Landscape.Pieces_Here_List.Append (A_Patch.Pieces_Here, A_Piece.Id);

      Landscape.Put_Pieces_Here (A_Patch.Pieces_Here);
      Landscape.Pieces_Here_Sort.Sort (A_Patch.Pieces_Here);
      Landscape.Put_Pieces_Here (A_Patch.Pieces_Here);

      AUnit.Assertions.Assert
        (Condition => Landscape.Pieces_Here_List.Length (A_Patch.Pieces_Here) = 3 and
                      Landscape.Pieces_Here_List.Element (A_Patch.Pieces_Here, 1) = 1 and
                      Landscape.Pieces_Here_List.Element (A_Patch.Pieces_Here, 2) = 3 and
                      Landscape.Pieces_Here_List.Element (A_Patch.Pieces_Here, 3) = 6,
         Message   => "Sorting of the pieces in pieces here are not as expected");

   end Test_Landscape_Prepare_Pieces_Here_Index_3;

   procedure Test_Landscape_Get_Landscape_Info
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Res : Landscape.Server.Type_Landscape_Type_Info;

      use Utilities.RemoteString;
   begin

      Landscape.Server.Init(Test_Piece.Landscapes_Type_Info_List);

      Res := Landscape.Server.Get_Landscape_Info (Landscape.Type_Landscape(Test_Piece.Landscape_Grass));

      AUnit.Assertions.Assert
        (Condition => Res.Max_Pieces_Here = 6,
         Message   => "Get_Landscape_Info did not get the Max_Pieces='6' but " & Res.Max_Pieces_Here'Img);
      AUnit.Assertions.Assert
        (Condition => Res.Type_Name = Utilities.RemoteString.To_Unbounded_String("Grass"),
         Message   => "Get_Landscape_Info did not get the Type_Name='6' but " & Utilities.RemoteString.To_String(Res.Type_Name) );
   end Test_Landscape_Get_Landscape_Info;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Landscape_Prepare_Pieces_Here_Index_1'Access,
         Name    => "Landscape Test Prepare_Pieces_Here 1");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Landscape_Prepare_Pieces_Here_Index_2'Access,
         Name    => "Landscape Test Prepare_Pieces_Here 2");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Landscape_Prepare_Pieces_Here_Index_3'Access,
         Name    => "Landscape Test Prepare_Pieces_Here 3");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Landscape_Get_Landscape_Info'Access,
         Name    => "Test Landscape Get Landscape Info");


   end Register_Tests;

end Tc_Landscape;
