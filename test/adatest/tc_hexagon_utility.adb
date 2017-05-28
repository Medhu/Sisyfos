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

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Ada.Containers;
with Piece;
with Hexagon.Utility;
with Hexagon.Area;
with Hexagon.Area.Server_Area;
with Text_IO;
with Hexagon.Server_Map;
with Piece.Server;
with Server.ServerAPI;
with Client.Server_Adm;
with Player;
with Hexagon.Client_Map;
with Piece.Client_Piece;
with Utilities;
with Landscape;
with Status;
with Test_Piece;
with Server;
with Test_ServerRCI;
with Action;
with Ada.Strings;

package body Tc_Hexagon_Utility is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

   type Type_Answer_Path is array (Positive range <>) of Hexagon.Type_Hexagon_Position;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line     : Utilities.RemoteString.Type_Command_Parameters;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Set_Up - enter");
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

      Player_Id_1 :=
        Client.Server_Adm.Join_Game
          (Utilities.RemoteString.To_Unbounded_String ("User A"),
           Adm_Status);
      Player_Id_2 :=
        Client.Server_Adm.Join_Game
          (Utilities.RemoteString.To_Unbounded_String ("User B"),
           Adm_Status);

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Set_Up - exit");
      end if;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Tear_Down - enter");
      end if;

      Server.ServerAPI.Stop;

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Tear_Down - exit");
      end if;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test functions in Hexagon-Utility");
   end Name;

   procedure Test_Hexagon_Distance_1_1_To_2_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_1_1_To_2_3 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 1, 1),
              Hexagon.Type_Hexagon_Position'(True, 2, 3)) =
           3,
         Message => "Didnt find distance from 1, 1 to 2, 3");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_1_1_To_2_3 - exit");
      end if;
   end Test_Hexagon_Distance_1_1_To_2_3;

   procedure Test_Hexagon_Distance_15_15_To_18_15
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_18_15 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 18, 15)) =
           3,
         Message => "Didnt find distance from 15, 15 to 15, 18");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_18_15 - exit");
      end if;

   end Test_Hexagon_Distance_15_15_To_18_15;

   procedure Test_Hexagon_Distance_15_15_To_18_12
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_18_12 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 18, 12)) =
           3,
         Message => "Didnt find distance from 15, 15 to 18, 12");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_18_12 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_18_12;

   procedure Test_Hexagon_Distance_15_15_To_13_14
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_13_14 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 13, 14)) =
           3,
         Message => "Didnt find distance from 15, 15 to 13, 14");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_13_14 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_13_14;

   procedure Test_Hexagon_Distance_15_15_To_16_20
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_16_20 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 16, 20)) =
           6,
         Message =>
           "Didnt find distance from 15, 15 to 16, 20 " &
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 16, 20))'
             Img);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_16_20 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_16_20;

   procedure Test_Hexagon_Distance_15_15_To_19_13
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_19_13 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 19, 13)) =
           4,
         Message =>
           "Didnt find distance from 15, 15 to 19, 13 " &
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 19, 13))'
             Img);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_19_13 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_19_13;

   procedure Test_Hexagon_Distance_15_15_To_17_11
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_17_11 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 17, 11)) =
           4,
         Message =>
           "Didnt find distance from 15, 15 to 17, 11 " &
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 17, 11))'
             Img);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_17_11 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_17_11;

   procedure Test_Hexagon_Distance_15_15_To_11_13
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_11_13 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 11, 13)) =
           6,
         Message =>
           "Didnt find distance from 15, 15 to 11, 13 " &
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 11, 13))'
             Img);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_11_13 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_11_13;

   procedure Test_Hexagon_Distance_15_15_To_13_11
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_13_11 - enter");
      end if;

      AUnit.Assertions.Assert
        (Condition =>
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 13, 11)) =
           6,
         Message =>
           "Didnt find distance from 15, 15 to 13, 11 " &
           Hexagon.Utility.Hexagon_Distance
             (Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 13, 11))'
             Img);

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Hexagon_Distance_15_15_To_13_11 - exit");
      end if;
   end Test_Hexagon_Distance_15_15_To_13_11;

   procedure Test_Find_Path_Along_Axis_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 16, 14), (True, 17, 13), (True, 18, 12));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 12;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 18, 12");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1 - exit");
      end if;
   end Test_Find_Path_Along_Axis_1;

   procedure Test_Find_Path_Along_Axis_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 15, 14), (True, 15, 13), (True, 15, 12));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 12;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,

         Message => "Didnt find path from 15, 15 to 15, 12");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2 - exit");
      end if;
   end Test_Find_Path_Along_Axis_2;

   procedure Test_Find_Path_Along_Axis_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 14, 15), (True, 13, 15), (True, 12, 15));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 15;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 12, 15");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3 - exit");
      end if;
   end Test_Find_Path_Along_Axis_3;

   procedure Test_Find_Path_Along_Axis_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 14, 16), (True, 13, 17), (True, 12, 18));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 12, 18");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4 - exit");
      end if;
   end Test_Find_Path_Along_Axis_4;

   procedure Test_Find_Path_Along_Axis_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 15, 16), (True, 15, 17), (True, 15, 18));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 15, 18");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5 - exit");
      end if;
   end Test_Find_Path_Along_Axis_5;

   procedure Test_Find_Path_Along_Axis_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 16, 15), (True, 17, 15), (True, 18, 15));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 15;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 18, 15");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6 - exit");
      end if;
   end Test_Find_Path_Along_Axis_6;
   --
   --
   procedure Test_Find_Path_to_15_15_to_18_13 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 16, 15), (True, 17, 14), (True, 18, 13));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_18_13 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 13;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 18, 13");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_18_13 - exit");
      end if;

   end Test_Find_Path_to_15_15_to_18_13;

   procedure Test_Find_Path_to_15_15_to_17_12 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 15, 14), (True, 16, 13), (True, 17, 12));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_17_12 - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 17;
      To_B   := 12;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 17, 12");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_17_12 - exit");
      end if;
   end Test_Find_Path_to_15_15_to_17_12;

   procedure Test_Find_Path_to_15_15_to_13_18 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 14, 16), (True, 13, 17), (True, 13, 18));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 15, 15 to 13, 18");
   end Test_Find_Path_to_15_15_to_13_18;

   procedure Test_Find_Path_to_15_15_to_13_18_Reach_Problem
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 16, 15),
         (True, 16, 16),
         (True, 15, 17),
         (True, 14, 18),
         (True, 13, 18));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      declare
         A_Patch_To_Change : Hexagon.Server_Map.Type_Server_Patch_Adress;
      begin
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 15);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Water);
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 16);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Water);
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 16);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Water);

      end;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      declare
         A_Patch_To_Change : Hexagon.Server_Map.Type_Server_Patch_Adress;
      begin
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 15);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Grass);
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 16);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Grass);
         A_Patch_To_Change := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 16);
         A_Patch_To_Change.all.Landscape_Here :=
           Landscape.Type_Landscape'(Test_Piece.Landscape_Grass);

      end;

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used.");
   end Test_Find_Path_to_15_15_to_13_18_Reach_Problem;

   procedure Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path := ((True, 15, 15), (True, 14, 16), (True, 13, 17), (True, 13, 18));
      Result : Boolean;

      Active_Piece : Piece.Type_Piece;
      A_Patch      : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Hexagon;
      use Ada.Containers;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces - enter");
      end if;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_01.html"));

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1000),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1000).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1000).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1001),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1001).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1001).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1002),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_04.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1002).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1002).Result'Img);

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;

         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 15, 15 to 13, 18 - can pass because there is one piece on each patch in front");
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces - exit");
      end if;

   end Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces;

   procedure Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 16, 15),
         (True, 16, 16),
         (True, 15, 17),
         (True, 14, 18),
         (True, 13, 18));
      Result : Boolean;

      Active_Piece : Piece.Type_Piece;

      use Hexagon;
      use Ada.Containers;
      use Piece;
      use Status;
   begin

      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces - enter");
      end if;

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1003),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_01.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1003).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1003).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1004),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_02.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1004).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1004).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1005),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_03.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1005).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1005).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1006),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_04.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1006).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1006).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1007),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_05.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1007).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1007).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1008),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_06.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1008).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1008).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1009),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_07.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1009).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1009).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1010),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_08.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1010).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1010).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1011),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_09.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1011).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1011).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1012),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_10.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1012).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1012).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1013),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_11.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1013).Result = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1014),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_12.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1014).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1014).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1015),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_13.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1015).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1015).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1016),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_14.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1016).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1016).Result'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1017),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_15.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1017).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1017).Result'Img);

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;

         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used.");
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces - exit");
      end if;

   end Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces;

   procedure Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;
      A_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 15, 14),
         (True, 14, 14),
         (True, 13, 15),
         (True, 13, 16),
         (True, 13, 17),
         (True, 13, 18));
      Result : Boolean;

      Active_Piece : Piece.Type_Piece;

      use Hexagon;
      use Ada.Containers;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy - enter");
      end if;

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 16, 16);
      Piece.Client_Piece.Create_Piece
        (Player.Type_Player_Id (2),
         Action.Type_Action_Type (1018),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all));

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy_01.html"));

      AUnit.Assertions.Assert
        (Condition => Test_Piece.Test_List.all (1018).Result = Status.Ok,
         Message   =>
           "Bad status when putting a piece " & Test_Piece.Test_List.all (1018).Result'Img);

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;

         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used.");
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy - exit");
      end if;

   end Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy;

   procedure Test_Find_Path_to_50_100_to_53_100_Along_Edge
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 50, 100), (True, 51, 100), (True, 52, 100), (True, 53, 100));
      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_50_100_to_53_100_Along_Edge - enter");
      end if;

      From_A := 50;
      From_B := 100;
      To_A   := 53;
      To_B   := 100;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   => "Didnt find path from 50, 100 to 53, 100");

      if Verbose then
         Text_IO.Put_Line
           ("TC_hexagon_utility.Test_Find_Path_to_50_100_to_53_100_Along_Edge - exit");
      end if;

   end Test_Find_Path_to_50_100_to_53_100_Along_Edge;

   procedure Test_Find_Path_to_5_4_to_9_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 5, 4), (True, 6, 4), (True, 7, 3), (True, 8, 2), (True, 9, 1));

      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_5_4_to_9_1 - enter");
      end if;

      From_A := 5;
      From_B := 4;
      To_A   := 9;
      To_B   := 1;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert (Condition => Result, Message => "Didnt find path from 5, 4 to 9, 1");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_5_4_to_9_1 - exit");
      end if;

   end Test_Find_Path_to_5_4_to_9_1;

   procedure Test_Find_Path_to_5_3_to_8_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 5, 3), (True, 5, 4), (True, 6, 4), (True, 7, 4), (True, 8, 4));

      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_5_3_to_8_4 - enter");
      end if;

      From_A := 5;
      From_B := 3;
      To_A   := 8;
      To_B   := 4;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert (Condition => Result, Message => "Didnt find path from 5, 3 to 8, 4");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_5_3_to_8_4 - exit");
      end if;

   end Test_Find_Path_to_5_3_to_8_4;

   procedure Test_Find_Path_to_4_6_to_3_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 4, 6), (True, 3, 6), (True, 3, 5), (True, 3, 4), (True, 3, 3));

      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_4_6_to_3_3 - enter");
      end if;

      From_A := 4;
      From_B := 6;
      To_A   := 3;
      To_B   := 3;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 4, 6 to 3, 3");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_4_6_to_3_3 - exit");
      end if;

   end Test_Find_Path_to_4_6_to_3_3;
   --
   --

   procedure Test_Find_Path_to_17_26_to_12_15 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 17, 26), (True, 16, 26), (True, 15, 26), (True, 14, 26), (True, 13, 26), (True, 12, 26),
        (True, 12, 25), (True, 12, 24), (True, 12, 23), (True, 12,22), (True, 12, 21),
        (True, 12, 20), (True, 12, 19), (True, 12, 18), (True, 12, 17), (True, 12, 16), (True, 12, 15)
        );

      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_17_26_to_12_15 - enter");
      end if;

      From_A := 17;
      From_B := 26;
      To_A   := 12;
      To_B   := 15;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 17, 26 to 12, 15");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_17_26_to_12_15 - exit");
      end if;

   end Test_Find_Path_to_17_26_to_12_15;

   procedure Test_Find_Path_to_17_26_to_11_16 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 17, 26), (True, 16, 26), (True, 15, 26), (True, 14, 26), (True, 13, 26), (True, 12, 26),
        (True, 12, 25), (True, 12, 24), (True, 12, 23), (True, 11,23), (True, 11, 22),
        (True, 11, 21), (True, 11, 20), (True, 11, 19), (True, 11, 18), (True, 11, 17), (True, 11, 16)
        );

      Result : Boolean;

      use Hexagon;
      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_17_26_to_12_15 - enter");
      end if;

      From_A := 17;
      From_B := 26;
      To_A   := 11;
      To_B   := 16;

      Hexagon.Utility.Find_Path
        (Player.Type_Player_Id (1),
         Test_Piece.Sentry_Piece,
         Hexagon.Type_Hexagon_Position'(True, From_A, From_B),
         Hexagon.Type_Hexagon_Position'(True, To_A, To_B),
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         if Hexagon.Path.Element (Trav).A /= Answer (Hexagon.Path.To_Index (Trav)).A or
           Hexagon.Path.Element (Trav).B /= Answer (Hexagon.Path.To_Index (Trav)).B
         then
            Result := False;
         end if;
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 17, 26 to 12, 15");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_17_26_to_12_15 - exit");
      end if;

   end Test_Find_Path_to_17_26_to_11_16;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_1_1_To_2_3'Access,
         Name    => "Find distance from 1, 1 to 2, 3");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_18_15'Access,
         Name    => "Find distance from 15, 15 to 18, 15");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_18_12'Access,
         Name    => "Find distance from 15, 15 to 18, 12");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_13_14'Access,
         Name    => "Find distance from 15, 15 to 13, 14");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_16_20'Access,
         Name    => "Find distance from 15, 15 to 16, 20");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_19_13'Access,
         Name    => "Find distance from 15, 15 to 19, 13");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_17_11'Access,
         Name    => "Find distance from 15, 15 to 17, 11");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_11_13'Access,
         Name    => "Find distance from 15, 15 to 11, 13");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Hexagon_Distance_15_15_To_13_11'Access,
         Name    => "Find distance from 15, 15 to 13, 11");
      --
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_1'Access,
         Name    => "Path Test find a straight path along axis 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_2'Access,
         Name    => "Path Test find a straight path along axis 2");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_3'Access,
         Name    => "Path Test find a straight path along axis 3");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_4'Access,
         Name    => "Path Test find a straight path along axis 4");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_5'Access,
         Name    => "Path Test find a straight path along axis 5");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_6'Access,
         Name    => "Path Test find a straight path along axis 6");
      --
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_18_13'Access,
         Name    => "Path Test find to 15, 15 to 18, 13");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_17_12'Access,
         Name    => "Path Test find to 15, 15 to 17, 12");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18'Access,
         Name    => "Path Test find to 15, 15 to 13, 18");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Reach_Problem'Access,
         Name    =>
           "Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used.");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces'Access,
         Name    =>
           "Path Test find to 15, 15 to 13, 18 - passing through 3 patches with one piece on each");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces'Access,
         Name    =>
           "Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 pieces in front");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy'Access,
         Name    =>
           "Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 enemy pieces in front");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_50_100_to_53_100_Along_Edge'Access,
         Name    => "Path Test find to 50, 100 to 53, 100 - Along edge");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_5_4_to_9_1'Access,
         Name    => "Path Test find to 5, 4 to 9, 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_5_3_to_8_4'Access,
         Name    => "Path Test find to 5, 3 to 8, 4");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_4_6_to_3_3'Access,
         Name    => "Path Test find to 4, 6 to 3, 3");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_17_26_to_12_15'Access,
         Name    => "Path Test find to 17, 26 to 12, 15");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_17_26_to_11_16'Access,
         Name    => "Path Test find to 17, 26 to 11, 16");

      --

   end Register_Tests;

end Tc_Hexagon_Utility;
