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
with Server.Server;
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

package body Tc_Hexagon_Utility is
   Verbose : constant Boolean := True;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;

   Test_Capability_Area, Test_Capability_Area2, Test_Capability_Area_Knight :
     Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   type Type_Answer_Path is array (Positive range <>) of Hexagon.Type_Hexagon_Position;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Set_Up - enter");
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
         Test_Piece.Test_Start_Turn'Access,
         Test_Piece.Test_End_Turn'Access,
         Test_Piece.Test_End_Game'Access);
      Server.Server.Start;

      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User A"));
      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User B"));
      Test_ServerRCI.Create_Game
        (Utilities.RemoteString.To_Unbounded_String ("test0000.dat"),
         Player_Name_List,
         Adm_Status);

      Player_Id_1 :=
         Client.Server_Adm.Join_Game (Utilities.RemoteString.To_Unbounded_String ("User A"), Adm_Status);
      Player_Id_2 :=
         Client.Server_Adm.Join_Game (Utilities.RemoteString.To_Unbounded_String ("User B"), Adm_Status);

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      -- Area stolen from Tower's observation area.
      Test_Capability_Area :=
        new Hexagon.Area.Type_Action_Capabilities'
        (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 1),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -3),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 3));

      Test_Capability_Area2 :=
        new Hexagon.Area.Type_Action_Capabilities'
        (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 2),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 1),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -3),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, -1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 1),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 2),

         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 3),
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 3));

      Test_Capability_Area_Knight :=
        new Hexagon.Area.Type_Action_Capabilities'
        (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
      -- group I
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 4),--1
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 3),--3
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 1),--6
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 4, -1),--8
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 4, -3),--11
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -4),--13
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -4),--16
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -3),--18
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, -1),--21
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -4, 1),--23
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -4, 3),--26
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 4),--28
      --group II
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),--2
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),--7
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),--12
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),--17
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),--22
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),--27
      --group III
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),--4
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),--31
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),--9
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),--32
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),--14
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),--33
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),--19
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),--34
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),--24
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),--35
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),--29
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),--36
      --group IV
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),--5
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),--10
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--20
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--25
         Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--30
        );
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Set_Up - exit");
      end if;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Tear_Down - enter");
      end if;

      Server.Server.Stop;

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
        (Condition => Hexagon.Utility.Hexagon_Distance (1, 1, 2, 3) = 3,
         Message   => "Didnt find distance from 1, 1 to 2, 3");

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 18, 15) = 3,
         Message   => "Didnt find distance from 15, 15 to 15, 18");

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 18, 12) = 3,
         Message   => "Didnt find distance from 15, 15 to 18, 12");

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 13, 14) = 3,
         Message   => "Didnt find distance from 15, 15 to 13, 14");

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 16, 20) = 6,
         Message   => "Didnt find distance from 15, 15 to 16, 20 " &
                      Hexagon.Utility.Hexagon_Distance (15, 15, 16, 20)'Img);

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 19, 13) = 4,
         Message   => "Didnt find distance from 15, 15 to 19, 13 " &
                      Hexagon.Utility.Hexagon_Distance (15, 15, 19, 13)'Img);

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 17, 11) = 4,
         Message   => "Didnt find distance from 15, 15 to 17, 11 " &
                      Hexagon.Utility.Hexagon_Distance (15, 15, 17, 11)'Img);

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 11, 13) = 6,
         Message   => "Didnt find distance from 15, 15 to 11, 13 " &
                      Hexagon.Utility.Hexagon_Distance (15, 15, 11, 13)'Img);

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
        (Condition => Hexagon.Utility.Hexagon_Distance (15, 15, 13, 11) = 6,
         Message   => "Didnt find distance from 15, 15 to 13, 11 " &
                      Hexagon.Utility.Hexagon_Distance (15, 15, 13, 11)'Img);

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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 16, 14),
         (True, 17, 13),
         (True, 18, 12));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 15, 14),
         (True, 15, 13),
         (True, 15, 12));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

         Message   => "Didnt find path from 15, 15 to 15, 12");

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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 14, 15),
         (True, 13, 15),
         (True, 12, 15));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 14, 16),
         (True, 13, 17),
         (True, 12, 18));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 15, 16),
         (True, 15, 17),
         (True, 15, 18));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 16, 15),
         (True, 17, 15),
         (True, 18, 15));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 16, 15),
         (True, 17, 14),
         (True, 18, 13));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 15, 14),
         (True, 16, 13),
         (True, 17, 12));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 14, 16),
         (True, 13, 17),
         (True, 13, 18));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area2,
         Hexagon.Utility.Move,
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
"Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used."
);
   end Test_Find_Path_to_15_15_to_13_18_Reach_Problem;

   procedure Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 15, 15),
         (True, 14, 16),
         (True, 13, 17),
         (True, 13, 18));
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

      Piece.Server.Print_Pieces_In_Game;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_01.html"));

      Piece.Server.Print_Pieces_In_Game;

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_02.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      Piece.Server.Print_Pieces_In_Game;

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      Piece.Server.Print_Pieces_In_Game;

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces_04.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);
      Piece.Server.Print_Pieces_In_Game;

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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
"Didnt find path from 15, 15 to 13, 18 - can pass because there is one piece on each patch in front"
);
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
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_01.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_02.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_03.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_04.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_05.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_06.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_07.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_08.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_09.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_10.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_11.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_12.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 15);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_13.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 14, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_14.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 15, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces_15.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);
      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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
         Text_IO.Put_Line
           ("Hexagon.Path.Element (Trav).A=" &
            Hexagon.Path.Element (Trav).A'Img &
            " Hexagon.Path.Element (Trav).B=" &
            Hexagon.Path.Element (Trav).B'Img);
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
"Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used."
);
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

      AUnit.Assertions.Assert
        (Condition => Client.Server_Adm.End_Turn (1),
         Message   => "Can't give turn to the other player");

      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 16, 16);
      Piece.Client_Piece.Create_Piece
        (Action.Type_Action_Type(1),
         Active_Piece,
         Test_Piece.Sentry_Piece,
         Piece.Fighting_Piece,
         Landscape.Type_Patch (A_Patch.all),
         2,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
            (Test_Piece.HTML_Path & "TC_hexagon_utility-Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy_01.html"));
      Piece.Server.Print_Pieces_In_Game;

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Bad status when putting a piece " & Ret_Status'Img);

      From_A := 15;
      From_B := 15;
      To_A   := 13;
      To_B   := 18;

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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
         Text_IO.Put_Line
           ("Hexagon.Path.Element (Trav).A=" &
            Hexagon.Path.Element (Trav).A'Img &
            " Hexagon.Path.Element (Trav).B=" &
            Hexagon.Path.Element (Trav).B'Img);
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
"Didnt find path from 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used."
);
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
        ((True, 50, 100),
         (True, 51, 100),
         (True, 52, 100),
         (True, 53, 100));
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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area,
         Hexagon.Utility.Move,
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
        ((True, 5, 4),
         (True, 6, 3),
         (True, 7, 2),
         (True, 8, 1),
         (True, 9, 1));

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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area_Knight,
         Hexagon.Utility.Move,
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
         Text_IO.Put_Line
           ("A=" & Hexagon.Path.Element (Trav).A'Img & " B=" & Hexagon.Path.Element (Trav).B'Img);
         Trav := Hexagon.Path.Next (Trav);
      end loop;

      if Hexagon.Path.Length (Path) /= Answer'Length then
         Result := False;
      end if;

      AUnit.Assertions.Assert
        (Condition => Result,
         Message   =>
           "Didnt find path from 5, 4 to 9, 1, using Knight (from tutorial) reachable area");

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
        ((True, 5, 3),
         (True, 6, 3),
         (True, 7, 3),
         (True, 8, 3),
         (True, 8, 4));

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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area_Knight,
         Hexagon.Utility.Move,
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
           "Didnt find path from 5, 3 to 8, 4, using Knight (from tutorial) reachable area");

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
        ((True, 4, 6),
         (True, 4, 5),
         (True, 4, 4),
         (True, 4, 3),
         (True, 3, 3));

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

      Hexagon.Utility.Find_Accurate_Path
        (1,
         From_A,
         From_B,
         To_A,
         To_B,
         Test_Capability_Area_Knight,
         Hexagon.Utility.Move,
         Ret_Status,
         Path);

      Result := True;
      Trav   := Hexagon.Path.First (Path);
      while Hexagon.Path.Has_Element (Trav) loop
         Text_IO.Put_Line
           ("Path: A=" &
            Hexagon.Path.Element (Trav).A'Img &
            " B=" &
            Hexagon.Path.Element (Trav).B'Img);

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
           "Didnt find path from 4, 6 to 3, 3, using Knight (from tutorial) reachable area");

      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_to_4_6_to_3_3 - exit");
      end if;

   end Test_Find_Path_to_4_6_to_3_3;
   --
   --

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
"Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 patches in front that cant be used."
);

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Not_Blocked_By_Pieces'Access,
         Name    =>
           "Path Test find to 15, 15 to 13, 18 - passing through 3 patches with one piece on each");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Blocked_By_Pieces'Access,
         Name    =>
"Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 pieces in front"
);

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_to_15_15_to_13_18_Blocked_By_Enemy'Access,
         Name    =>
"Path Test find to 15, 15 to 13, 18 - has problem with reaching because there are 3 enemy pieces in front"
);

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
      --

   end Register_Tests;

end Tc_Hexagon_Utility;
