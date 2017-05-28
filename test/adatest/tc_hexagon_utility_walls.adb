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
with Construction;

package body Tc_Hexagon_Utility_Walls is
   Verbose : constant Boolean := False;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

   Test_Capability_Area,
   Test_Capability_Area2,
   Test_Capability_Area_Knight : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   type Type_Answer_Path is array (Positive range <>) of Hexagon.Type_Hexagon_Position;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line  : Utilities.RemoteString.Type_Command_Parameters;

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

      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User A"));
      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User B"));
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
      return Format ("Test functions in Hexagon-Utility on map with walls");
   end Name;

   procedure Test_Find_Path_Along_Axis_1_Wall_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 15, 14), (True, 16, 13), (True, 17, 12), (True, 18, 12));
      Result : Boolean;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1_Wall_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 12;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 14);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall5);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1_Wall_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_1_Wall_On_Entry;

   procedure Test_Find_Path_Along_Axis_1_Wall_On_Exit
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 16, 14), (True, 16, 13), (True, 17, 12), (True, 18, 12));
      Result : Boolean;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1_Wall_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 12;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 14); --17, 13
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall2);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_1_Wall_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_1_Wall_On_Exit;

   procedure Test_Find_Path_Along_Axis_2_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 15), (True, 14, 14), (True, 14, 13), (True, 15, 12));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 12;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 14);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall6);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_2_On_Entry;

   procedure Test_Find_Path_Along_Axis_2_On_Exit (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 15, 14), (True, 14, 14), (True, 14, 13), (True, 15, 12));
      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 12;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 14);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall3);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_2_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_2_On_Exit;

   procedure Test_Find_Path_Along_Axis_3_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 16), (True, 13, 16), (True, 12, 16), (True, 12, 15));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 15;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 15);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall1);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_3_On_Entry;

   procedure Test_Find_Path_Along_Axis_3_On_Exit (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 15), (True, 13, 16), (True, 12, 16), (True, 12, 15));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 15;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 15);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall4);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_3_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_3_On_Exit;

   procedure Test_Find_Path_Along_Axis_4_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 15), (True, 13, 16), (True, 12, 17), (True, 12, 18));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 18;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 16);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall2);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_4_On_Entry;

   procedure Test_Find_Path_Along_Axis_4_On_Exit (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 16), (True, 13, 16), (True, 12, 17), (True, 12, 18));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 12;
      To_B   := 18;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (14, 16);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall5);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_4_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_4_On_Exit;

   procedure Test_Find_Path_Along_Axis_5_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 14, 16), (True, 14, 17), (True, 14, 18), (True, 15, 18));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 18;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 16);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall3);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_5_On_Entry;

   procedure Test_Find_Path_Along_Axis_5_On_Exit (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 15, 16), (True, 14, 17), (True, 14, 18), (True, 15, 18));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 15;
      To_B   := 18;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (15, 16);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall6);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_5_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_5_On_Exit;

   procedure Test_Find_Path_Along_Axis_6_On_Entry
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 15, 16), (True, 16, 16), (True, 17, 16), (True, 18, 15));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6_On_Entry - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 15;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 15);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall4);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6_On_Entry - exit");
      end if;
   end Test_Find_Path_Along_Axis_6_On_Entry;

   procedure Test_Find_Path_Along_Axis_6_On_Exit (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      From_A, From_B : Hexagon.Type_Hexagon_Numbers;
      To_A, To_B     : Hexagon.Type_Hexagon_Numbers;
      Ret_Status     : Status.Type_Status;
      Path           : Hexagon.Path.Vector;
      Trav           : Hexagon.Path.Cursor;

      A_Wall_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Answer : Type_Answer_Path :=
        ((True, 15, 15), (True, 16, 15), (True, 16, 16), (True, 17, 16), (True, 18, 15));

      Result : Boolean;
      use Hexagon;
      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6_On_Exit - enter");
      end if;

      From_A := 15;
      From_B := 15;
      To_A   := 18;
      To_B   := 15;

      -- set a wall in the way
      A_Wall_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (16, 15);
      Construction.Construction_List.Clear (A_Wall_Patch.all.Constructions_Here);
      Construction.Construction_List.Include
        (A_Wall_Patch.all.Constructions_Here,
         Test_Piece.Construction_Wall1);

      Hexagon.Utility.Find_Path
        (1,
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
         Text_IO.Put_Line ("TC_hexagon_utility.Test_Find_Path_Along_Axis_6_On_Exit - exit");
      end if;
   end Test_Find_Path_Along_Axis_6_On_Exit;
   --

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin

      --
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_1_Wall_On_Entry'Access,
         Name => "Path Test find a straight path along axis 1, meeting a wall when entering patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_1_Wall_On_Exit'Access,
         Name => "Path Test find a straight path along axis 1, meeting a wall when exiting patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_2_On_Entry'Access,
         Name    =>
           "Path Test find a straight path along axis 2,  meeting a wall when entering patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_2_On_Exit'Access,
         Name    =>
           "Path Test find a straight path along axis 2,  meeting a wall when entering patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_3_On_Entry'Access,
         Name    => "Path Test find a straight path along axis 3");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_3_On_Exit'Access,
         Name    => "Path Test find a straight path along axis 3");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_4_On_Entry'Access,
         Name    => "Path Test find a straight path along axis 4");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_4_On_Exit'Access,
         Name    => "Path Test find a straight path along axis 4");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_5_On_Entry'Access,
         Name    => "Path Test find a straight path along axis 5");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_5_On_Exit'Access,
         Name    => "Path Test find a straight path along axis 5");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_6_On_Entry'Access,
         Name    => "Path Test find a straight path along axis 6");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Find_Path_Along_Axis_6_On_Exit'Access,
         Name    => "Path Test find a straight path along axis 6");
      --
      --

   end Register_Tests;

end Tc_Hexagon_Utility_Walls;
