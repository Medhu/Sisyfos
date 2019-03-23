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
with Piece;
with Piece.Server;
with Piece.Server.Fighting_Piece;
with Hexagon;
with Hexagon.Area.Server_Area;
with Hexagon.Client_Map;
with Text_IO;
with Test_ServerRCI;
with Player;
with Utilities;
with Ada.Strings.Unbounded;
with Piece.Client_Piece;
with Test_Piece;
with Server.ServerAPI;

with Hexagon.Server_Map;
with Status;
with Action;

package body TC_Piece_Server_Piece is

   Verbose                    : constant Boolean := False;
   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;
   Test_List                  : aliased Test_Piece.Type_Test_List;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Set_Up - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Set_Up - exit");
      end if;
   end Set_Up;

   ------------
   -- Set_Up Case--
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma Unreferenced (T);

      Player_Name_List : Utilities.RemoteString_List.Vector;
      Command_Line : Utilities.RemoteString.Type_Command_Parameters;

      Adm_Status : Status.Type_Adm_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Set_Up_Case - enter");
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

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Set_Up_Case - exit");
      end if;

   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Tear_Down - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Tear_Down - exit");
      end if;
   end Tear_Down;

   ---------------
   -- Tear_Down Case--
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Tear_Down_Case - enter");
      end if;

      Server.ServerAPI.Stop;

      if Verbose then
         Text_IO.Put_Line ("TC_Piece_Server_Piece.Tear_Down_Case - exit");
      end if;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test of server part of pieces logic");
   end Name;

   procedure Test_Observation_Area_Sentry (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Piece_Server : Test_Piece.Type_My_Test_Piece;
      Ret          : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_piece_server_piece.Test_Observation_Area_Sentry - enter");
      end if;

      Piece_Server.Id            := Piece.Undefined_Piece_Id;
      Piece_Server.Type_Of_Piece := Test_Piece.Sentry_Piece;

      Ret := Piece.Server.Observation_Area (Piece.Server.Type_Piece'Class (Piece_Server));

      AUnit.Assertions.Assert
        (Condition => Ret'Length = 13,
         Message   => "Sentry returned wrong observation area");

      if Verbose then
         Text_IO.Put_Line ("tc_piece_server_piece.Test_Observation_Area_Sentry - exit");
      end if;
   end Test_Observation_Area_Sentry;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Observation_Area_Sentry'Access,
         Name    => "Testing that a sentry returns correct observation area [TOO SIMPLE TEST]");

   end Register_Tests;

end TC_Piece_Server_Piece;
