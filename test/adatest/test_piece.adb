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

with Piece;
with Piece.Server;
with Piece.Server.House_Piece;
with Piece.Server.Fighting_Piece;
with Text_IO;
with Hexagon;
with Hexagon.Area;
with Hexagon.Area.Server_Area;
with Ada.Numerics.Discrete_Random;
with Server;
with Status;
with Effect.Server;
with Server.Server.Player_Action;

package body Test_Piece is
   package Random is new Ada.Numerics.Discrete_Random (Positive);
   RandomGen : Random.Generator;

   Verbose : constant Boolean := True;

   Current_Scenario : Utilities.RemoteString.Type_String;

   function Test_Action_Points
     (P_Type_Of_Piece : in Piece.Type_Piece_Type) return Integer
   is
      Ret : Integer;

      use Piece;
   begin
      if P_Type_Of_Piece = Test_Piece.Sentry_Piece then
         Ret := 4;
      elsif P_Type_Of_Piece = Test_Piece.Knight_Piece then
         Ret := 10;
      elsif P_Type_Of_Piece = Test_Piece.Bowman_Piece then
         Ret := 10;
      elsif P_Type_Of_Piece = Test_Piece.Ship_Piece then
         Ret := 10;
      elsif P_Type_Of_Piece = Test_Piece.Carrier_Piece then
         Ret := 10;
      elsif P_Type_Of_Piece = Test_Piece.Farm_House then
         Ret := 0;
      elsif P_Type_Of_Piece = Test_Piece.Lumberjack_House then
         Ret := 0;
      elsif P_Type_Of_Piece = Test_Piece.Stonecutter_House then
         Ret := 0;
      elsif P_Type_Of_Piece = Test_Piece.Tower_House then
         Ret := 5;
      end if;

      return Ret;
   end Test_Action_Points;

   procedure Init_Piece (P_Piece_Class : in out Type_My_Test_Piece) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Init_Piece (Piece) - enter");
      end if;

      P_Piece_Class.Action_Points :=
        Test_Piece.Test_Action_Points (P_Piece_Class.Type_Of_Piece);

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Init_Piece (Piece) - exit");
      end if;
   end Init_Piece;

   procedure Init_Piece (P_Piece_Class : in out Type_My_Test_House) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Init_Piece (House) - enter");
      end if;

      P_Piece_Class.Action_Points :=
        Test_Piece.Test_Action_Points (P_Piece_Class.Type_Of_Piece);

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Init_Piece (House) - exit");
      end if;
   end Init_Piece;

   function Create_Piece_Name
     (P_Piece : in Type_My_Test_House)
      return Utilities.RemoteString.Type_String
   is
      Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Name (House) - enter");
      end if;

      Name :=
        Utilities.RemoteString.To_Unbounded_String ("Default House Name");

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Name (House) - exit");
      end if;

      return Name;
   end Create_Piece_Name;

   function Create_Piece_Name
     (P_Piece : in Type_My_Test_Piece)
      return Utilities.RemoteString.Type_String
   is
      Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Name (Piece) - enter");
      end if;

      Name :=
        Utilities.RemoteString.To_Unbounded_String ("Default Piece Name");

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Name (Piece) - exit");
      end if;

      return Name;
   end Create_Piece_Name;

   function Create_Piece_Area
     (P_Piece : in Type_My_Test_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A := null;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Create_Piece_Area (Piece) - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Sentry_Piece or
        P_Piece.Type_Of_Piece = Test_Piece.Knight_Piece or
        P_Piece.Type_Of_Piece = Test_Piece.Ship_Piece or
        P_Piece.Type_Of_Piece = Test_Piece.Bowman_Piece
      then

         Ret :=
           new Hexagon.Area.Type_Action_Capabilities_A'
             (Hexagon.Type_Hexagon_Position'(True, 1, 1),
              Hexagon.Type_Hexagon_Position'(True, 1, 2),
              Hexagon.Type_Hexagon_Position'(True, 1, 3),
              Hexagon.Type_Hexagon_Position'(True, 1, 4),
              Hexagon.Type_Hexagon_Position'(True, 1, 5),
              Hexagon.Type_Hexagon_Position'(True, 1, 6),
              Hexagon.Type_Hexagon_Position'(True, 1, 7),
              Hexagon.Type_Hexagon_Position'(True, 2, 1),
              Hexagon.Type_Hexagon_Position'(True, 2, 2),
              Hexagon.Type_Hexagon_Position'(True, 2, 3),
              Hexagon.Type_Hexagon_Position'(True, 2, 4),
              Hexagon.Type_Hexagon_Position'(True, 2, 5),
              Hexagon.Type_Hexagon_Position'(True, 2, 6),
              Hexagon.Type_Hexagon_Position'(True, 2, 7),
              Hexagon.Type_Hexagon_Position'(True, 3, 1),
              Hexagon.Type_Hexagon_Position'(True, 3, 2),
              Hexagon.Type_Hexagon_Position'(True, 3, 3),
              Hexagon.Type_Hexagon_Position'(True, 3, 4),
              Hexagon.Type_Hexagon_Position'(True, 3, 5),
              Hexagon.Type_Hexagon_Position'(True, 3, 6),
              Hexagon.Type_Hexagon_Position'(True, 3, 7),
         --
              Hexagon.Type_Hexagon_Position'(True, 3, 28),
         --
              Hexagon.Type_Hexagon_Position'(True, 4, 1),
              Hexagon.Type_Hexagon_Position'(True, 4, 2),
              Hexagon.Type_Hexagon_Position'(True, 4, 3),
              Hexagon.Type_Hexagon_Position'(True, 4, 4),
              Hexagon.Type_Hexagon_Position'(True, 4, 5),
              Hexagon.Type_Hexagon_Position'(True, 4, 6),
              Hexagon.Type_Hexagon_Position'(True, 4, 7),
              Hexagon.Type_Hexagon_Position'(True, 4, 8),
              Hexagon.Type_Hexagon_Position'(True, 4, 9),
              Hexagon.Type_Hexagon_Position'(True, 4, 10),
         --
              Hexagon.Type_Hexagon_Position'(True, 4, 29),
              Hexagon.Type_Hexagon_Position'(True, 4, 30),
         --
              Hexagon.Type_Hexagon_Position'(True, 5, 1),
              Hexagon.Type_Hexagon_Position'(True, 5, 2),
              Hexagon.Type_Hexagon_Position'(True, 5, 3),
              Hexagon.Type_Hexagon_Position'(True, 5, 4),
              Hexagon.Type_Hexagon_Position'(True, 5, 5),
              Hexagon.Type_Hexagon_Position'(True, 5, 6),
              Hexagon.Type_Hexagon_Position'(True, 5, 7),
              Hexagon.Type_Hexagon_Position'(True, 5, 8),
         --
              Hexagon.Type_Hexagon_Position'(True, 5, 32),
         --
              Hexagon.Type_Hexagon_Position'(True, 6, 1),
              Hexagon.Type_Hexagon_Position'(True, 6, 2),
              Hexagon.Type_Hexagon_Position'(True, 6, 3),
              Hexagon.Type_Hexagon_Position'(True, 6, 4),
              Hexagon.Type_Hexagon_Position'(True, 6, 5),
              Hexagon.Type_Hexagon_Position'(True, 6, 6),
              Hexagon.Type_Hexagon_Position'(True, 6, 7),
              Hexagon.Type_Hexagon_Position'(True, 6, 8),
              Hexagon.Type_Hexagon_Position'(True, 7, 1),
              Hexagon.Type_Hexagon_Position'(True, 7, 2),
              Hexagon.Type_Hexagon_Position'(True, 7, 3),
              Hexagon.Type_Hexagon_Position'(True, 7, 4),
              Hexagon.Type_Hexagon_Position'(True, 7, 5),
              Hexagon.Type_Hexagon_Position'(True, 7, 6),
              Hexagon.Type_Hexagon_Position'(True, 7, 7),
              Hexagon.Type_Hexagon_Position'(True, 8, 9),
              Hexagon.Type_Hexagon_Position'(True, 9, 2),
         --
              Hexagon.Type_Hexagon_Position'(True, 9, 7),

         --
              Hexagon.Type_Hexagon_Position'(True, 10, 6),
              Hexagon.Type_Hexagon_Position'(True, 11, 11),
              Hexagon.Type_Hexagon_Position'(True, 12, 12),
         --
              Hexagon.Type_Hexagon_Position'(True, 14, 18),
              Hexagon.Type_Hexagon_Position'(True, 16, 16),
         --
              Hexagon.Type_Hexagon_Position'(True, 20, 10),
              Hexagon.Type_Hexagon_Position'(True, 20, 11),
              Hexagon.Type_Hexagon_Position'(True, 21, 10),
              Hexagon.Type_Hexagon_Position'(True, 21, 9),
              Hexagon.Type_Hexagon_Position'(True, 20, 9),
              Hexagon.Type_Hexagon_Position'(True, 19, 10),
              Hexagon.Type_Hexagon_Position'(True, 19, 11),
         --
              Hexagon.Type_Hexagon_Position'(True, 19, 12),
              Hexagon.Type_Hexagon_Position'(True, 21, 11),
         --
         --  (-1,+1)
         --  (+1,+0)
         --  (+0,-1)
         --  (+0,+1)

              Hexagon.Type_Hexagon_Position'(True, 30, 10),
              Hexagon.Type_Hexagon_Position'(True, 29, 11),
              Hexagon.Type_Hexagon_Position'(True, 29, 12),
              Hexagon.Type_Hexagon_Position'(True, 31, 10),
              Hexagon.Type_Hexagon_Position'(True, 31, 11),
              Hexagon.Type_Hexagon_Position'(True, 30, 9),
              Hexagon.Type_Hexagon_Position'(True, 30, 11),
         --
              Hexagon.Type_Hexagon_Position'(True, 38, 3),
              Hexagon.Type_Hexagon_Position'(True, 38, 4),

         --
              Hexagon.Type_Hexagon_Position'(True, 15, 15),
              Hexagon.Type_Hexagon_Position'(True, 15, 16),
              Hexagon.Type_Hexagon_Position'(True, 16, 16),
              Hexagon.Type_Hexagon_Position'(True, 14, 15),
              Hexagon.Type_Hexagon_Position'(True, 14, 16),
              Hexagon.Type_Hexagon_Position'(True, 41, 41),
              Hexagon.Type_Hexagon_Position'(True, 42, 42),
              Hexagon.Type_Hexagon_Position'(True, 42, 41),
              Hexagon.Type_Hexagon_Position'(True, 43, 23),
              Hexagon.Type_Hexagon_Position'(True, 43, 43),
              Hexagon.Type_Hexagon_Position'(True, 43, 42),
              Hexagon.Type_Hexagon_Position'(True, 44, 44),
              Hexagon.Type_Hexagon_Position'(True, 44, 39),
              Hexagon.Type_Hexagon_Position'(True, 44, 50),
              Hexagon.Type_Hexagon_Position'(True, 45, 50),
              Hexagon.Type_Hexagon_Position'(True, 45, 43),
              Hexagon.Type_Hexagon_Position'(True, 45, 49),
              Hexagon.Type_Hexagon_Position'(True, 46, 51),
              Hexagon.Type_Hexagon_Position'(True, 46, 38),
              Hexagon.Type_Hexagon_Position'(True, 65, 87),
              Hexagon.Type_Hexagon_Position'(True, 70, 70),
              Hexagon.Type_Hexagon_Position'(True, 100, 100));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Area -exit");
      end if;

      return Ret;
   end Create_Piece_Area;

   function Create_Piece_Area
     (P_Piece : in Type_My_Test_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A := null;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Area (House)- enter");
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Farm_House or
        P_Piece.Type_Of_Piece = Test_Piece.Tower_House or
        P_Piece.Type_Of_Piece = Test_Piece.Lumberjack_House or
        P_Piece.Type_Of_Piece = Test_Piece.Stonecutter_House
      then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities_A'
             (Hexagon.Type_Hexagon_Position'(True, 1, 1),
              Hexagon.Type_Hexagon_Position'(True, 1, 2),
              Hexagon.Type_Hexagon_Position'(True, 1, 3),
              Hexagon.Type_Hexagon_Position'(True, 1, 4),
              Hexagon.Type_Hexagon_Position'(True, 1, 5),
              Hexagon.Type_Hexagon_Position'(True, 1, 6),
              Hexagon.Type_Hexagon_Position'(True, 1, 7),
              Hexagon.Type_Hexagon_Position'(True, 2, 1),
              Hexagon.Type_Hexagon_Position'(True, 2, 2),
              Hexagon.Type_Hexagon_Position'(True, 2, 3),
              Hexagon.Type_Hexagon_Position'(True, 2, 4),
              Hexagon.Type_Hexagon_Position'(True, 2, 5),
              Hexagon.Type_Hexagon_Position'(True, 2, 6),
              Hexagon.Type_Hexagon_Position'(True, 2, 7),
              Hexagon.Type_Hexagon_Position'(True, 3, 1),
              Hexagon.Type_Hexagon_Position'(True, 3, 2),
              Hexagon.Type_Hexagon_Position'(True, 3, 3),
              Hexagon.Type_Hexagon_Position'(True, 3, 4),
              Hexagon.Type_Hexagon_Position'(True, 3, 5),
              Hexagon.Type_Hexagon_Position'(True, 3, 6),
              Hexagon.Type_Hexagon_Position'(True, 3, 7),
              Hexagon.Type_Hexagon_Position'(True, 4, 1),
              Hexagon.Type_Hexagon_Position'(True, 4, 2),
              Hexagon.Type_Hexagon_Position'(True, 4, 3),
              Hexagon.Type_Hexagon_Position'(True, 4, 4),
              Hexagon.Type_Hexagon_Position'(True, 4, 5),
              Hexagon.Type_Hexagon_Position'(True, 4, 6),
              Hexagon.Type_Hexagon_Position'(True, 4, 7),
              Hexagon.Type_Hexagon_Position'(True, 4, 10),
              Hexagon.Type_Hexagon_Position'(True, 5, 1),
              Hexagon.Type_Hexagon_Position'(True, 5, 2),
              Hexagon.Type_Hexagon_Position'(True, 5, 3),
              Hexagon.Type_Hexagon_Position'(True, 5, 4),
              Hexagon.Type_Hexagon_Position'(True, 5, 5),
              Hexagon.Type_Hexagon_Position'(True, 5, 6),
              Hexagon.Type_Hexagon_Position'(True, 5, 7),
              Hexagon.Type_Hexagon_Position'(True, 5, 8),
         --
              Hexagon.Type_Hexagon_Position'(True, 5, 31),
         --
              Hexagon.Type_Hexagon_Position'(True, 6, 1),
              Hexagon.Type_Hexagon_Position'(True, 6, 2),
              Hexagon.Type_Hexagon_Position'(True, 6, 3),
              Hexagon.Type_Hexagon_Position'(True, 6, 4),
              Hexagon.Type_Hexagon_Position'(True, 6, 5),
              Hexagon.Type_Hexagon_Position'(True, 6, 6),
              Hexagon.Type_Hexagon_Position'(True, 6, 7),
              Hexagon.Type_Hexagon_Position'(True, 7, 1),
              Hexagon.Type_Hexagon_Position'(True, 7, 2),
              Hexagon.Type_Hexagon_Position'(True, 7, 3),
              Hexagon.Type_Hexagon_Position'(True, 7, 4),
              Hexagon.Type_Hexagon_Position'(True, 7, 5),
              Hexagon.Type_Hexagon_Position'(True, 7, 6),
              Hexagon.Type_Hexagon_Position'(True, 7, 7),
              Hexagon.Type_Hexagon_Position'(True, 8, 9),
              Hexagon.Type_Hexagon_Position'(True, 9, 2),
              Hexagon.Type_Hexagon_Position'(True, 9, 7),
         --
              Hexagon.Type_Hexagon_Position'(True, 10, 6),
         --
              Hexagon.Type_Hexagon_Position'(True, 39, 39),
              Hexagon.Type_Hexagon_Position'(True, 40, 40),
              Hexagon.Type_Hexagon_Position'(True, 41, 41),
              Hexagon.Type_Hexagon_Position'(True, 42, 42),
              Hexagon.Type_Hexagon_Position'(True, 43, 23),
              Hexagon.Type_Hexagon_Position'(True, 43, 43),
              Hexagon.Type_Hexagon_Position'(True, 44, 44),
              Hexagon.Type_Hexagon_Position'(True, 46, 40),
              Hexagon.Type_Hexagon_Position'(True, 65, 87),
              Hexagon.Type_Hexagon_Position'(True, 70, 70),
              Hexagon.Type_Hexagon_Position'(True, 100, 100));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Create_Piece_Area -exit");
      end if;
      return Ret;
   end Create_Piece_Area;

   procedure Spend_Resources_On_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_My_Test_Piece;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Success     :    out Boolean)
   is

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Spend_Resources_On_Piece (Piece)- enter P_Piece.Player_Id=" &
            P_Piece.Player_Id'Img &
            " Type_Of_Piece=" &
            P_Piece.Type_Of_Piece'Img);
      end if;

      P_Success := True;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Spend_Resources_On_Piece (Piece)- exit " &
            P_Success'Img);
      end if;

   end Spend_Resources_On_Piece;

   procedure Spend_Resources_On_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_My_Test_House;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Success     :    out Boolean)
   is

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Spend_Resources_On_Piece (House)- enter P_Piece.Player_Id=" &
            P_Piece.Player_Id'Img &
            " Type_Of_Piece=" &
            P_Piece.Type_Of_Piece'Img);
      end if;

      P_Success := True;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Spend_Resources_On_Piece (House)- exit " &
            P_Success'Img);
      end if;

   end Spend_Resources_On_Piece;

   function Movement_Capability
     (P_Piece : in Type_My_Test_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access := null;

      use Piece;
      use Hexagon.Area;
   begin

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Movement_Capability - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Sentry_Piece then

         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Knight_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Ship_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Movement_Capability -exit");
      end if;

      return Ret;
   end Movement_Capability;

   function Observation_Area
     (P_Piece : in Type_My_Test_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Observation_Area - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Sentry_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
         --
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Bowman_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
         --
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2));

      elsif P_Piece.Type_Of_Piece = Test_Piece.Knight_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Ship_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   function Attack_Capability
     (P_Piece : in Type_My_Test_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access := null;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Attack_Capability - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Sentry_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Bowman_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Knight_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'
                (True, 0, 2),--added 24.nov-2013
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Ship_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Attack_Capability - exit ");
      end if;

      return Ret;
   end Attack_Capability;

   function Construction_Capability
     (P_Piece : in Type_My_Test_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access := null;

      use Piece;
      use Hexagon.Area;
   begin

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Construction_Capability - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Tower_House then

         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 2));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Construction_Capability -exit");
      end if;

      return Ret;
   end Construction_Capability;

   procedure Calculate_Attack_Result
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_My_Test_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            :    out Player.Type_Player_Id)
   is

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Attack_Result - enter");
      end if;

      if P_Attacking_Piece.Id = 1 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 2 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 3 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 4 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 5 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 6 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 7 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 8 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 9 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 10 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 11 and P_Attacked_Piece.Id = 12 then
         P_Winner := Player.Undefined_Player_Id;
      elsif P_Attacking_Piece.Id = 12 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 13 then
         P_Winner := P_Attacking_Piece.Player_Id;

      elsif P_Attacking_Piece.Id = 25 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 39 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 40 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 41 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 42 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 44 and P_Attacked_Piece.Id = 45 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 44 and P_Attacked_Piece.Id = 46 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 47 and P_Attacked_Piece.Id = 46 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 56 and P_Attacked_Piece.Id = 57 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 58 and P_Attacked_Piece.Id = 59 then
         P_Winner := Player.Undefined_Player_Id;
      elsif P_Attacking_Piece.Id = 60 and P_Attacked_Piece.Id = 61 then
         P_Winner := P_Attacking_Piece.Player_Id;
      else
         Text_IO.Put_Line
           ("Battle not defined P_Attacking_Piece.Id=" &
            P_Attacking_Piece.Id'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Attack_Result - exit");
      end if;
   end Calculate_Attack_Result;

   function Calculate_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_My_Test_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Attack_Action_Points - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Attack_Action_Points - exit");
      end if;

      return Attack_Action_Points_Example;
   end Calculate_Attack_Action_Points;

   function Calculate_Ranged_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_My_Test_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Ranged_Attack_Action_Points - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Ranged_Attack_Action_Points - exit");
      end if;

      return Ranged_Attack_Action_Points_Example;
   end Calculate_Ranged_Attack_Action_Points;

   function Calculate_Move_Action_Points
     (P_Action_Type            : in Action.Type_Action_Type;
      P_Moving_Piece           : in Type_My_Test_Piece;
      P_From_Patch, P_To_Patch : in Landscape.Type_Patch;
      P_Player_Id              : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Move_Action_Points - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Move_Action_Points - exit");
      end if;

      return Move_Action_Points_Example;
   end Calculate_Move_Action_Points;

   function Calculate_Patch_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect_Action_Points(Piece) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect_Action_Points(Piece)- exit");
      end if;

      return Search_Action_Points_Example;
   end Calculate_Patch_Effect_Action_Points;

   function Calculate_Patch_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect_Action_Points(House) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect_Action_Points(House) - exit");
      end if;

      return Search_Action_Points_Example;
   end Calculate_Patch_Effect_Action_Points;

   function Calculate_Piece_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect_Action_Points(Piece) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect_Action_Points(Piece)- exit");
      end if;

      return Perform_Piece_Effect_Action_Points_Example;
   end Calculate_Piece_Effect_Action_Points;

   function Calculate_Piece_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect_Action_Points(House) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect_Action_Points(House) - exit");
      end if;

      return Perform_Piece_Effect_Action_Points_Example;
   end Calculate_Piece_Effect_Action_Points;

   function Calculate_Construction_Action_Points
     (P_Action_Type        : in Action.Type_Action_Type;
      P_Constructing_Piece : in Type_My_Test_House;
      P_Piece_Patch        : in Landscape.Type_Patch;
      P_Construction_Patch : in Landscape.Type_Patch;
      P_Construction       : in Construction.Type_Construction;
      P_Player_Id          : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Construction_Action_Points - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Construction_Action_Points - exit");
      end if;
      return Construction_Action_Point_Example;
   end Calculate_Construction_Action_Points;

   function Calculate_Demolition_Action_Points
     (P_Action_Type      : in Action.Type_Action_Type;
      P_Demolition_Piece : in Type_My_Test_House;
      P_Piece_Patch      : in Landscape.Type_Patch;
      P_Demolition_Patch : in Landscape.Type_Patch;
      P_Construction     : in Construction.Type_Construction;
      P_Player_Id        : in Player.Type_Player_Id) return Integer
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Demolition_Action_Points - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Demolition_Action_Points - exit");
      end if;
      return Demolition_Action_Point_Example;
   end Calculate_Demolition_Action_Points;

   procedure Calculate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect (Piece) - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Patch_Effect (Piece)- exit");
      end if;
   end Calculate_Patch_Effect;

   procedure Calculate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect       : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Patch_Effect (House) - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Patch_Effect (House)- exit");
      end if;
   end Calculate_Patch_Effect;

   procedure Calculate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect (Piece) - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Piece_Effect (Piece)- exit");
      end if;
   end Calculate_Piece_Effect;

   procedure Calculate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect       : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Calculate_Piece_Effect (House) - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Piece_Effect (House)- exit");
      end if;
   end Calculate_Piece_Effect;

   function Validate_Create_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Create_Piece (Piece)- enter - exit " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Create_Piece;

   function Validate_Create_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Create_Piece (House)- enter - exit " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Create_Piece;

   function Validate_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Put_Piece - enter - exit " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Put_Piece;

   function Validate_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Put_Piece - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Put_Piece;

   function Validate_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Remove_Piece - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Remove_Piece;

   function Validate_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Remove_Piece - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Remove_Piece;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Test_Piece.Type_My_Test_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Attack - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Attack;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Test_Piece.Type_My_Test_Piece;
      P_Path                              : in Hexagon.Path.Vector;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Attack - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Attack;

   function Validate_Perform_Ranged_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Test_Piece.Type_My_Test_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Ranged_Attack - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Ranged_Attack;

   function Validate_Perform_Move
     (P_Action_Type  : in Action.Type_Action_Type;
      P_Moving_Piece : in Test_Piece.Type_My_Test_Piece;
      P_Path         : in Hexagon.Path.Vector;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Move - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Move;

   function Validate_Perform_Move
     (P_Action_Type        : in Action.Type_Action_Type;
      P_Moving_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Move - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Move;

   function Validate_Perform_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Patch_Effect(Piece) - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Patch_Effect;

   function Validate_Perform_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Patch_Effect(House) - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Patch_Effect;

   function Validate_Perform_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Piece_Effect(Piece) - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Piece_Effect;

   function Validate_Perform_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Piece_Effect(House) - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Piece_Effect;

   function Validate_Grant_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Grant_Piece_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Grant_Piece_Effect;

   function Validate_Grant_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Grant_Piece_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Grant_Piece_Effect;

   function Validate_Revoke_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Revoke_Piece_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Revoke_Piece_Effect;

   function Validate_Revoke_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Revoke_Piece_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Revoke_Piece_Effect;

   function Validate_Grant_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Grant_Patch_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Grant_Patch_Effect;

   function Validate_Grant_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Grant_Patch_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Grant_Patch_Effect;

   function Validate_Revoke_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Revoke_Patch_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Revoke_Patch_Effect;

   function Validate_Revoke_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Patch       : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Revoke_Patch_Effect - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Revoke_Patch_Effect;

   function Validate_Perform_Construction
     (P_Action_Type        : in Action.Type_Action_Type;
      P_Constructing_Piece : in Test_Piece.Type_My_Test_House;
      P_Piece_Pos          : in Hexagon.Type_Hexagon_Position;
      P_Construction_Pos   : in Hexagon.Type_Hexagon_Position;
      P_Construction       : in Construction.Type_Construction;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Construction - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Construction;

   function Validate_Perform_Demolition
     (P_Action_Type      : in Action.Type_Action_Type;
      P_Demolition_Piece : in Test_Piece.Type_My_Test_House;
      P_Piece_Pos        : in Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos   : in Hexagon.Type_Hexagon_Position;
      P_Construction     : in Construction.Type_Construction;
      P_Current_Player_Id,
      P_Player_Id : in Player.Type_Player_Id)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Perform_Demolition - enter - exit");
      end if;

      return P_Current_Player_Id = P_Player_Id;
   end Validate_Perform_Demolition;

   procedure After_Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in out Test_Piece.Type_My_Test_Piece;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Create_Piece;

   procedure After_Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in out Test_Piece.Type_My_Test_House;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Create_Piece;

   procedure After_Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Grant_Piece_Effect;

   procedure After_Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_House;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Grant_Piece_Effect;

   procedure After_Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Revoke_Piece_Effect;

   procedure After_Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_House;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Revoke_Piece_Effect;

   procedure After_Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_Piece;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Grant_Patch_Effect;

   procedure After_Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_House;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Grant_Patch_Effect;

   procedure After_Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_Piece;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Revoke_Patch_Effect;

   procedure After_Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Test_Piece.Type_My_Test_House;
      P_Patch : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Revoke_Patch_Effect;

   procedure After_Perform_Attack
     (P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece         : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Winner                 : in     Player.Type_Player_Id;
      P_Player_Id              : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Attack;

   procedure After_Perform_Ranged_Attack
     (P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece         : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Winner                 : in     Player.Type_Player_Id;
      P_Player_Id              : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Ranged_Attack;

   procedure After_Perform_Move
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Move;

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end After_Put_Piece;

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end After_Put_Piece;

   procedure Before_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end Before_Remove_Piece;

   procedure Before_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end Before_Remove_Piece;

   procedure After_Perform_Patch_Effect
     (P_Action_Type   : in     Action.Type_Action_Type;
      P_Piece         : in out Test_Piece.Type_My_Test_Piece;
      P_Patch         : in     Landscape.Type_Patch;
      P_Effect_Name   : in     Effect.Type_Effect_Name;
      P_Player_Id     : in     Player.Type_Player_Id;
      P_Search_Result : in     Positive)
   is
   begin
      null;
   end After_Perform_Patch_Effect;

   procedure After_Perform_Construction
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Constructing_Piece : in out Test_Piece.Type_My_Test_House;
      P_Piece_Patch        : in     Landscape.Type_Patch;
      P_Construction_Patch : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction;
      P_Player_Id          : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Construction;

   procedure After_Perform_Demolition
     (P_Action_Type      : in     Action.Type_Action_Type;
      P_Demolition_Piece : in out Test_Piece.Type_My_Test_House;
      P_Piece_Patch      : in     Landscape.Type_Patch;
      P_Demolition_Patch : in     Landscape.Type_Patch;
      P_Construction     : in     Construction.Type_Construction;
      P_Player_Id        : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Demolition;

   procedure Upkeep
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece             : in out Type_My_Test_Piece)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - Piece - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - Piece - exit");
      end if;
   end Upkeep;

   function Observation_Area
     (P_Piece : in Type_My_Test_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Observation_Area - enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Farm_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Lumberjack_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Test_Piece.Tower_House then
         Ret :=
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

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   procedure Upkeep
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Type_My_Test_House)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - House - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - House - exit");
      end if;

   end Upkeep;

   procedure Test_Joining_Game
   is
   begin
      if Verbose then
         Text_IO.Put_Line("Test_Piece.Test_Joining_Game -enter - exit");
      end if;

   end Test_Joining_Game;

   procedure Test_Leaving_Game
   is
   begin
      if Verbose then
         Text_IO.Put_Line("Test_Piece.Test_Leaving_Game -enter - exit");
      end if;

   end Test_Leaving_Game;

   procedure Test_Creating_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Creating_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Creating_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Creating_Game;

   procedure Test_Saving_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Saving_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Saving_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Saving_Game;

   procedure Test_Loading_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Loading_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Loading_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Loading_Game;

   procedure Test_Start_Game is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;

      A_Pos1 : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 4, 4);
      A_Pos2 : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 8, 9);
      A_To_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 5, 5);
      A_From_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 6, 5);
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Game - enter");
      end if;
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 6);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));

      A_Piece.Type_Of_Piece := Test_Piece.Tower_House;
      A_Piece.Category      := Piece.House_Piece;
      A_Piece.Player_Id     := 1;

      Server.Server.Player_Action.Create_Piece
        (Action.Type_Action_Type (1),
         1,
         1,
         A_Pos1,
         A_Piece,
         A_Piece.Id,
         Ret_Status);

      Text_IO.Put_Line ("Piece_Id=" & A_Piece.Id'Img);

      A_Piece.Type_Of_Piece := Test_Piece.Tower_House;
      A_Piece.Category      := Piece.House_Piece;
      A_Piece.Player_Id     := 2;

      Server.Server.Player_Action.Create_Piece
        (Action.Type_Action_Type (1),
         2,
         2,
         A_Pos2,
         A_Piece,
         A_Piece.Id,
         Ret_Status);
      Text_IO.Put_Line ("Piece_Id=" & A_Piece.Id'Img);

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Category      := Piece.Fighting_Piece;
      A_Piece.Player_Id     := 2;

      Server.Server.Player_Action.Create_Piece
        (Action.Type_Action_Type (1),
         2,
         2,
         A_From_Pos,
         A_Piece,
         A_Piece.Id,
         Ret_Status);
      Text_IO.Put_Line ("Piece_Id=" & A_Piece.Id'Img);

      Server.Server.Observe_Game (5);

      Server.Server.Player_Action.Perform_Move
        (Action.Type_Action_Type (1),
         A_Piece.Id,
         A_From_Pos,
         A_To_Pos,
         2,
         2,
         Ret_Status);

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Game - exit");
      end if;
   end Test_Start_Game;

   procedure Test_Upkeep_Game is
      Trav_All_Pieces : Piece.Server.Pieces_Server_List.Cursor;
      Trav_Pieces     : Landscape.Pieces_Here_List.Cursor;
      A_Piece_To_Visit,
      A_Piece_Encountered : Piece.Server.Type_Piece_Access :=
        null;
      A_Patch    : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Pos      : Hexagon.Type_Hexagon_Position;
      Axis_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;

      use Piece;
      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Upkeep_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Upkeep_Game - exit");
      end if;

   end Test_Upkeep_Game;

   procedure Test_Start_Turn (P_Player_Id : in Player.Type_Player_Id) is
      Trav_All_Pieces  : Piece.Server.Pieces_Server_List.Cursor;
      A_Piece_To_Visit : Piece.Server.Type_Piece_Access := null;

      use Piece;
      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Turn - enter ");
      end if;

      Trav_All_Pieces :=
        Piece.Server.Pieces_Server_List.First
          (Piece.Server.All_Pieces_In_Game);

      while Piece.Server.Pieces_Server_List.Has_Element (Trav_All_Pieces) loop
         A_Piece_To_Visit :=
           Piece.Server.Type_Piece_Access
             (Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces)
                .Actual_Piece);
         A_Piece_To_Visit.all.Action_Points :=
           Test_Piece.Test_Action_Points (A_Piece_To_Visit.all.Type_Of_Piece);

         Trav_All_Pieces :=
           Piece.Server.Pieces_Server_List.Next (Trav_All_Pieces);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Turn - exit");
      end if;
   end Test_Start_Turn;

   procedure Test_End_Turn (P_Player_Id : in Player.Type_Player_Id) is
      Trav_All_Pieces  : Piece.Server.Pieces_Server_List.Cursor;
      A_Piece_To_Visit : Piece.Server.Type_Piece_Access := null;

      use Piece;
      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Turn - enter ");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Turn - exit");
      end if;

   end Test_End_Turn;

   procedure Test_End_Game (P_Game_Status : out Status.Type_Game_Status) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Game - enter");
      end if;
      P_Game_Status := Status.Playing;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Game - exit");
      end if;
   end Test_End_Game;

begin
   Random.Reset (RandomGen, 1);
end Test_Piece;
