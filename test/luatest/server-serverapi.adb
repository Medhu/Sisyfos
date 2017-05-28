--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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
with Player;
with Hexagon.Area.Server_Area;

package body Server.ServerAPI is

   Verbose : constant Boolean := False;

   procedure Init
     (P_Fighting_Piece_Class,
      P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
   --
      P_Landscape_Info    : in Landscape.Server.Type_Landscape_Type_Info_List;
      P_Piece_Info : in Piece.Server.Fighting_Piece.Type_Piece_Type_Info_List;
      P_House_Info : in Piece.Server.House_Piece.Type_House_Type_Info_List;
      P_Construction_Info : in Construction.Server
        .Type_Construction_Type_Info_List;
      P_Effect_Info : in Effect.Server.Type_Effect_Type_Info_List;

      P_Game_Creating,
      P_Game_Saving,
      P_Game_Loading : in Server.Type_Game_Archive_Procedure;
      P_Game_Joining,
      P_Game_Leaving : in Server.Type_Game_Joining_Leaving_Procedure;
      P_Game_Start   : in Server.Type_Game_Start_Procedure;
      P_Game_Upkeep  : in Server.Type_Game_Upkeep_Procedure;
      P_Game_End     : in Server.Type_Game_End_Procedure)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Init - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Init - exit");
      end if;
   end Init;

   procedure Start is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Start - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Start - exit");
      end if;
   end Start;

   procedure Stop is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Stop - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Stop - exit");
      end if;
   end Stop;

   procedure Run is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Run - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Run - exit");
      end if;
   end Run;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Server_Info - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      P_Server_Info := Utilities.RemoteString_List.Empty_Vector;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Server_Info - exit");
      end if;
   end Get_Server_Info;

   procedure Set_Server_Info
     (P_Server_Info : in Utilities.RemoteString_List.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Set_Server_Info - enter");
      end if;

      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Set_Server_Info - exit");
      end if;
   end Set_Server_Info;

   procedure Observe_Game_Minimum_Details (P_Minimum_Details : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Observe_Game_Minimum_Details - enter");
      end if;

      if P_Minimum_Details /= 697 then
         Text_IO.Put_Line
           ("P_Minimum_Details=" & P_Minimum_Details'Img & " we expected 697");
      else
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Observe_Game_Minimum_Details (LUA) -    OK");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Observe_Game_Minimum_Details - exit");
      end if;
   end Observe_Game_Minimum_Details;

   procedure Observe_Game (P_Detail : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game - enter");
      end if;

      if P_Detail /= 879 then
         Text_IO.Put_Line ("P_Detail=" & P_Detail'Img & " we expected 879");
      else
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Observe_Game (LUA) -                    OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game - exit");
      end if;
   end Observe_Game;

   procedure Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in     Piece.Type_Piece;
      P_Piece_Id    :    out Piece.Type_Piece_Id;

      P_Status :    out Status.Type_Status;
      P_Force  : in     Boolean := False)
   is
      use Utilities.RemoteString;
      use Player;
      use Hexagon;
      use Piece;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Create_Piece - enter");
         Text_IO.Put_Line
           ("Create_Piece : " &
            " P_Action_Type=" &
            P_Action_Type'Img &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Piece=" &
            P_Piece.Id'Img &
            " " &
            P_Piece.Type_Of_Piece'Img &
            " " &
            P_Piece.Category'Img &
            " " &
            Utilities.RemoteString.To_String (P_Piece.Name) &
            " " &
            P_Piece.Player_Id'Img &
            " P_Pos.A=" &
            P_Pos.A'Img &
            " P_Pos.B=" &
            P_Pos.B'Img &
            " P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Create_Piece (LUA) - TEST");

      if P_Action_Type /= 900 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 900");
      elsif P_Player_Id /= 2 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 2");
      elsif not P_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Pos.P_Valid=" & P_Pos.P_Valid'Img & " we expected 'True'");
      elsif P_Pos.A /= 53 then
         Text_IO.Put_Line ("P_Pos.A=" & P_Pos.A'Img & " we expected '53'");
      elsif P_Pos.B /= 54 then
         Text_IO.Put_Line ("P_Pos.B=" & P_Pos.B'Img & " we expected '54'");
      elsif P_Piece.Id /= 1005 then
         Text_IO.Put_Line
           ("P_Piece.Id=" & P_Piece.Id'Img & " we expected '1005'");
      elsif P_Piece.Type_Of_Piece /= 1006 then
         Text_IO.Put_Line
           ("P_Piece.Type_Of_Piece=" &
            P_Piece.Type_Of_Piece'Img &
            " we expected '1006'");
      elsif P_Piece.Category /= Piece.Fighting_Piece then
         Text_IO.Put_Line
           ("P_Piece.Category=" &
            P_Piece.Category'Img &
            " we expected 'Fighting_Piece'");
      elsif P_Piece.Name /=
        Utilities.RemoteString.To_Unbounded_String ("TestName")
      then
         Text_IO.Put_Line
           ("P_Piece.Name=" &
            Utilities.RemoteString.To_String (P_Piece.Name) &
            " we expected 'TestName'");
      elsif P_Piece.Player_Id /= Player.Type_Player_Id (2) then
         Text_IO.Put_Line
           ("P_Piece.Player_Id=" & P_Piece.Player_Id'Img & " we expected '2'");
      elsif not P_Force then
         Text_IO.Put_Line ("P_Force=" & P_Force'Img & " we expected 'true'");
      else
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Create_Piece (LUA) -                    OK");
      end if;

      P_Piece_Id := Piece.Type_Piece_Id (999000);
      P_Status   := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Create_Piece - exit P_Status=" & P_Status'Img);
      end if;

   end Create_Piece;

   procedure Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Hexagon;
      use Piece;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Put_Piece - enter");
         Text_IO.Put_Line
           ("Put_Piece : " &
            " P_Action_Type=" &
            P_Action_Type'Img &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Pos.A=" &
            P_Pos.A'Img &
            " P_Pos.B=" &
            P_Pos.B'Img &
            " P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Put_Piece (LUA) - TEST");

      if P_Action_Type /= 901 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 901");
      elsif P_Player_Id /= 2 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 2");
      elsif not P_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Pos.P_Valid=" & P_Pos.P_Valid'Img & " we expected 'True'");
      elsif P_Pos.A /= 33 then
         Text_IO.Put_Line ("P_Pos.A=" & P_Pos.A'Img & " we expected '33'");
      elsif P_Pos.B /= 44 then
         Text_IO.Put_Line ("P_Pos.B=" & P_Pos.B'Img & " we expected '44'");
      elsif P_Piece_Id /= 55 then
         Text_IO.Put_Line
           ("P_Piece.Id=" & P_Piece_Id'Img & " we expected '55'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Put_Piece (LUA) -                                  OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Put_Piece - exit " & P_Status'Img);
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Hexagon;
      use Piece;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Remove_Piece - enter");

         Text_IO.Put_Line
           ("Remove_Piece : " &
            " P_Action_Type=" &
            P_Action_Type'Img &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Pos.A=" &
            P_Pos.A'Img &
            " P_Pos.B=" &
            P_Pos.B'Img &
            " P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Remove_Piece (LUA) - TEST");

      if P_Action_Type /= 902 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 902");
      elsif P_Player_Id /= 2 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 2");
      elsif not P_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Pos.P_Valid=" & P_Pos.P_Valid'Img & " we expected 'True'");
      elsif P_Pos.A /= 34 then
         Text_IO.Put_Line ("P_Pos.A=" & P_Pos.A'Img & " we expected '34'");
      elsif P_Pos.B /= 45 then
         Text_IO.Put_Line ("P_Pos.B=" & P_Pos.B'Img & " we expected '45'");
      elsif P_Piece_Id /= 56 then
         Text_IO.Put_Line
           ("P_Piece.Id=" & P_Piece_Id'Img & " we expected '56'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Remove_Piece (LUA) -                               OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Remove_Piece - exit");
      end if;
   end Remove_Piece;

   procedure Perform_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      use Player;
      use Hexagon;
      use Piece;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Attack (from, to) - enter");
         Text_IO.Put_Line
           ("Perform_Attack : " &
            " P_Action_Type=" &
            P_Action_Type'Img &

            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Attacking_Id=" &
            P_Attacking_Piece_Id'Img &
            " P_Attacked_Id=" &
            P_Attacked_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Attack (Pos) (LUA) - TEST");

      if P_Action_Type /= 903 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 903");
      elsif P_Player_Id /= 11 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 11");
      elsif P_Attacking_Piece_Id /= 81 then
         Text_IO.Put_Line
           ("P_Attacking_Piece_Id=" &
            P_Attacking_Piece_Id'Img &
            " we expected '81'");
      elsif P_Attacked_Piece_Id /= 72 then
         Text_IO.Put_Line
           ("P_Attacked_Piece_Id=" &
            P_Attacked_Piece_Id'Img &
            " we expected '72'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Attack (Pos) (LUA) -                       OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Attack (from, to) - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Attack;

   procedure Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;

      P_Status : out Status.Type_Status)
   is

      use Piece;
      use Action;
      use Hexagon;
      use Effect;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Patch_Effect - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Patch_Effect (LUA) - TEST");

      if P_Action_Type /= 920 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 920");
      elsif P_Piece_Id /= 33 then
         Text_IO.Put_Line ("P_Piece_Id=" & P_Piece_Id'Img & " we expected 33");
      elsif P_Effect.Effect_Name /= 4 or P_Effect.Aux /= 5 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected 4, P_Effect.Aux=" &
            P_Effect.Aux'Img &
            " we expected 5");
      elsif P_Area (1).A /= 1 or
        P_Area (1).B /= 2 or
        P_Area (2).A /= 3 or
        P_Area (2).B /= 4
      then
         Text_IO.Put_Line ("P_Area - not expected result");
      elsif P_Player_Id /= 78 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 78");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Patch_Effect (LUA) -                       OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Patch_Effect - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;

      P_Status : out Status.Type_Status)
   is

      use Piece;
      use Action;
      use Hexagon;
      use Effect;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Piece_Effect - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Piece_Effect (LUA) - TEST");

      if P_Action_Type /= 921 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 921");
      elsif P_Piece_Id /= 34 then
         Text_IO.Put_Line ("P_Piece_Id=" & P_Piece_Id'Img & " we expected 34");
      elsif P_Pos.A /= 89 or P_Pos.B /= 98 then
         Text_IO.Put_Line
           ("P_Pos.A=" &
            P_Pos.A'Img &
            " P_Pos.B=" &
            P_Pos.B'Img &
            " we expected 89, 98");
      elsif P_Effect.Effect_Name /= 5 or P_Effect.Aux /= 4 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected 5, P_Effect.Aux=" &
            P_Effect.Aux'Img &
            " we expected 4");
      elsif P_Player_Id /= 87 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 87");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Piece_Effect (LUA) -                       OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Piece_Effect - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Piece_Effect;

-- The from-to we received must be usable for this turn until it is "consumed"
-- otherwise we will return a failiure
   procedure Perform_Move
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_To_Pos      : in Hexagon.Type_Hexagon_Position;

      P_Status : out Status.Type_Status)
   is
      A_Moving_Piece : Piece.Server.Type_Piece_Access_Class := null;
      Move_Path      : Hexagon.Path.Vector;

      use Hexagon;
      use Player;
      use Piece;
      use Status;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Move (from,to)- enter P_Piece_Id=" &
            P_Piece_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      P_Status := Status.Ok;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Move (LUA) Pos - TEST");

      if P_Action_Type /= 905 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 905");
      elsif P_Player_Id /= 7 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 7");
      elsif not P_To_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_To_Pos.P_Valid=" &
            P_To_Pos.P_Valid'Img &
            " we expected 'True'");
      elsif P_To_Pos.A /= 44 then
         Text_IO.Put_Line
           ("P_To_Pos.A=" & P_To_Pos.A'Img & " we expected '44'");
      elsif P_To_Pos.B /= 54 then
         Text_IO.Put_Line
           ("P_To_Pos.B=" & P_To_Pos.B'Img & " we expected '54'");
      elsif P_Piece_Id /= 121 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '81'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Move (LUA) Pos -                           OK");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Move (from,to)- exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Move;

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      use Player;
      use Piece;
      use Hexagon;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Ranged_Attack - enter");
         Text_IO.Put_Line
           ("Perform_Ranged_Attack :" &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Attacking_Id=" &
            P_Attacking_Piece_Id'Img &
            " P_Attacked_Id=" &
            P_Attacked_Piece_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Ranged_Attack (LUA) - TEST");

      if P_Action_Type /= 907 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 907");
      elsif P_Player_Id /= 81 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 81");
      elsif P_Attacking_Piece_Id /= 11 then
         Text_IO.Put_Line
           ("P_Attacking_Piece_Id=" &
            P_Attacking_Piece_Id'Img &
            " we expected '11'");
      elsif P_Attacked_Piece_Id /= 22 then
         Text_IO.Put_Line
           ("P_Attacked_Piece_Id=" &
            P_Attacked_Piece_Id'Img &
            " we expected '22'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Ranged_Attack (LUA) -                      OK");
      end if;

      P_Status := Status.Ok;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Ranged_Attack - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Construction
     (P_Player_Id        : in Player.Type_Player_Id;
      P_Action_Type      : in Action.Type_Action_Type;
      P_Piece_Id         : in Piece.Type_Piece_Id;
      P_Piece_Pos        : in Hexagon.Type_Hexagon_Position;
      P_Construction_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction     : in Construction.Type_Construction;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Hexagon;
      use Piece;
      use Construction;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Construction - enter");
         Text_IO.Put_Line
           ("Perform_Construction : " &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Constructing_Piece_Id=" &
            P_Piece_Id'Img &
            " P_Piece_Pos.A=" &
            P_Piece_Pos.A'Img &
            " P_Piece_Pos.B=" &
            P_Piece_Pos.B'Img &
            " P_Construction_Pos.A=" &
            P_Construction_Pos.A'Img &
            " P_Construction_Pos.B=" &
            P_Construction_Pos.B'Img &
            " P_Construction=" &
            P_Construction'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Construction (LUA) - TEST");
      P_Status := Status.Ok;

      if P_Action_Type /= 908 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 908");
      elsif P_Player_Id /= 8 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 8");
      elsif not P_Piece_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Piece_Pos.P_Valid=" &
            P_Piece_Pos.P_Valid'Img &
            " we expected 'True'");
      elsif P_Piece_Pos.A /= 2 then
         Text_IO.Put_Line
           ("P_Piece_Pos.A=" & P_Piece_Pos.A'Img & " we expected '2'");
      elsif P_Piece_Pos.B /= 3 then
         Text_IO.Put_Line
           ("P_Piece_Pos.B=" & P_Piece_Pos.B'Img & " we expected '3'");
      elsif not P_Construction_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Construction_Pos.P_Valid=" &
            P_Construction_Pos.P_Valid'Img &
            " we expected 'True'");
      elsif P_Construction_Pos.A /= 4 then
         Text_IO.Put_Line
           ("P_Construction_Pos.A=" &
            P_Construction_Pos.A'Img &
            " we expected '4'");
      elsif P_Construction_Pos.B /= 5 then
         Text_IO.Put_Line
           ("P_Construction_Pos.B=" &
            P_Construction_Pos.B'Img &
            " we expected '5'");
      elsif P_Piece_Id /= 1 then
         Text_IO.Put_Line
           ("P_Constructing_Piece_Id=" & P_Piece_Id'Img & " we expected '1'");
      elsif P_Construction /= 6 then
         Text_IO.Put_Line
           ("P_Construction=" & P_Construction'Img & " we expected '22'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Construction (LUA) -                       OK");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Construction - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Construction;

   procedure Perform_Demolition
     (P_Player_Id      : in Player.Type_Player_Id;
      P_Action_Type    : in Action.Type_Action_Type;
      P_Piece_Id       : in Piece.Type_Piece_Id;
      P_Piece_Pos      : in Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction   : in Construction.Type_Construction;

      P_Status : out Status.Type_Status)

   is
      use Player;
      use Hexagon;
      use Piece;
      use Construction;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Demolition - enter");
         Text_IO.Put_Line
           ("Perform_Demolition : " &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " P_Constructing_Piece_Id=" &
            P_Piece_Id'Img &
            " P_Piece_Pos.A=" &
            P_Piece_Pos.A'Img &
            " P_Piece_Pos.B=" &
            P_Piece_Pos.B'Img &
            " P_Demolition_Pos.A=" &
            P_Demolition_Pos.A'Img &
            " P_Demolition_Pos.B=" &
            P_Demolition_Pos.B'Img &
            " P_Construction=" &
            P_Construction'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.Perform_Demolition (LUA) - TEST");

      P_Status := Status.Ok;

      if P_Action_Type /= 909 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 909");
      elsif P_Player_Id /= 88 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 88");
      elsif not P_Piece_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Piece_Pos.P_Valid=" &
            P_Piece_Pos.P_Valid'Img &
            " we expected 'True'");
      elsif P_Piece_Pos.A /= 22 then
         Text_IO.Put_Line
           ("P_Piece_Pos.A=" & P_Piece_Pos.A'Img & " we expected '22'");
      elsif P_Piece_Pos.B /= 33 then
         Text_IO.Put_Line
           ("P_Piece_Pos.B=" & P_Piece_Pos.B'Img & " we expected '33'");
      elsif not P_Demolition_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Demolition_Pos.P_Valid=" &
            P_Demolition_Pos.P_Valid'Img &
            " we expected 'True'");
      elsif P_Demolition_Pos.A /= 44 then
         Text_IO.Put_Line
           ("P_Demolition_Pos.A=" &
            P_Demolition_Pos.A'Img &
            " we expected '44'");
      elsif P_Demolition_Pos.B /= 55 then
         Text_IO.Put_Line
           ("P_Construction_Pos.B=" &
            P_Demolition_Pos.B'Img &
            " we expected '55'");
      elsif P_Piece_Id /= 11 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '11'");
      elsif P_Construction /= 66 then
         Text_IO.Put_Line
           ("P_Construction=" & P_Construction'Img & " we expected '66'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Demolition (LUA) -                         OK");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Demolition - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Demolition;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Piece;
      use Effect;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Piece_Effect- enter");
         Text_IO.Put_Line
           ("Grant_Piece_Effect : " &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " Piece_Id=" &
            P_Piece_Id'Img &
            " P_Effect=" &
            P_Effect.Effect_Name'Img &
            " " &
            P_Effect.Aux'Img);
      end if;

      P_Status := Status.Ok;
      Text_IO.Put_Line ("Server.ServerAPI.Grant_Piece_Effect (LUA) - TEST");

      if P_Action_Type /= 910 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 910");
      elsif P_Player_Id /= 2 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 2");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '11'");
      elsif P_Effect.Effect_Name /= 4 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected '4'");
      elsif P_Effect.Aux /= 5 then
         Text_IO.Put_Line
           ("P_Effect.Aux=" & P_Effect.Aux'Img & " we expected '5'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Grant_Piece_Effect (LUA) -                         OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Piece;
      use Effect;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Piece_Effect- enter");
         Text_IO.Put_Line
           ("Revoke_Piece_Effect :" &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " Piece_Id=" &
            P_Piece_Id'Img &
            " P_Effect=" &
            P_Effect.Effect_Name'Img &
            " " &
            P_Effect.Aux'Img);
      end if;

      P_Status := Status.Ok;

      Text_IO.Put_Line ("Server.ServerAPI.Revoke_Piece_Effect (LUA) - TEST");

      if P_Action_Type /= 911 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 911");
      elsif P_Player_Id /= 3 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 3");
      elsif P_Piece_Id /= 4 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '4'");
      elsif P_Effect.Effect_Name /= 5 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected '5'");
      elsif P_Effect.Aux /= 6 then
         Text_IO.Put_Line
           ("P_Effect.Aux=" & P_Effect.Aux'Img & " we expected '6'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Revoke_Piece_Effect (LUA) -                        OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;

      P_Status : out Status.Type_Status)
   is
      use Player;
      use Piece;
      use Effect;
      use Hexagon;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Patch_Effect- enter");

         Text_IO.Put_Line
           ("Grant_Patch_Effect : " &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " Piece_Id=" &
            P_Piece_Id'Img &
            " P_Pos.A=" &
            P_Pos.A'Img &
            " P_Pos.B=" &
            P_Pos.B'Img &
            " P_Effect=" &
            P_Effect.Effect_Name'Img &
            " " &
            P_Effect.Aux'Img &
            " P_Area'Length=" &
            P_Area'Length'Img);
      end if;

      P_Status := Status.Ok;

      Text_IO.Put_Line ("Server.ServerAPI.Grant_Patch_Effect (LUA) - TEST");

      if P_Action_Type /= 912 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 912");
      elsif P_Player_Id /= 12 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 12");
      elsif P_Piece_Id /= 13 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '13'");
      elsif not P_Pos.P_Valid then
         Text_IO.Put_Line
           ("P_Pos.P_Valid=" & P_Pos.P_Valid'Img & " excected 'true'");
      elsif P_Pos.A /= 14 then
         Text_IO.Put_Line ("P_Pos.A=" & P_Pos.A'Img & " excected '14'");
      elsif P_Pos.B /= 15 then
         Text_IO.Put_Line ("P_Pos.B=" & P_Pos.B'Img & " excected '15'");
      elsif P_Effect.Effect_Name /= 16 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected '16'");
      elsif P_Effect.Aux /= 17 then
         Text_IO.Put_Line
           ("P_Effect.Aux=" & P_Effect.Aux'Img & " we expected '17'");
      elsif not P_Area (1).P_Valid or
        P_Area (1).A /= 2 or
        P_Area (1).B /= 4 or
        not P_Area (2).P_Valid or
        P_Area (2).A /= 32 or
        P_Area (2).B /= 14 or
        not P_Area (3).P_Valid or
        P_Area (3).A /= 21 or
        P_Area (3).B /= 17 or
        not P_Area (4).P_Valid or
        P_Area (4).A /= 21 or
        P_Area (4).B /= 18
      then
         Text_IO.Put_Line ("P_Area not as expected");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Grant_Patch_Effect (LUA) -                         OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect;
      P_Status      :    out Status.Type_Status)
   is
      use Hexagon;
      use Player;
      use Piece;
      use Effect;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Patch_Effect- enter");

         Text_IO.Put_Line
           ("Revoke_Patch_Effect : " &
            " P_Player_Id=" &
            P_Player_Id'Img &
            " Piece_Id=" &
            P_Piece_Id'Img &
            " P_Effect=" &
            P_Effect.Effect_Name'Img &
            " " &
            P_Effect.Aux'Img);
      end if;

      P_Status := Status.Ok;

      Text_IO.Put_Line ("Server.ServerAPI.Revoke_Patch_Effect (LUA) - TEST");

      if P_Action_Type /= 913 then
         Text_IO.Put_Line
           ("P_Action_Type=" & P_Action_Type'Img & " we expected 913");
      elsif P_Player_Id /= 22 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 22");
      elsif P_Piece_Id /= 23 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '23'");
      elsif P_Effect.Effect_Name /= 26 then
         Text_IO.Put_Line
           ("P_Effect.Effect_Name=" &
            P_Effect.Effect_Name'Img &
            " we expected '26'");
      elsif P_Effect.Aux /= 27 then
         Text_IO.Put_Line
           ("P_Effect.Aux=" & P_Effect.Aux'Img & " we expected '27'");
      elsif not P_Area (1).P_Valid or
        P_Area (1).A /= 21 or
        P_Area (1).B /= 4 or
        not P_Area (2).P_Valid or
        P_Area (2).A /= 26 or
        P_Area (2).B /= 7 or
        not P_Area (3).P_Valid or
        P_Area (3).A /= 2 or
        P_Area (3).B /= 1 or
        not P_Area (4).P_Valid or
        P_Area (4).A /= 21 or
        P_Area (4).B /= 18 or
        not P_Area (5).P_Valid or
        P_Area (5).A /= 98 or
        P_Area (5).B /= 17
      then
         Text_IO.Put_Line ("P_Area not as expected");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Revoke_Patch_Effect (LUA) -                        OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean is
      Ret : Boolean;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.End_Turn - enter Player_Id=" & P_Player_Id'Img);
      end if;

      Text_IO.Put_Line ("Server.ServerAPI.End_Turn (LUA) - TEST");

      if P_Player_Id /= 98 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 101");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.End_Turn (LUA) -                                   OK");
      end if;

      Ret := False;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.End_Turn - exit");
      end if;

      return Ret;
   end End_Turn;

   function Observation_Area
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Type_Action_Capabilities
   is
      Ret : Hexagon.Area.Type_Action_Capabilities (1 .. 3) :=
        (1 =>
           (Hexagon.Area.Type_Hexagon_Delta_Position'
              (True,
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(6),
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(7))),

         2 =>
           (Hexagon.Area.Type_Hexagon_Delta_Position'
              (True,
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(8),
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(9))),

         3 =>
           (Hexagon.Area.Type_Hexagon_Delta_Position'
              (True,
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(5),
               Hexagon.Area.Type_Hexagon_Delta_Numbers'(4))));

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observation_Area - enter");
      end if;

      if P_Piece_Id /= 65 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '65'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Observation_Area (LUA) -                           OK");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   function Movement_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret_Movement_Capability : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      use Hexagon.Area.Server_Area;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Movement_Capability - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      if P_Piece_Id /= 67 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '67'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Movement_Capability (LUA) -                        OK");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Movement_Capability - exit");
      end if;

      Ret_Movement_Capability :=
        new Hexagon.Area.Type_Action_Capabilities'
          (1 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(1),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(2))),

           2 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(3),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(4))),

           3 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(5),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(6))));

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Movement_Capability - exit");
      end if;
      return Ret_Movement_Capability;
   end Movement_Capability;

   function Attack_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret_Attack_Capability : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;

      use Hexagon.Area.Server_Area;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Attack_Capability - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;

      if P_Piece_Id /= 68 then
         Text_IO.Put_Line
           ("P_Piece_Id=" & P_Piece_Id'Img & " we expected '68'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Movement_Capability (LUA) -                        OK");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Movement_Capability - exit");
      end if;

      Ret_Attack_Capability :=
        new Hexagon.Area.Type_Action_Capabilities'
          (1 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(3),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(4))),

           2 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(5),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(6))),

           3 =>
             (Hexagon.Area.Type_Hexagon_Delta_Position'
                (True,
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(7),
                 Hexagon.Area.Type_Hexagon_Delta_Numbers'(8))));

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Attack_Capability - exit");
      end if;
      return Ret_Attack_Capability;
   end Attack_Capability;

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id) return Type_Piece_Position
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Find_Piece_In_List - enter");
      end if;

      return Type_Piece_Position'
          (Piece.Type_Piece'
             (Piece.Type_Piece_Id (76),
              Piece.Type_Piece_Type (1),
              Piece.Type_Category (Piece.Fighting_Piece),
              Utilities.RemoteString.To_Unbounded_String ("Dummy Name"),
              Player.Type_Player_Id (1)),
           Hexagon.Type_Hexagon_Position'
             (True,
              Hexagon.Type_Hexagon_Numbers'(5),
              Hexagon.Type_Hexagon_Numbers'(7)));
   end Find_Piece_In_List;

   procedure Opponents_Activity_Report_Append
     (P_Detail    : Positive;
      P_Player_Id : Player
        .Type_Player_Id; -- Opponents of this player should get this report.
      P_Activity_Description : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Opponents_Activity_Report_Append - enter");
      end if;

      if P_Detail /= 10 then
         Text_IO.Put_Line ("P_Detail=" & P_Detail'Img & " we expected 10");
      elsif P_Player_Id /= 40 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 40");
      elsif P_Activity_Description /=
        Utilities.RemoteString.To_Unbounded_String
          ("Opponents_Activity_Report_Append")
      then
         Text_IO.Put_Line
           ("P_Activity_Description=" &
            Utilities.RemoteString.To_String (P_Activity_Description) &
            " we expected 'Opponents_Activity_Report_Append'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Opponents_Activity_Report_Append (LUA) -           OK");
      end if;

   end Opponents_Activity_Report_Append;

   procedure Player_Activity_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Player_Activity_Report_Append - enter");
      end if;

      if P_Detail /= 11 then
         Text_IO.Put_Line ("P_Detail=" & P_Detail'Img & " we expected 11");
      elsif P_Player_Id /= 41 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 41");
      elsif P_Activity_Description /=
        Utilities.RemoteString.To_Unbounded_String
          ("Player_Activity_Report_Append")
      then
         Text_IO.Put_Line
           ("P_Activity_Description=" &
            Utilities.RemoteString.To_String (P_Activity_Description) &
            " we expected 'Player_Activity_Report_Append'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Player_Activity_Report_Append (LUA) -              OK");
      end if;

   end Player_Activity_Report_Append;

   procedure Opponents_System_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Opponents_System_Report_Append - enter");
      end if;

      if P_Detail /= 12 then
         Text_IO.Put_Line ("P_Detail=" & P_Detail'Img & " we expected 12");
      elsif P_Player_Id /= 42 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 42");
      elsif P_Activity_Description /=
        Utilities.RemoteString.To_Unbounded_String
          ("Opponents_System_Report_Append")
      then
         Text_IO.Put_Line
           ("P_Activity_Description=" &
            Utilities.RemoteString.To_String (P_Activity_Description) &
            " we expected 'Opponents_System_Report_Append'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Opponents_System_Report_Append (LUA) -             OK");
      end if;

   end Opponents_System_Report_Append;

   procedure Player_System_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Player_System_Report_Append - enter");
      end if;

      if P_Detail /= 13 then
         Text_IO.Put_Line ("P_Detail=" & P_Detail'Img & " we expected 13");
      elsif P_Player_Id /= 43 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 43");
      elsif P_Activity_Description /=
        Utilities.RemoteString.To_Unbounded_String
          ("Player_System_Report_Append")
      then
         Text_IO.Put_Line
           ("P_Activity_Description=" &
            Utilities.RemoteString.To_String (P_Activity_Description) &
            " we expected 'Player_System_Report_Append'");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Player_System_Report_Append (LUA) -                OK");
      end if;

   end Player_System_Report_Append;

   function Is_Player_In_Scenario
     (P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Is_Player_In_Scenario - enter ");
      end if;

      Ret := True;
      Text_IO.Put_Line ("TEST NOT IMPLEMENTED");

      return Ret;
   end Is_Player_In_Scenario;

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id)
      return Utilities.RemoteString.Type_String
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Player_Name - enter");
      end if;

      if P_Player_Id /= 5 then
         Text_IO.Put_Line
           ("P_Player_Id=" & P_Player_Id'Img & " we expected 5");
      else
         Text_IO.Put_Line
           ("Server.ServerAPI.Get_Player_Name (LUA) -                            OK");
      end if;

      return Utilities.RemoteString.To_Unbounded_String ("The Player Name");

   end Get_Player_Name;

   function Get_Map_Terrain
     (P_Pos : in Hexagon.Type_Hexagon_Position) return Landscape.Type_Landscape
   is
   begin
      return Landscape.Type_Landscape (3);
   end Get_Map_Terrain;

   function Get_Map_Construction_List
     (P_Pos : in Hexagon.Type_Hexagon_Position)
      return Construction.Construction_List.Set
   is
      Test_Construction_List : Construction.Construction_List.Set;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Get_Map_Construction_List - enter");
      end if;

      Construction.Construction_List.Include
        (Test_Construction_List,
         Construction.Type_Construction (201));
      Construction.Construction_List.Include
        (Test_Construction_List,
         Construction.Type_Construction (202));
      Construction.Construction_List.Include
        (Test_Construction_List,
         Construction.Type_Construction (203));

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Get_Map_Construction_List - enter");
      end if;

      return Test_Construction_List;
   end Get_Map_Construction_List;

   function Get_Map_Pieces_List
     (P_Pos : in Hexagon.Type_Hexagon_Position)
      return Landscape.Pieces_Here_List.Vector
   is
      Test_Pieces_List : Landscape.Pieces_Here_List.Vector;

   begin

      Landscape.Pieces_Here_List.Append
        (Test_Pieces_List,
         Piece.Type_Piece_Id (92));
      Landscape.Pieces_Here_List.Append
        (Test_Pieces_List,
         Piece.Type_Piece_Id (23));
      Landscape.Pieces_Here_List.Append
        (Test_Pieces_List,
         Piece.Type_Piece_Id (42));

      return Test_Pieces_List;
   end Get_Map_Pieces_List;

end Server.ServerAPI;
