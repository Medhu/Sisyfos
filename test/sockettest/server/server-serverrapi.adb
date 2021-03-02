--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2021  Frank J Jorgensen
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
with Hexagon;
with Piece;
with Text_IO;
with Utilities;
with Status;

package body Server.ServerRAPI is

   Verbose : constant Boolean := False;

   procedure Init (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Init - enter");
      end if;

      Text_IO.Put_Line ("PolyORB Communication");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Init - exit");
      end if;
   end Init;

   procedure Start is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Start - enter");
      end if;

      Text_IO.Put_Line ("Start                                                   - OK");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Start - exit");
      end if;
   end Start;

   procedure Stop is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Stop - enter");
      end if;

      Text_IO.Put_Line ("Stop                                                    - OK");

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Type_Piece)
   is
      use Player;
      use Action;
      use Hexagon;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Piece - enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Create_Piece: Player_Id:" & P_Player_Id'Img & "expected 1");
         Text_IO.Put_Line ("Create_Piece                                            - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Create_Piece: P_Action_Type:" & P_Action_Type'Img & "expected 2");
         Text_IO.Put_Line ("Create_Piece                                            - Failed");
      elsif P_Pos /= Hexagon.Type_Hexagon_Position'(True, 3, 4) then
         Text_IO.Put_Line ("Create_Piece: P_Pos:" & P_Pos.A'Img & ", " & P_Pos.B'Img);
         Text_IO.Put_Line ("Create_Piece                                            - Failed");
      elsif P_Piece /=
        Piece.Type_Piece'
          (5,
           Piece.Type_Piece_Type (6),
           Piece.Fighting_Piece,
           Utilities.RemoteString.To_Unbounded_String ("Test"),
           7)
      then
         Text_IO.Put_Line ("Create_Piece: P_Piece:----");
         Text_IO.Put_Line ("Create_Piece                                            - Failed");
      else
         Text_IO.Put_Line ("Create_Piece                                            - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Piece - exit");
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
      use Player;
      use Action;
      use Hexagon;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Put_Piece - enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Put_Piece: Player_Id:" & P_Player_Id'Img & "expected 1");
         Text_IO.Put_Line ("Put_Piece                                               - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Put_Piece: P_Action_Type:" & P_Action_Type'Img & "expected 2");
         Text_IO.Put_Line ("Put_Piece                                               - Failed");
      elsif P_Pos /= Hexagon.Type_Hexagon_Position'(True, 3, 4) then
         Text_IO.Put_Line ("Put_Piece: P_Pos:" & P_Pos.A'Img & ", " & P_Pos.B'Img);
         Text_IO.Put_Line ("Put_Piece                                               - Failed");
      else
         Text_IO.Put_Line ("Put_Piece                                               - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Put_Piece - exit ");
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
      use Player;
      use Action;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Remove_Piece - enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Remove_Piece: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Remove_Piece                                            - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Remove_Piece: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Remove_Piece                                            - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Remove_Piece: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Remove_Piece                                            - Failed");
      else
         Text_IO.Put_Line ("Remove_Piece                                            - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Remove_Piece - exit");
      end if;

   end Remove_Piece;

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames.Vector)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Pieces_Report - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Perform_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
      use Player;
      use Action;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack - enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Perform_Attack: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Perform_Attack: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Attacking_Piece_Id /= 3 then
         Text_IO.Put_Line
           ("Perform_Attack: P_Piece_Id:" & P_Attacking_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Attacked_Piece_Id /= 4 then
         Text_IO.Put_Line ("Perform_Attack: P_Piece_Id:" & P_Attacked_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      else
         Text_IO.Put_Line ("Perform_Attack                                          - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
      use Player;
      use Action;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Ranged_Attack - enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Perform_Attack: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Perform_Attack: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Attacking_Piece_Id /= 3 then
         Text_IO.Put_Line
           ("Perform_Attack: P_Piece_Id:" & P_Attacking_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      elsif P_Attacked_Piece_Id /= 4 then
         Text_IO.Put_Line ("Perform_Attack: P_Piece_Id:" & P_Attacked_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Attack                                          - Failed");
      else
         Text_IO.Put_Line ("Perform_Attack                                          - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_To_Pos      : in Hexagon.Type_Hexagon_Position)
   is
      use Player;
      use Action;
      use Hexagon;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Move - enter Piece_Id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Perform_Move: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Perform_Move                                            - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line ("Perform_Move: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Perform_Move                                            - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Perform_Move: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Move                                            - Failed");
      elsif P_To_Pos /= Hexagon.Type_Hexagon_Position'(True, 4, 5) then
         Text_IO.Put_Line ("Perform_Move: P_Pos:-----");
         Text_IO.Put_Line ("Perform_Move                                            - Failed");
      else
         Text_IO.Put_Line ("Perform_Move                                            - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Player;
      use Action;
      use Hexagon;
      use Hexagon.Area;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Patch_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Perform_Patch_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Perform_Patch_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Perform_Patch_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - Failed");
      elsif P_Effect_Name /= 4 then
         Text_IO.Put_Line
           ("Perform_Patch_Effect: P_Effect_Name:" &
            P_Effect_Name'Img &
            " expected 4");
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - Failed");
      elsif P_Area /=
        Hexagon.Area.Type_Action_Capabilities_A'(1 => Hexagon.Type_Hexagon_Position'(True, 8, 3))
      then
         Text_IO.Put_Line ("Perform_Patch_Effect: P_Area:----");
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - Failed");
      else
         Text_IO.Put_Line ("Perform_Patch_Effect                                    - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Patch_Effect - exit");
      end if;

   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name)
   is
      use Player;
      use Action;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Piece_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Perform_Piece_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Perform_Piece_Effect                                    - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Perform_Piece_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Perform_Piece_Effect                                    - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Perform_Piece_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Perform_Piece_Effect                                    - Failed");
      elsif P_Effect_Name /= 4 then
         Text_IO.Put_Line
           ("Perform_Piece_Effect: P_Effect_Name:" &
            P_Effect_Name'Img &
            " expected 4");
         Text_IO.Put_Line ("Perform_Piece_Effect                                    - Failed");
      else
         Text_IO.Put_Line ("Perform_Piece_Effect                                    - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Piece_Effect - exit");
      end if;

   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
      use Player;
      use Action;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Piece_Effect- enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Grant_Piece_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Grant_Piece_Effect                                      - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Grant_Piece_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Grant_Piece_Effect                                      - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Grant_Piece_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Grant_Piece_Effect                                      - Failed");
      elsif P_Effect.Effect_Name /= 4 or P_Effect.Aux /= 5 then
         Text_IO.Put_Line
           ("Grant_Piece_Effect: P_Effect:" &
            P_Effect.Effect_Name'Img &
            " expected 4  Aux:" &
            P_Effect.Aux'Img);
         Text_IO.Put_Line ("Grant_Piece_Effect                                      - Failed");
      else
         Text_IO.Put_Line ("Grant_Piece_Effect                                      - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name)
   is
      use Player;
      use Action;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Piece_Effect- enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Revoke_Piece_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Revoke_Piece_Effect                                     - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Revoke_Piece_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Revoke_Piece_Effect                                     - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Revoke_Piece_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Revoke_Piece_Effect                                     - Failed");
      elsif P_Effect_Name /= 4 then
         Text_IO.Put_Line
           ("Revoke_Piece_Effect: P_Effect_Name:" &
            P_Effect_Name'Img &
            " expected 4");
         Text_IO.Put_Line ("Revoke_Piece_Effect                                     - Failed");
      else
         Text_IO.Put_Line ("Revoke_Piece_Effect                                     - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Player;
      use Action;
      use Piece;
      use Effect;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Patch_Effect- enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Grant_Patch_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Grant_Patch_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Grant_Patch_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      elsif P_Effect.Effect_Name /= 4 or P_Effect.Aux /= 5 then
         Text_IO.Put_Line
           ("Revoke_Piece_Effect: P_Effect:" &
            P_Effect.Effect_Name'Img &
            " expected 4  Aux:" &
            P_Effect.Aux'Img);
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      elsif P_Area /=
        Hexagon.Area.Type_Action_Capabilities_A'(1 => Hexagon.Type_Hexagon_Position'(True, 8, 3))
      then
         Text_IO.Put_Line ("Grant_Patch_Effect: P_Area:----");
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      else
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Player;
      use Action;
      use Piece;
      use Effect;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Patch_Effect- enter");
      end if;

      if P_Player_Id /= 1 then
         Text_IO.Put_Line ("Revoke_Patch_Effect: Player_Id:" & P_Player_Id'Img & " expected 1");
         Text_IO.Put_Line ("Revoke_Patch_Effect                                     - Failed");
      elsif P_Action_Type /= 2 then
         Text_IO.Put_Line
           ("Revoke_Patch_Effect: P_Action_Type:" & P_Action_Type'Img & " expected 2");
         Text_IO.Put_Line ("Revoke_Patch_Effect                                     - Failed");
      elsif P_Piece_Id /= 3 then
         Text_IO.Put_Line ("Revoke_Patch_Effect: P_Piece_Id:" & P_Piece_Id'Img & " expected 3");
         Text_IO.Put_Line ("Revoke_Patch_Effect                                     - Failed");
      elsif P_Effect_Name /= 4 then
         Text_IO.Put_Line
           ("Revoke_Patch_Effect: P_Effect:" &
            P_Effect_Name'Img &
            " expected 4");
         Text_IO.Put_Line ("Revoke_Patch_Effect                                     - Failed");
      elsif P_Area /=
        Hexagon.Area.Type_Action_Capabilities_A'(1 => Hexagon.Type_Hexagon_Position'(True, 8, 3))
      then
         Text_IO.Put_Line ("Revoke_Patch_Effect: P_Area:----");
         Text_IO.Put_Line ("Grant_Patch_Effect                                      - Failed");
      else
         Text_IO.Put_Line ("Revoke_Patch_Effect                                     - OK");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Map - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Map - exit");
      end if;
   end Get_Map;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Server_Info - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Server_Info - exit");
      end if;
   end Get_Server_Info;

   procedure Set_Server_Info
     (P_Server_Info : in     Utilities.RemoteString_List.Vector;
      P_Status      :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Set_Server_Info - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Set_Server_Info - exit");
      end if;
   end Set_Server_Info;

   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Game - exit");
      end if;
   end Create_Game;

   procedure Save_Game
     (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Save_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Save_Game - exit");
      end if;
   end Save_Game;

   procedure Load_Game
     (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Load_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Load_Game - exit");
      end if;
   end Load_Game;

   procedure Join_Game
     (P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status;
      P_Player_Id   :    out Player.Type_Player_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Join_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Join_Game - exit");
      end if;
   end Join_Game;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Leave_Game - enter - exit");
      end if;

   end Leave_Game;

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status) return Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Player_Name - enter - exit");
      end if;

      return Player_Name;
   end Get_Player_Name;

   procedure Get_Updates_Summary
     (P_Player_Id       : in     Player.Type_Player_Id;
      P_Countdown       :    out Positive;
      P_Game_Status     :    out Status.Type_Game_Status;
      P_System_Messages :    out Observation.Activity.Activity_Report.Vector)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Get_Updates_Summary - enter P_Player_Id=" & P_Player_Id'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Updates_Summary - exit");
      end if;
   end Get_Updates_Summary;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Client_Stopped - enter Player_Id=" & P_Player_Id'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Client_Stopped - exit");
      end if;

   end Client_Stopped;

end Server.ServerRAPI;
