--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015  Frank J Jorgensen
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
with Piece;
with Text_IO;
with Utilities;
with Server.ServerRAPI;
with Status;

package body Server.Generic_ServerRCI is

   Verbose : constant Boolean := False;

   procedure Init
     (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Init - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Init - exit");
      end if;
   end Init;

   procedure Start is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Start - enter");
      end if;

      Server.ServerRAPI.Start;

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Start - exit");
      end if;
   end Start;

   procedure Stop is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Stop - enter");
      end if;

      Server.ServerRAPI.Stop;

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Type_Piece)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Create_Piece - enter");
      end if;

      Server.ServerRAPI.Create_Piece
        (P_Player_Id,
         P_Action_Type,
         P_Pos,
         P_Piece);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Create_Piece - exit");
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Put_Piece - enter");
      end if;

      Server.ServerRAPI.Put_Piece
        (P_Player_Id,
         P_Action_Type,
         P_Pos,
         P_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Put_Piece - exit ");
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Remove_Piece - enter");
      end if;

      Server.ServerRAPI.Remove_Piece (P_Player_Id, P_Action_Type, P_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Remove_Piece - exit");
      end if;

   end Remove_Piece;

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames
        .Vector)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Get_Pieces_Report - enter");
      end if;

      Server.ServerRAPI.Get_Pieces_Report (P_Player_Id, P_Visibility_Frames);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Perform_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Perform_Attack - enter");
      end if;

      Server.ServerRAPI.Perform_Attack
        (P_Player_Id,
         P_Action_Type,
         P_Attacking_Piece_Id,
         P_Attacked_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Ranged_Attack - enter");
      end if;

      Server.ServerRAPI.Perform_Ranged_Attack
        (P_Player_Id,
         P_Action_Type,
         P_Attacking_Piece_Id,
         P_Attacked_Piece_Id);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_To_Pos      : in Hexagon.Type_Hexagon_Position)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Move - enter P_Piece_Id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerRAPI.Perform_Move
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_To_Pos);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Patch_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerRAPI.Perform_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Patch_Effect - exit");
      end if;

   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Piece_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerRAPI.Perform_Piece_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Piece_Effect - exit");
      end if;

   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Grant_Piece_Effect- enter");
      end if;

      Server.ServerRAPI.Grant_Piece_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Revoke_Piece_Effect- enter");
      end if;

      Server.ServerRAPI.Revoke_Piece_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Grant_Patch_Effect- enter");
      end if;

      Server.ServerRAPI.Grant_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Revoke_Patch_Effect- enter");
      end if;

      Server.ServerRAPI.Revoke_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Perform_Construction
     (P_Player_Id        : in Player.Type_Player_Id;
      P_Action_Type      : in Action.Type_Action_Type;
      P_Piece_Id         : in Piece.Type_Piece_Id;
      P_Construction_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction     : in Construction.Type_Construction)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Construction - enter");
      end if;

      Server.ServerRAPI.Perform_Construction
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Construction_Pos,
         P_Construction);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Construction - exit");
      end if;
   end Perform_Construction;

   procedure Perform_Demolition
     (P_Player_Id      : in Player.Type_Player_Id;
      P_Action_Type    : in Action.Type_Action_Type;
      P_Piece_Id       : in Piece.Type_Piece_Id;
      P_Demolition_Pos : in Hexagon.Type_Hexagon_Position;
      P_Construction   : in Construction.Type_Construction)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Demolition - enter");
      end if;

      Server.ServerRAPI.Perform_Demolition
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Demolition_Pos,
         P_Construction);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Perform_Demolition - exit");
      end if;
   end Perform_Demolition;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Get_Map - enter");
      end if;

      Server.ServerRAPI.Get_Map (P_Server_Map);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Get_Map - exit");
      end if;
   end Get_Map;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Get_Server_Info - enter");
      end if;

      Server.ServerRAPI.Get_Server_Info (P_Server_Info, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Get_Server_Info - exit");
      end if;
   end Get_Server_Info;

   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Create_Game - enter");
      end if;

      Server.ServerRAPI.Create_Game
        (P_Create_File_Name,
         P_Player_Name_List,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Create_Game - exit");
      end if;
   end Create_Game;

   procedure Save_Game
     (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Save_Game - enter");
      end if;

      Server.ServerRAPI.Save_Game (P_Save_File_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Save_Game - exit");
      end if;
   end Save_Game;

   procedure Load_Game
     (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Load_Game - enter");
      end if;

      Server.ServerRAPI.Load_Game (P_Load_File_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Load_Game - exit");
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
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Join_Game - enter");
      end if;

      Server.ServerRAPI.Join_Game (P_Player_Name, P_Status, P_Player_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Join_Game - exit");
      end if;
   end Join_Game;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Leave_Game - enter");
      end if;

      Server.ServerRAPI.Leave_Game (P_Player_Id, P_Player_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Leave_Game - exit");
      end if;
   end Leave_Game;

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status)
      return Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Get_Player_Name - enter - exit");
      end if;

      Player_Name := Server.ServerRAPI.Get_Player_Name (P_Player_Id, P_Status);

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
           ("Server.Generic_ServerRCI.Get_Updates_Summary - enter P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerRAPI.Get_Updates_Summary
        (P_Player_Id,
         P_Countdown,
         P_Game_Status,
         P_System_Messages);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Get_Updates_Summary - exit");
      end if;
   end Get_Updates_Summary;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Generic_ServerRCI.Client_Stopped - enter Player_Id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerRAPI.Client_Stopped (P_Player_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.Generic_ServerRCI.Client_Stopped - exit");
      end if;

   end Client_Stopped;

end Server.Generic_ServerRCI;
