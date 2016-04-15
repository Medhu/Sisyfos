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
with Hexagon;
with Hexagon.Area;
with Piece;
with Player;
with Utilities;
with Landscape;
with Status;
with Observation;
with Effect;
with GNAT.Sockets;
with Hexagon.Area.Server_Area;
with Construction;
with Action;

generic
package Server.Generic_ServerRCI is

   procedure Init
     (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters);
   procedure Start (P_Channel : in GNAT.Sockets.Stream_Access);
   procedure Stop (P_Channel : in GNAT.Sockets.Stream_Access);

   procedure Get_Server_Info_In (P_Channel : in GNAT.Sockets.Stream_Access);
   procedure Get_Server_Info_Out
     (P_Channel     : in GNAT.Sockets.Stream_Access;
      P_Server_Info : in Utilities.RemoteString_List.Vector;
      P_Status      : in Status.Type_Adm_Status);

   procedure Create_Game_In
     (P_Channel          : in     GNAT.Sockets.Stream_Access;
      P_Create_File_Name :    out Utilities.RemoteString.Type_String;
      P_Player_Name_List :    out Utilities.RemoteString_List.Vector);
   procedure Create_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status);

   procedure Save_Game_In
     (P_Channel        : in     GNAT.Sockets.Stream_Access;
      P_Save_File_Name :    out Utilities.RemoteString.Type_String);
   procedure Save_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status);

   procedure Load_Game_In
     (P_Channel        : in     GNAT.Sockets.Stream_Access;
      P_Load_File_Name :    out Utilities.RemoteString.Type_String);
   procedure Load_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status);

   procedure Join_Game_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Name :    out Utilities.RemoteString.Type_String);
   procedure Join_Game_Out
     (P_Channel   : in GNAT.Sockets.Stream_Access;
      P_Player_Id : in Player.Type_Player_Id;
      P_Status    : in Status.Type_Adm_Status);

   procedure Leave_Game_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Player_Name :    out Utilities.RemoteString.Type_String);
   procedure Leave_Game_Out
     (P_Channel   : in GNAT.Sockets.Stream_Access;
      P_Status    : in Status.Type_Adm_Status);

   procedure Get_Player_Name_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id);
   procedure Get_Player_Name_Out
     (P_Channel     : in GNAT.Sockets.Stream_Access;
      P_Player_Name : in Utilities.RemoteString.Type_String;
      P_Status      : in Status.Type_Adm_Status);

   procedure Create_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Piece       :    out Piece.Type_Piece);

   procedure Create_Piece_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Put_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Piece_Id    :    out Piece.Type_Piece_Id);

   procedure Put_Piece_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Remove_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Piece_Id    :    out Piece.Type_Piece_Id);

   procedure Remove_Piece_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Get_Pieces_Report_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id);

   procedure Get_Pieces_Report_Out
     (P_Channel           : in GNAT.Sockets.Stream_Access;
      P_Visibility_Frames : in Observation.Frames.Piece_Visibility_Frames
        .Vector);

   procedure Perform_Attack_In
     (P_Channel : in     GNAT.Sockets.Stream_Access;
      P_Action_Type                             : out Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id :    out Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos :    out Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : out Player.Type_Player_Id);

   procedure Perform_Attack_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Winner  : in Player.Type_Player_Id;
      P_Status  : in Status.Type_Status);

   procedure Perform_Attack_In
     (P_Channel : in     GNAT.Sockets.Stream_Access;
      P_Action_Type                             : out Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id :    out Piece.Type_Piece_Id;
      P_Path                                    :    out Hexagon.Path.Vector;
      P_Player_Id                               : out Player.Type_Player_Id);

   procedure Perform_Ranged_Attack_In
     (P_Channel : in     GNAT.Sockets.Stream_Access;
      P_Action_Type                             : out Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id :    out Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos :    out Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : out Player.Type_Player_Id);

   procedure Perform_Move_In
     (P_Channel            : in     GNAT.Sockets.Stream_Access;
      P_Action_Type        :    out Action.Type_Action_Type;
      P_Moving_Piece_Id    :    out Piece.Type_Piece_Id;
      P_From_Pos, P_To_Pos :    out Hexagon.Type_Hexagon_Position;
      P_Player_Id          :    out Player.Type_Player_Id);

   procedure Perform_Move_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Perform_Move_In
     (P_Channel         : in     GNAT.Sockets.Stream_Access;
      P_Action_Type     :    out Action.Type_Action_Type;
      P_Moving_Piece_Id :    out Piece.Type_Piece_Id;
      P_Path            :    out Hexagon.Path.Vector;
      P_Player_Id       :    out Player.Type_Player_Id);

   procedure Perform_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
      P_Player_Id   :    out Player.Type_Player_Id);

   procedure Perform_Patch_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Perform_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Effect      :    out Effect.Type_Effect;
      P_Player_Id   :    out Player.Type_Player_Id);

   procedure Perform_Piece_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Perform_Construction_In
     (P_Channel               : in     GNAT.Sockets.Stream_Access;
      P_Action_Type           :    out Action.Type_Action_Type;
      P_Constructing_Piece_Id :    out Piece.Type_Piece_Id;
      P_Piece_Pos             :    out Hexagon.Type_Hexagon_Position;
      P_Construction_Pos      :    out Hexagon.Type_Hexagon_Position;
      P_Construction          :    out Construction.Type_Construction;
      P_Player_Id             :    out Player.Type_Player_Id);

   procedure Perform_Construction_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Grant_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect);

   procedure Grant_Piece_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Revoke_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect);

   procedure Revoke_Piece_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Grant_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A);

   procedure Grant_Patch_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Revoke_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A);

   procedure Revoke_Patch_Effect_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Status);

   procedure Get_Map_In (P_Channel : in GNAT.Sockets.Stream_Access);

   procedure Get_Map_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Map     : in Landscape.Type_Map);

   procedure Get_Updates_Summary_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id);

   procedure Get_Updates_Summary_Out
     (P_Channel           : in GNAT.Sockets.Stream_Access;
      P_Current_Player_Id : in Player.Type_Player_Id;
      P_Countdown         : in Positive;
      P_Game_Status       : in Status.Type_Game_Status;
      P_System_Messages   : in Observation.Activity.Activity_Report.Vector);

   procedure End_Turn_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id);
   procedure End_Turn_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Ret     : in Boolean);

   procedure Client_Stopped_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id);
   procedure Client_Stopped_Out (P_Channel : in GNAT.Sockets.Stream_Access);

   procedure Observation_Area_In
     (P_Channel  : in     GNAT.Sockets.Stream_Access;
      P_Piece_Id :    out Piece.Type_Piece_Id);
   procedure Observation_Area_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Area    : in Hexagon.Area.Server_Area.Type_Action_Capabilities_Access);

   procedure Movement_Capability_In
     (P_Channel  : in     GNAT.Sockets.Stream_Access;
      P_Piece_Id :    out Piece.Type_Piece_Id);
   procedure Movement_Capability_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Area    : in Hexagon.Area.Server_Area.Type_Action_Capabilities_Access);

   procedure Attack_Capability_In
     (P_Channel  : in     GNAT.Sockets.Stream_Access;
      P_Piece_Id :    out Piece.Type_Piece_Id);
   procedure Attack_Capability_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Area    : in Hexagon.Area.Server_Area.Type_Action_Capabilities_Access);

end Server.Generic_ServerRCI;
