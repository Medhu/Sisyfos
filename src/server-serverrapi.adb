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
with Hexagon;
with Piece;
with Hexagon.Area.Server_Area;
with Text_IO;
with Utilities;
with Server.Server;
with Status;

package body Server.ServerRAPI is

   Verbose : constant Boolean := True;

   procedure Init
     (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters)
   is
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
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Start - enter");
      end if;

      if Server.Game_Engine /= null then
         raise Game_Engine_Exists;
      else
         Server.Start;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Start - exit");
      end if;
   end Start;

   procedure Stop is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Stop - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Stop;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in     Piece.Type_Piece;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Piece - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Create_Piece
           (P_Action_Type,
            P_Pos,
            P_Piece,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Create_Piece - exit");
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Put_Piece - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Put_Piece
           (P_Action_Type,
            P_Pos,
            P_Piece_Id,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Put_Piece - exit " & P_Status'Img);
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Remove_Piece - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Remove_Piece
           (P_Action_Type,
            P_Pos,
            P_Piece_Id,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Remove_Piece - exit");
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
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Pieces_Report - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Get_Pieces_Report
           (P_Player_Id,
            P_Visibility_Frames);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Attack
           (P_Action_Type,
            P_Attacking_Piece_Id,
            P_Attacked_Piece_Id,
            P_Attacking_Pos,
            P_Attacked_Pos,
            P_Player_Id,
            P_Winner,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path                                    : in     Hexagon.Path.Vector;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack (path) - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Attack
           (P_Action_Type,
            P_Attacking_Piece_Id,
            P_Attacked_Piece_Id,
            P_Path,
            P_Player_Id,
            P_Winner,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Attack (path) - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Ranged_Attack - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Ranged_Attack
           (P_Action_Type,
            P_Attacking_Piece_Id,
            P_Attacked_Piece_Id,
            P_Attacking_Pos,
            P_Attacked_Pos,
            P_Player_Id,
            P_Winner,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece_Id    : in     Piece.Type_Piece_Id;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id          : in     Player.Type_Player_Id;
      P_Status             :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Move - enter piece.id=" &
            P_Moving_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Move
           (P_Action_Type,
            P_Moving_Piece_Id,
            P_From_Pos,
            P_To_Pos,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Move
     (P_Action_Type     : in     Action.Type_Action_Type;
      P_Moving_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path            : in     Hexagon.Path.Vector;
      P_Player_Id       : in     Player.Type_Player_Id;
      P_Status          :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Move (Path)- enter piece.id=" &
            P_Moving_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Move
           (P_Action_Type,
            P_Moving_Piece_Id,
            P_Path,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Move (Path)- exit");
      end if;

   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Patch_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Patch_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Pos,
            P_Effect,
            P_Area,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Patch_Effect - exit");
      end if;

   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Perform_Piece_Effect - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Piece_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Pos,
            P_Effect,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Piece_Effect - exit");
      end if;

   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Piece_Effect- enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Grant_Piece_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Effect,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Piece_Effect- enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Revoke_Piece_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Effect,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Patch_Effect- enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Grant_Patch_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Pos,
            P_Effect,
            P_Area,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Patch_Effect- enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Revoke_Patch_Effect
           (P_Action_Type,
            P_Piece_Id,
            P_Pos,
            P_Effect,
            P_Area,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Perform_Construction
     (P_Action_Type           : in     Action.Type_Action_Type;
      P_Construction_Piece_Id : in     Piece.Type_Piece_Id;
      P_Piece_Pos             : in     Hexagon.Type_Hexagon_Position;
      P_Construction_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Construction          : in     Construction.Type_Construction;
      P_Player_Id             : in     Player.Type_Player_Id;
      P_Status                :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Construction - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Construction
           (P_Action_Type,
            P_Construction_Piece_Id,
            P_Piece_Pos,
            P_Construction_Pos,
            P_Construction,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Construction - exit");
      end if;
   end Perform_Construction;

   procedure Perform_Demolition
     (P_Action_Type         : in     Action.Type_Action_Type;
      P_Demolition_Piece_Id : in     Piece.Type_Piece_Id;
      P_Piece_Pos           : in     Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Construction        : in     Construction.Type_Construction;
      P_Player_Id           : in     Player.Type_Player_Id;
      P_Status              :    out Status.Type_Status)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Demolition - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Perform_Demolition
           (P_Action_Type,
            P_Demolition_Piece_Id,
            P_Piece_Pos,
            P_Demolition_Pos,
            P_Construction,
            P_Player_Id,
            P_Status);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Perform_Demolition - exit");
      end if;
   end Perform_Demolition;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Map - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Get_Map (P_Server_Map);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Get_Server_Info (P_Server_Info, P_Status);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Set_Server_Info (P_Server_Info, P_Status);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Create_Game
           (P_Create_File_Name,
            P_Player_Name_List,
            P_Status);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Save_Game (P_Save_File_Name, P_Status);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Load_Game (P_Load_File_Name, P_Status);
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
         Text_IO.Put_Line ("Server.ServerRAPI.Join_Game - enter - exit");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Join_Game
           (P_Player_Id,
            P_Player_Name,
            P_Status);
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

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Leave_Game
           (P_Player_Id,
            P_Player_Name,
            P_Status);
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
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Player_Name - enter - exit");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Get_Player_Name
           (P_Player_Id,
            Player_Name,
            P_Status);
      end if;

      return Player_Name;
   end Get_Player_Name;

   procedure Get_Updates_Summary
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Current_Player_Id :    out Player.Type_Player_Id;
      P_Countdown         :    out Positive;
      P_Game_Status       :    out Status.Type_Game_Status;
      P_System_Messages   :    out Observation.Activity.Activity_Report.Vector)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Get_Updates_Summary - enter P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Get_Updates_Summary
           (P_Player_Id,
            P_Current_Player_Id,
            P_Countdown,
            P_Game_Status,
            P_System_Messages);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Get_Updates_Summary - exit");
      end if;
   end Get_Updates_Summary;

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean is
      Ret : Boolean;

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.End_Turn - enter Player_Id=" & P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_End_Turn (P_Player_Id, Ret);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.End_Turn - exit");
      end if;

      return Ret;
   end End_Turn;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Client_Stopped - enter Player_Id=" &
            P_Player_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Client_Stopped (P_Player_Id);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Client_Stopped - exit");
      end if;

   end Client_Stopped;

   -- Information providers.
   -- When client needs information these RPCs should be used
   function Observation_Area
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Observation_Area - enter - exit");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Observation_Area (P_Piece_Id, Tmp);
      end if;

      --        declare
      --           Ret : Hexagon.Area.Type_Action_Capabilities := (Tmp.all);
      --        begin
      --           -- Free Tmp!!
      --           if Verbose then
      --              Text_IO.Put_Line ("Game.Observation_Area - exit");
      --           end if;
      --           return Ret;
      --        end;
      return Tmp;

   end Observation_Area;

   function Movement_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area.Server_Area;
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerRAPI.Movement_Capability - enter piece_id=" &
            P_Piece_Id'Img);
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Movement_Capability (P_Piece_Id, Tmp);
      end if;

      return Tmp;

   end Movement_Capability;

   function Attack_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area.Server_Area;
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerRAPI.Attack_Capability - enter");
      end if;

      if Server.Game_Engine = null then
         raise Game_Engine_Doesnt_Exists;
      else
         Server.Game_Engine.Entry_Attack_Capability (P_Piece_Id, Tmp);
      end if;

      return Tmp;

   end Attack_Capability;

end Server.ServerRAPI;