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
with Ada.Real_Time;
with Hexagon;
with Hexagon.Area;
with Piece;
with Piece.Server;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Player;
with Utilities;
with Landscape;
with Status;
with Observation;
with Effect;
with Unchecked_Deallocation;
with Construction;
with Construction.Server;
with Effect.Server;
with Landscape.Server;
with Action;

package Server.Server is

   Time_Interval : constant Positive := 1500; -- milliseconds

   type Type_Game_Archive_Procedure is access procedure
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);
   type Type_Game_Start_Procedure is access procedure;
   type Type_Game_Upkeep_Procedure is access procedure;
   type Type_Game_Joining_Leaving_Procedure is access procedure;
   type Type_Game_End_Procedure is access procedure
     (P_Game_Status : out Status.Type_Game_Status);

   type Type_Player is record
      Player_Name  : Utilities.RemoteString.Type_String;
      Active       : Boolean := False;
      In_Scenario  : Boolean := False;
      Is_Observing : Boolean := False;

      Current_Player_Pieces_Observations  : Piece.Server.Type_Pieces_Report;
      Visibility_Frames : Observation.Frames.Piece_Visibility_Frames.Vector;
      Previous_Player_Pieces_Observations : Piece.Server.Type_Pieces_Report;
      Activity_Reports : Observation.Activity.Activity_Report.Vector;
      System_Messages : Observation.Activity.Activity_Report.Vector;
      Last_Update_Summary                 : Ada.Real_Time.Time;
      Last_System_Warning                 : Ada.Real_Time.Time;
      Game_State_Reported                 : Boolean := False;
      Last_Report                         : Boolean := False;
   end record;

   type Type_Player_List is array (Player.Type_Player_Id) of Type_Player;

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
   --
      P_Game_Creating,
      P_Game_Saving,
      P_Game_Loading                 : in Type_Game_Archive_Procedure;
      P_Game_Joining, P_Game_Leaving : in Type_Game_Joining_Leaving_Procedure;

      P_Game_Start  : in Type_Game_Start_Procedure;
      P_Game_Upkeep : in Type_Game_Upkeep_Procedure;
      P_Game_End    : in Type_Game_End_Procedure);

   Game_Creating, Game_Saving, Game_Loading : Type_Game_Archive_Procedure;

   Game_Joining, Game_Leaving : Type_Game_Joining_Leaving_Procedure;
   Game_Start                 : Type_Game_Start_Procedure;
   Game_Upkeep                : Type_Game_Upkeep_Procedure;
   Game_End                   : Type_Game_End_Procedure;

   task type Type_Game_Engine is

      entry Entry_Start;

      -- At the start of the game session the client needs to get the entire map (landscape)
      entry Entry_Get_Map (P_Server_Map : out Landscape.Type_Map);

      -- At the start of the game session the client needs to get information about who the players are,
      -- what map they are playing on and how much resources they have at the moment.
      entry Entry_Get_Server_Info
        (P_Server_Info : out Utilities.RemoteString_List.Vector;
         P_Status      : out Status.Type_Adm_Status);

      -- Client code can change Server_Info
      entry Entry_Set_Server_Info
        (P_Server_Info : in     Utilities.RemoteString_List.Vector;
         P_Status      :    out Status.Type_Adm_Status);

      -- The server creates a totally new game, based on the chose map and the players names.
      -- The server is currently supporting only two players :)
      entry Entry_Create_Game
        (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
         P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
         P_Status           :    out Status.Type_Adm_Status);

      -- The server saves the game in progress, using the filename given in the parameter.
      -- The file is saved on the server computer.
      entry Entry_Save_Game
        (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
         P_Status         :    out Status.Type_Adm_Status);

      -- The server can load a game that has been started previously and has been
      -- saved on the server. The players need to join the game over again themselves.
      entry Entry_Load_Game
        (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
         P_Status         :    out Status.Type_Adm_Status);

      -- When there is a game in progress in the server, then the
      -- players need to join the game.
      -- Both players needs to join the game, no matter if the game is
      -- created from start of loaded from a saved file.
      entry Entry_Join_Game
        (P_Player_Id   :    out Player.Type_Player_Id;
         P_Player_Name : in     Utilities.RemoteString.Type_String;
         P_Status      :    out Status.Type_Adm_Status);

      entry Entry_Leave_Game
        (P_Player_Id   : in     Player.Type_Player_Id;
         P_Player_Name : in     Utilities.RemoteString.Type_String;
         P_Status      :    out Status.Type_Adm_Status);

      entry Entry_Get_Player_Name
        (P_Player_Id   : in     Player.Type_Player_Id;
         P_Player_Name :    out Utilities.RemoteString.Type_String;
         P_Status      :    out Status.Type_Adm_Status);

      entry Entry_Get_Activity_Reports
        (P_Player_Id            : in     Player.Type_Player_Id;
         P_Activity_Report_List :    out Observation.Activity.Activity_Report
           .Vector);

      entry Entry_Get_Pieces_Report
        (P_Player_Id         : in     Player.Type_Player_Id;
         P_Visibility_Frames : out Observation.Frames.Piece_Visibility_Frames
           .Vector);

      entry Entry_Create_Piece
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Pos         : in Hexagon.Type_Hexagon_Position;
         P_Piece       : in Piece.Type_Piece);

      entry Entry_Put_Piece
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Pos         : in Hexagon.Type_Hexagon_Position;
         P_Piece_Id    : in Piece.Type_Piece_Id);

      entry Entry_Remove_Piece
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id);

      entry Entry_Perform_Attack
        (P_Player_Id                               : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id);

      entry Entry_Perform_Ranged_Attack
        (P_Player_Id                               : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id);

      entry Entry_Perform_Move
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_To_Pos      : in Hexagon.Type_Hexagon_Position);

      entry Entry_Perform_Patch_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect;
         P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

      entry Entry_Perform_Piece_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect);

      entry Entry_Grant_Piece_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect);

      entry Entry_Revoke_Piece_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect);

      entry Entry_Grant_Patch_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect;
         P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

      entry Entry_Revoke_Patch_Effect
        (P_Player_Id   : in Player.Type_Player_Id;
         P_Action_Type : in Action.Type_Action_Type;
         P_Piece_Id    : in Piece.Type_Piece_Id;
         P_Effect      : in Effect.Type_Effect;
         P_Area        : in Hexagon.Area.Type_Action_Capabilities_A);

      -- The player constructs something on patch on the map (e.g a wall)
      entry Entry_Perform_Construction
        (P_Player_Id        : in Player.Type_Player_Id;
         P_Action_Type      : in Action.Type_Action_Type;
         P_Piece_Id         : in Piece.Type_Piece_Id;
         P_Construction_Pos : in Hexagon.Type_Hexagon_Position;
         P_Construction     : in Construction.Type_Construction);

      -- The player demolish something on patch on the map (e.g a wall)
      entry Entry_Perform_Demolition
        (P_Player_Id      : in Player.Type_Player_Id;
         P_Action_Type    : in Action.Type_Action_Type;
         P_Piece_Id       : in Piece.Type_Piece_Id;
         P_Demolition_Pos : in Hexagon.Type_Hexagon_Position;
         P_Construction   : in Construction.Type_Construction);

      -- The client will call this frequently to get some system data (mostly)
      entry Entry_Get_Updates_Summary
        (P_Player_Id       : in     Player.Type_Player_Id;
         P_Countdown       :    out Positive;
         P_Game_State      :    out Status.Type_Game_Status;
         P_System_Messages : out Observation.Activity.Activity_Report.Vector);

      entry Entry_Client_Stopped (P_Player_Id : in Player.Type_Player_Id);

      entry Entry_Get_Game_Engine_State
        (P_Game_Engine_State : out Status.Type_Engine_State);

      entry Entry_Stop;
   end Type_Game_Engine;

   type Type_Game_Engine_Adress is access all Type_Game_Engine;
   Game_Engine : Type_Game_Engine_Adress := null;

   procedure Start;
   procedure Run;
   procedure Stop;

   function Get_Game_Engine_State return Status.Type_Engine_State;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector);

   procedure Set_Server_Info
     (P_Server_Info : in Utilities.RemoteString_List.Vector);

   procedure Observe_Game_Minimum_Details (P_Minimum_Details : in Positive);

   procedure Observe_Game (P_Detail : in Positive);

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id)
      return Utilities.RemoteString.Type_String;

   function Is_Player_In_Scenario
     (P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Opponents_Activity_Report_Append
     (P_Detail    : Positive;
      P_Player_Id : Player
        .Type_Player_Id; -- Opponents of this player should get this report.
      P_Activity_Description : Utilities.RemoteString.Type_String);

   procedure Player_Activity_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String);

   procedure Opponents_System_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String);

   procedure Player_System_Report_Append
     (P_Detail               : Positive;
      P_Player_Id            : Player.Type_Player_Id;
      P_Activity_Description : Utilities.RemoteString.Type_String);

   procedure Game_Engine_Free is new Unchecked_Deallocation
     (Type_Game_Engine,
      Type_Game_Engine_Adress);
end Server.Server;
