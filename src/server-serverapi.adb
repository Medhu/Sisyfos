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

with Hexagon.Server_Map;
with Text_IO;
with Server.Server.Piece_Action;
with Server.Server;
with Server.Server.Cmd;
with Attempt;

package body Server.ServerAPI is

   Verbose : constant Boolean := False;

   procedure Init (P_Fighting_Piece_Class, P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
   --
      P_Landscape_Info : in Landscape.Server.Type_Landscape_Type_Info_List;
      P_Piece_Info     : in Piece.Server.Fighting_Piece.Type_Piece_Type_Info_List;
      P_House_Info     : in Piece.Server.House_Piece.Type_House_Type_Info_List;
      P_Effect_Info    : in Effect.Server.Type_Effect_Type_Info_List;

      P_Game_Creating, P_Game_Saving, P_Game_Loading : in Server.Type_Game_Archive_Procedure;
      P_Game_Joining, P_Game_Leaving : in Server.Type_Game_Joining_Leaving_Procedure;
      P_Game_Start                                   : in Server.Type_Game_Start_Procedure;
      P_Game_Upkeep                                  : in Server.Type_Game_Upkeep_Procedure;
      P_Game_End                                     : in Server.Type_Game_End_Procedure)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Init - enter");
      end if;

      Server.Init
        (P_Fighting_Piece_Class, P_House_Piece_Class, P_Landscape_Info, P_Piece_Info, P_House_Info,
         P_Effect_Info, P_Game_Creating, P_Game_Saving, P_Game_Loading, P_Game_Joining,
         P_Game_Leaving, P_Game_Start, P_Game_Upkeep, P_Game_End);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Init - exit");
      end if;
   end Init;

   procedure Start is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Start - enter");
      end if;

      Server.Start;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Start - exit");
      end if;
   end Start;

   procedure Stop is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Stop - enter");
      end if;

      Server.Stop;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Stop - exit");
      end if;
   end Stop;

   procedure Run is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Run - enter");
      end if;

      Server.Run;

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Run - exit");
      end if;
   end Run;

   procedure Get_Server_Info (P_Server_Info : out Utilities.RemoteString_List.Vector) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Server_Info - enter");
      end if;

      Server.Get_Server_Info (P_Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Server_Info - exit");
      end if;
   end Get_Server_Info;

   procedure Set_Server_Info (P_Server_Info : in Utilities.RemoteString_List.Vector) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Set_Server_Info - enter");
      end if;

      Server.Set_Server_Info (P_Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Set_Server_Info - exit");
      end if;
   end Set_Server_Info;

   procedure Observe_Game_Minimum_Details (P_Minimum_Details : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game_Minimum_Details - enter");
      end if;

      Server.Observe_Game_Minimum_Details (P_Minimum_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game_Minimum_Details - exit");
      end if;
   end Observe_Game_Minimum_Details;

   procedure Observe_Game (P_Detail : in Positive) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game - enter");
      end if;

      Server.Observe_Game (P_Detail);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Observe_Game - exit");
      end if;
   end Observe_Game;

   procedure Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Piece.Type_Piece; P_Piece_Id : out Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status; P_Force : in Boolean := False)
   is
      A_Server_Piece : Piece.Server.Type_Piece_Access_Class;

      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Create_Piece - enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.New_Piece (P_Piece, A_Server_Piece);

      Server.Piece_Action.Init_Piece
        (P_Player_Id, P_Action_Type, P_Pos, A_Server_Piece, Attempt_Info, P_Force);

      P_Piece_Id := A_Server_Piece.all.Id;
      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Create_Piece - exit P_Status=" & P_Status'Img);
      end if;
   end Create_Piece;

   procedure Put_Piece (P_Player_Id : in Player.Type_Player_Id;
                        P_Action_Type : in Action.Type_Action_Type;
                        P_Pos : in Hexagon.Type_Hexagon_Position;
                        P_Piece_Id                    : in Piece.Type_Piece_Id;
                        P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Put_Piece - enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Put_Piece
        (P_Player_Id, P_Action_Type, P_Pos, P_Piece_Id, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Put_Piece - exit " & P_Status'Img);
      end if;
   end Put_Piece;

   procedure Remove_Piece (P_Player_Id : in Player.Type_Player_Id;
                           P_Action_Type : in Action.Type_Action_Type;
                           P_Pos : in Hexagon.Type_Hexagon_Position;
                           P_Piece_Id                       : in Piece.Type_Piece_Id;
                           P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Remove_Piece - enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Remove_Piece (P_Player_Id, P_Action_Type, P_Piece_Id, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Remove_Piece - exit");
      end if;
   end Remove_Piece;

   procedure Perform_Attack (P_Player_Id        : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Attack (from, to) - enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Perform_Attack
        (P_Player_Id, P_Action_Type, P_Attacking_Piece_Id, P_Attacked_Piece_Id, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Attack (from, to) - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Ranged_Attack - enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Perform_Ranged_Attack
        (P_Player_Id, P_Action_Type, P_Attacking_Piece_Id, P_Attacked_Piece_Id, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Ranged_Attack - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Move (from,to)- enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Perform_Move
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_To_Pos, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Move (from,to)- exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Move;

   procedure Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Status                                 :    out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Patch_Effect - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Perform_Patch_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name, P_Area, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Patch_Effect - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Pos : in     Hexagon.Type_Hexagon_Position; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Status                                 :    out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.ServerAPI.Perform_Piece_Effect - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Perform_Piece_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Perform_Piece_Effect - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect : in Effect.Type_Effect; P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Piece_Effect- enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Grant_Piece_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in Effect.Type_Effect_Name; P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Piece_Effect- enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Revoke_Piece_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Pos : in Hexagon.Type_Hexagon_Position; P_Effect : in Effect.Type_Effect;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A; P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Patch_Effect- enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Grant_Patch_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect, P_Area, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Area                                  : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name; P_Status : out Status.Type_Status)
   is
      Attempt_Info : Attempt.Type_Attempt_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Patch_Effect- enter");
      end if;

      Attempt_Info := Attempt.Initial_Attempt;

      Server.Piece_Action.Revoke_Patch_Effect
        (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name, P_Area, Attempt_Info);

      P_Status := Attempt.Get_Attempt_Status(Attempt_Info);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   function Is_Effect_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                            : in Piece.Type_Piece_Id;
      P_Effect_Name                         : in Effect.Type_Effect_Name) return Boolean
   is
      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Is_Effect_On_Piece- enter");
      end if;

      Ret := Server.Piece_Action.Is_Effect_On_Piece (P_Player_Id, P_Piece_Id, P_Effect_Name);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Is_Effect_On_Piece- exit");
      end if;

      return Ret;
   end Is_Effect_On_Piece;

   function Get_Effect_Aux_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                                 : in Piece.Type_Piece_Id;
      P_Effect_Name                              : in Effect.Type_Effect_Name) return Natural
   is
      Ret : Natural;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Effect_Aux_On_Piece- enter");
      end if;

      Ret := Server.Piece_Action.Get_Effect_Aux_On_Piece (P_Player_Id, P_Piece_Id, P_Effect_Name);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Effect_Aux_On_Piece- exit");
      end if;

      return Ret;
   end Get_Effect_Aux_On_Piece;

   function Find_Piece_In_List (P_Piece_Id : in Piece.Type_Piece_Id) return Type_Piece_Position is
      A_Piece_Position      : Piece.Server.Type_Piece_Position;
      An_API_Piece_Position : Type_Piece_Position;
   begin
      A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);

      An_API_Piece_Position.Actual_Piece := Piece.Type_Piece (A_Piece_Position.Actual_Piece.all);
      An_API_Piece_Position.Actual_Pos   := A_Piece_Position.Actual_Pos;

      return An_API_Piece_Position;
   end Find_Piece_In_List;

   procedure Opponents_Activity_Report_Append (P_Detail : Positive;
      P_Player_Id                                       : Player.Type_Player_Id;
      P_Activity_Description                            : Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Opponents_Activity_Report_Append - enter");
      end if;

      Server.Opponents_Activity_Report_Append (P_Detail, P_Player_Id, P_Activity_Description);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Opponents_Activity_Report_Append - exit");
      end if;
   end Opponents_Activity_Report_Append;

   procedure Player_Activity_Report_Append (P_Detail : Positive;
      P_Player_Id                                    : Player.Type_Player_Id;
      P_Activity_Description                         : Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Player_Activity_Report_Append - enter");
      end if;

      Server.Player_Activity_Report_Append (P_Detail, P_Player_Id, P_Activity_Description);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Player_Activity_Report_Append - exit");
      end if;
   end Player_Activity_Report_Append;

   procedure Opponents_System_Report_Append (P_Detail : Positive;
      P_Player_Id                                     : Player.Type_Player_Id;
      P_Activity_Description                          : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Opponents_System_Report_Append - enter");
      end if;

      Server.Opponents_System_Report_Append (P_Detail, P_Player_Id, P_Activity_Description);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Opponents_System_Report_Append - enter");
      end if;
   end Opponents_System_Report_Append;

   procedure Player_System_Report_Append (P_Detail : Positive; P_Player_Id : Player.Type_Player_Id;
      P_Activity_Description                       : Utilities.RemoteString.Type_String)
   is
      use Player;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Player_System_Report_Append - enter");
      end if;

      Server.Player_System_Report_Append (P_Detail, P_Player_Id, P_Activity_Description);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Player_System_Report_Append - enter");
      end if;
   end Player_System_Report_Append;

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id) return Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Player_Name - enter");
      end if;

      Player_Name := Server.Get_Player_Name (P_Player_Id);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Player_Name - exit ");
      end if;
      return Player_Name;
   end Get_Player_Name;

   function Is_Player_In_Scenario (P_Player_Id : in Player.Type_Player_Id) return Boolean is
      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Is_Player_In_Scenario - enter ");
      end if;

      Ret := Server.Is_Player_In_Scenario (P_Player_Id);
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Is_Player_In_Scenario - exit ");
      end if;

      return Ret;
   end Is_Player_In_Scenario;

   function Get_Map_Terrain
     (P_Pos : in Hexagon.Type_Hexagon_Position) return Landscape.Type_Landscape
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Map_Terrain - enter");
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Map_Terrain - exit");
      end if;
      return A_Patch.all.Landscape_Here;
   end Get_Map_Terrain;

   function Get_Map_Pieces_List
     (P_Pos : in Hexagon.Type_Hexagon_Position) return Landscape.Pieces_Here_List.Vector
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Map_Pieces_List - enter");
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if Verbose then
         Text_IO.Put_Line ("Server.ServerAPI.Get_Map_Pieces_List - exit");
      end if;
      return A_Patch.all.Pieces_Here;
   end Get_Map_Pieces_List;

end Server.ServerAPI;
