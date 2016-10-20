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
with Status;
with GNAT.Sockets;
with Game_RPC;
with Server.ServerRAPI;
with Construction;
with Action;
with Ada.Task_Identification, Ada.Exceptions;

-- socket version
package body Server.Generic_ServerRCI is

   Address : GNAT.Sockets.Sock_Addr_Type;

   Verbose : constant Boolean := False;

   task type Type_Client_Channel is
      entry Start
        (P_No      : in Positive;
         P_Channel : in GNAT.Sockets.Stream_Access);
   end Type_Client_Channel;

   type Type_Client_Channel_Adress is access all Type_Client_Channel;

   task body Type_Client_Channel is
      Channel : GNAT.Sockets.Stream_Access;
      No      : Positive;

      RPC_Command : Game_RPC.Type_RPC;

      An_Action_Type : Action.Type_Action_Type;
      A_Player_Id    : Player.Type_Player_Id;
      A_Pos,
      An_Attacking_Pos,
      An_Attacked_Pos,
      A_From_Pos,
      A_To_Pos : Hexagon.Type_Hexagon_Position;
      A_Piece  : Piece.Type_Piece;
      A_Piece_Id,
      An_Attacking_Piece_Id,
      An_Attacked_Piece_Id : Piece.Type_Piece_Id;
      A_Player_Name        : Utilities.RemoteString.Type_String;

      An_Effect            : Effect.Type_Effect;
      A_Construction       : Construction.Type_Construction;

      A_Countdown         : Positive;
      A_Game_State        : Status.Type_Game_Status;
      The_System_Messages : Observation.Activity.Activity_Report.Vector;

      An_Area_A : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

      A_Map : Landscape.Type_Map;
      A_Create_File_Name,
      A_Save_File_Name,
      A_Load_File_Name   : Utilities.RemoteString.Type_String;
      A_Player_Name_List : Utilities.RemoteString_List.Vector;

      The_Server_Info : Utilities.RemoteString_List.Vector;

      Adm_Status            : Status.Type_Adm_Status;
      The_Visibility_Frames : Observation.Frames.Piece_Visibility_Frames
        .Vector;

      use Game_RPC;
   begin
      accept Start
        (P_No      : in Positive;
         P_Channel : in GNAT.Sockets.Stream_Access) do
         No      := P_No;
         Channel := P_Channel;

      end Start;

      while True loop
         RPC_Command := Game_RPC.Type_RPC'Input (Channel);

         if Verbose then
            Text_IO.Put_Line ("RPC_Command : " & RPC_Command'Img);
         end if;

         if RPC_Command = Game_RPC.Start_Start then
            Start (Channel);
            Server.ServerRAPI.Start;

         elsif RPC_Command = Game_RPC.Stop_Start then
            Stop (Channel);
            Server.ServerRAPI.Stop;

         elsif RPC_Command = Game_RPC.Create_Piece_Start then
            Create_Piece_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Pos,
               A_Piece);

            if Verbose then
               Text_IO.Put ("A_Player_Id=" & A_Player_Id'Img);
               if A_Pos.P_Valid then
                  Text_IO.Put
                    (" A_Pos A=" & A_Pos.A'Img & " B=" & A_Pos.B'Img);
               else
                  Text_IO.Put (" A_Pos invalid");
               end if;
               Text_IO.Put_Line
                 ("An_Action_Type " &
                  An_Action_Type'Img &
                  " A_Piece " &
                  A_Piece.Id'Img &
                  " " &
                  A_Piece.Type_Of_Piece'Img &
                  " " &
                  A_Piece.Category'Img &
                  " " &
                  A_Piece.Player_Id'Img);
            end if;

            Server.ServerRAPI.Create_Piece
              (A_Player_Id,
               An_Action_Type,
               A_Pos,
               A_Piece);

         elsif RPC_Command = Game_RPC.Put_Piece_Start then
            Put_Piece_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Pos,
               A_Piece_Id);

            if Verbose then
               Text_IO.Put
                 ("An_Action_Type " &
                  An_Action_Type'Img &
                  "A_Player_Id=" &
                  A_Player_Id'Img);
               if A_Pos.P_Valid then
                  Text_IO.Put
                    (" A_Pos A=" & A_Pos.A'Img & " B=" & A_Pos.B'Img);
               else
                  Text_IO.Put (" A_Pos invalid");
               end if;
               Text_IO.Put_Line (" A_Piece_Id " & A_Piece_Id'Img);
            end if;

            Server.ServerRAPI.Put_Piece
              (A_Player_Id,
               An_Action_Type,
               A_Pos,
               A_Piece_Id);

         elsif RPC_Command = Game_RPC.Remove_Piece_Start then
            Remove_Piece_In (Channel, A_Player_Id, An_Action_Type, A_Piece_Id);

            if Verbose then
               Text_IO.Put
                 ("A_Player_Id=" &
                  A_Player_Id'Img &
                  " An_Action_Type=" &
                  An_Action_Type'Img);

               Text_IO.Put_Line (" A_Piece_Id " & A_Piece_Id'Img);
            end if;

            Server.ServerRAPI.Remove_Piece
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id);

         elsif RPC_Command = Game_RPC.Get_Pieces_Report_Start then
            Get_Pieces_Report_In (Channel, A_Player_Id);
            if Verbose then
               Text_IO.Put ("A_Player_Id=" & A_Player_Id'Img);
            end if;

            Observation.Frames.Piece_Visibility_Frames.Clear
              (The_Visibility_Frames);
            Server.ServerRAPI.Get_Pieces_Report
              (A_Player_Id,
               The_Visibility_Frames);

            Get_Pieces_Report_Out (Channel, The_Visibility_Frames);

         elsif RPC_Command = Game_RPC.Perform_Attack_Start then
            Perform_Attack_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               An_Attacking_Piece_Id,
               An_Attacked_Piece_Id);

            Server.ServerRAPI.Perform_Attack
              (A_Player_Id,
               An_Action_Type,
               An_Attacking_Piece_Id,
               An_Attacked_Piece_Id);

         elsif RPC_Command = Game_RPC.Perform_Ranged_Attack_Start then
            Perform_Ranged_Attack_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               An_Attacking_Piece_Id,
               An_Attacked_Piece_Id);

            Server.ServerRAPI.Perform_Ranged_Attack
              (A_Player_Id,
               An_Action_Type,
               An_Attacking_Piece_Id,
               An_Attacked_Piece_Id);

         elsif RPC_Command = Game_RPC.Perform_Move_Start then
            Perform_Move_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_To_Pos);

            Server.ServerRAPI.Perform_Move
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_To_Pos);

         elsif RPC_Command = Game_RPC.Perform_Patch_Effect_Start then
            Perform_Patch_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A);

            Server.ServerRAPI.Perform_Patch_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A.all);

         elsif RPC_Command = Game_RPC.Perform_Piece_Effect_Start then
            Perform_Piece_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

            Server.ServerRAPI.Perform_Piece_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

         elsif RPC_Command = Game_RPC.Perform_Construction_Start then
            Perform_Construction_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_Pos,
               A_Construction);

            Server.ServerRAPI.Perform_Construction
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_Pos,
               A_Construction);

         elsif RPC_Command = Game_RPC.Perform_Demolition_Start then
            Perform_Demolition_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_Pos,
               A_Construction);

            Server.ServerRAPI.Perform_Demolition
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               A_Pos,
               A_Construction);

         elsif RPC_Command = Game_RPC.Grant_Piece_Effect_Start then
            Grant_Piece_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

            Server.ServerRAPI.Grant_Piece_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

         elsif RPC_Command = Game_RPC.Revoke_Piece_Effect_Start then
            Revoke_Piece_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

            Server.ServerRAPI.Revoke_Piece_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect);

         elsif RPC_Command = Game_RPC.Grant_Patch_Effect_Start then
            Grant_Patch_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A);

            Server.ServerRAPI.Grant_Patch_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A.all);
         -- TODO Free memory

         elsif RPC_Command = Game_RPC.Revoke_Patch_Effect_Start then
            Revoke_Patch_Effect_In
              (Channel,
               A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A);

            Server.ServerRAPI.Revoke_Patch_Effect
              (A_Player_Id,
               An_Action_Type,
               A_Piece_Id,
               An_Effect,
               An_Area_A.all);
         -- TODO Free memory

         elsif RPC_Command = Game_RPC.Get_Server_Info_Start then
            Get_Server_Info_In (Channel);

            Utilities.RemoteString_List.Clear (The_Server_Info);

            Server.ServerRAPI.Get_Server_Info (The_Server_Info, Adm_Status);
            Get_Server_Info_Out (Channel, The_Server_Info, Adm_Status);

         elsif RPC_Command = Game_RPC.Create_Game_Start then
            Create_Game_In (Channel, A_Create_File_Name, A_Player_Name_List);

            Server.ServerRAPI.Create_Game
              (A_Create_File_Name,
               A_Player_Name_List,
               Adm_Status);

            Create_Game_Out (Channel, Adm_Status);

         elsif RPC_Command = Game_RPC.Save_Game_Start then
            Save_Game_In (Channel, A_Save_File_Name);

            Server.ServerRAPI.Save_Game (A_Save_File_Name, Adm_Status);

            Save_Game_Out (Channel, Adm_Status);

         elsif RPC_Command = Game_RPC.Load_Game_Start then
            Load_Game_In (Channel, A_Load_File_Name);

            Server.ServerRAPI.Load_Game (A_Load_File_Name, Adm_Status);

            Load_Game_Out (Channel, Adm_Status);

         elsif RPC_Command = Game_RPC.Join_Game_Start then
            Join_Game_In (Channel, A_Player_Name);

            Server.ServerRAPI.Join_Game
              (A_Player_Name,
               Adm_Status,
               A_Player_Id);

            Join_Game_Out (Channel, A_Player_Id, Adm_Status);

         elsif RPC_Command = Game_RPC.Leave_Game_Start then
            Leave_Game_In (Channel, A_Player_Id, A_Player_Name);

            Server.ServerRAPI.Leave_Game
              (A_Player_Id,
               A_Player_Name,
               Adm_Status);

            Leave_Game_Out (Channel, Adm_Status);

         elsif RPC_Command = Game_RPC.Get_Player_Name_Start then
            Get_Player_Name_In (Channel, A_Player_Id);

            A_Player_Name :=
              Server.ServerRAPI.Get_Player_Name (A_Player_Id, Adm_Status);

            Get_Player_Name_Out (Channel, A_Player_Name, Adm_Status);

         elsif RPC_Command = Game_RPC.Get_Updates_Summary_Start then
            Get_Updates_Summary_In (Channel, A_Player_Id);

            Observation.Activity.Activity_Report.Clear (The_System_Messages);
            Server.ServerRAPI.Get_Updates_Summary
              (A_Player_Id,
               A_Countdown,
               A_Game_State,
               The_System_Messages);

            Get_Updates_Summary_Out
              (Channel,
               A_Countdown,
               A_Game_State,
               The_System_Messages);

         elsif RPC_Command = Game_RPC.Client_Stopped_Start then
            Client_Stopped_In (Channel, A_Player_Id);

            Server.ServerRAPI.Client_Stopped (A_Player_Id);

            Client_Stopped_Out (Channel);

         elsif RPC_Command = Game_RPC.Get_Map_Start then
            Get_Map_In (Channel);

            Server.ServerRAPI.Get_Map (A_Map);

            Get_Map_Out (Channel, A_Map);

         end if;

         RPC_Command := Game_RPC.Type_RPC'Input (Channel);

         if Verbose then
            Text_IO.Put_Line ("RPC_Command : " & RPC_Command'Img);
         end if;

      end loop;

   exception
      when E : others =>
         Text_IO.Put_Line ("Client Channel task died");
         Text_IO.Put_Line (Text_IO.Current_Error, Ada.Exceptions.Exception_Information (E));

   end Type_Client_Channel;

   task TCP_Server_Listener is
      entry Init (P_Address : in GNAT.Sockets.Sock_Addr_Type);
      entry Ready;
   end TCP_Server_Listener;

   task body TCP_Server_Listener is
      Server_Address : GNAT.Sockets.Sock_Addr_Type;
      Client_Address : GNAT.Sockets.Sock_Addr_Type;
      Client_Socket  : GNAT.Sockets.Socket_Type;
      Server_Socket  : GNAT.Sockets.Socket_Type;
      Channel        : GNAT.Sockets.Stream_Access;

      Client1, Client2 : Generic_ServerRCI.Type_Client_Channel_Adress;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.TCP_Server_Listener - starting");
      end if;

      accept Init (P_Address : in GNAT.Sockets.Sock_Addr_Type) do
         Server_Address := P_Address;
      end Init;
      Text_IO.Put_Line
        ("Attempts to create socket on " &
         GNAT.Sockets.Image (Server_Address));

      GNAT.Sockets.Create_Socket (Server_Socket);

      GNAT.Sockets.Bind_Socket (Server_Socket, Server_Address);

      GNAT.Sockets.Listen_Socket (Server_Socket);

      accept Ready;
      Text_IO.Put_Line ("Listening on " & GNAT.Sockets.Image (Server_Address));

      while Client1 = null or Client2 = null loop

         GNAT.Sockets.Accept_Socket
           (Server_Socket,
            Client_Socket,
            Client_Address);
         GNAT.Sockets.Set_Socket_Option
           (Client_Socket,
            GNAT.Sockets.IP_Protocol_For_TCP_Level,
            GNAT.Sockets.Option_Type'(GNAT.Sockets.No_Delay, True));
         Text_IO.Put_Line
           ("Client connected from " & GNAT.Sockets.Image (Client_Address));

         --  A client has been accepted, get the stream connected to the socket

         Channel := GNAT.Sockets.Stream (Client_Socket);

         if Client1 = null then
            Client1 := new Generic_ServerRCI.Type_Client_Channel;
            Client1.all.Start (1, Channel);
         --(Channel);
         else
            Client2 := new Generic_ServerRCI.Type_Client_Channel;
            Client2.all.Start (2, Channel);
         end if;
      end loop;

   end TCP_Server_Listener;

   procedure Init
     (P_Command_Line : in Utilities.RemoteString.Type_Command_Parameters)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Init - enter");
      end if;

      GNAT.Sockets.Initialize;   --  Initialize socket runtime

      Address.Addr :=
        GNAT.Sockets.Inet_Addr
          (Utilities.RemoteString.To_String (P_Command_Line (1)));

      Text_IO.Put_Line ("Server IP:" & GNAT.Sockets.Image (Address.Addr));

      Address.Port :=
        GNAT.Sockets.Port_Type
          (Integer'Value
             (Utilities.RemoteString.To_String (P_Command_Line (2))));

      Text_IO.Put_Line ("Socket Port:" & Address.Port'Img);

      TCP_Server_Listener.Init (Address);
      TCP_Server_Listener.Ready;

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Init - exit");
      end if;
   end Init;

   procedure Start (P_Channel : in GNAT.Sockets.Stream_Access) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Start - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Start - exit");
      end if;
   end Start;

   procedure Stop (P_Channel : in GNAT.Sockets.Stream_Access) is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Stop - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Piece       :    out Piece.Type_Piece)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Create_Piece_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Pos         := Hexagon.Type_Hexagon_Position'Input (P_Channel);
      P_Piece       := Piece.Type_Piece'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Create_Piece_In - exit");
      end if;

   end Create_Piece_In;

   -- Public procedures offered by Server
   procedure Put_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Pos         :    out Hexagon.Type_Hexagon_Position;
      P_Piece_Id    :    out Piece.Type_Piece_Id)
   is
      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Put_Piece_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Pos         := Hexagon.Type_Hexagon_Position'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Put_Piece_In - exit ");
      end if;
   end Put_Piece_In;

   procedure Remove_Piece_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Remove_Piece_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Remove_Piece_In - exit");
      end if;

   end Remove_Piece_In;

   procedure Get_Pieces_Report_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Pieces_Report_In - enter");
      end if;

      P_Player_Id := Player.Type_Player_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Pieces_Report_In - exit");
      end if;

   end Get_Pieces_Report_In;

   procedure Get_Pieces_Report_Out
     (P_Channel           : in GNAT.Sockets.Stream_Access;
      P_Visibility_Frames : in Observation.Frames.Piece_Visibility_Frames
        .Vector)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Pieces_Report_Out - enter");
      end if;

      Observation.Frames.Piece_Visibility_Frames.Vector'Output
        (P_Channel,
         P_Visibility_Frames);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Pieces_Report_Out - exit");
      end if;

   end Get_Pieces_Report_Out;

   procedure Perform_Attack_In
     (P_Channel : in     GNAT.Sockets.Stream_Access;
      P_Player_Id                               :    out Player.Type_Player_Id;
      P_Action_Type                             : out Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id :    out Piece.Type_Piece_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Attack_In - enter");
      end if;

      P_Player_Id          := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type        := Action.Type_Action_Type'Input (P_Channel);
      P_Attacking_Piece_Id := Piece.Type_Piece_Id'Input (P_Channel);
      P_Attacked_Piece_Id  := Piece.Type_Piece_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Attack_In - exit");
      end if;
   end Perform_Attack_In;

   procedure Perform_Ranged_Attack_In
     (P_Channel : in     GNAT.Sockets.Stream_Access;
      P_Player_Id                               :    out Player.Type_Player_Id;
      P_Action_Type                             : out Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id :    out Piece.Type_Piece_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Ranged_Attack_In - enter");
      end if;

      P_Player_Id          := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type        := Action.Type_Action_Type'Input (P_Channel);
      P_Attacking_Piece_Id := Piece.Type_Piece_Id'Input (P_Channel);
      P_Attacked_Piece_Id  := Piece.Type_Piece_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Ranged_Attack_In - exit");
      end if;
   end Perform_Ranged_Attack_In;

   procedure Perform_Move_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_To_Pos      :    out Hexagon.Type_Hexagon_Position)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Perform_Move_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_To_Pos      := Hexagon.Type_Hexagon_Position'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Perform_Move_In - exit");
      end if;

   end Perform_Move_In;

   procedure Perform_Move_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Path        :    out Hexagon.Path.Vector;
      P_Player_Id   :    out Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Move_In (Path)- enter");
      end if;

      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Path        := Hexagon.Path.Vector'Input (P_Channel);
      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Move_In (Path)- exit");
      end if;

   end Perform_Move_In;

   procedure Perform_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A)
   is

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Patch_Effect_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);
      P_Area        :=
        new Hexagon.Area.Type_Action_Capabilities_A'
          (Hexagon.Area.Type_Action_Capabilities_A'Input (P_Channel));

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Patch_Effect_In - exit");
      end if;

   end Perform_Patch_Effect_In;

   procedure Perform_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect)
   is

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Piece_Effect_In - enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Piece_Effect_In - exit");
      end if;

   end Perform_Piece_Effect_In;

   procedure Perform_Construction_In
     (P_Channel          : in     GNAT.Sockets.Stream_Access;
      P_Player_Id        :    out Player.Type_Player_Id;
      P_Action_Type      :    out Action.Type_Action_Type;
      P_Piece_Id         :    out Piece.Type_Piece_Id;
      P_Construction_Pos :    out Hexagon.Type_Hexagon_Position;
      P_Construction     :    out Construction.Type_Construction)
   is

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Construction_In - enter");
      end if;

      P_Player_Id        := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type      := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id         := Piece.Type_Piece_Id'Input (P_Channel);
      P_Construction_Pos := Hexagon.Type_Hexagon_Position'Input (P_Channel);
      P_Construction     := Construction.Type_Construction'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Construction_In - exit");
      end if;

   end Perform_Construction_In;

   procedure Perform_Demolition_In
     (P_Channel          : in     GNAT.Sockets.Stream_Access;
      P_Player_Id        :    out Player.Type_Player_Id;
      P_Action_Type      :    out Action.Type_Action_Type;
      P_Piece_Id         :    out Piece.Type_Piece_Id;
      P_Construction_Pos :    out Hexagon.Type_Hexagon_Position;
      P_Construction     :    out Construction.Type_Construction)
   is

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Demolition_In - enter");
      end if;

      P_Player_Id        := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type      := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id         := Piece.Type_Piece_Id'Input (P_Channel);
      P_Construction_Pos := Hexagon.Type_Hexagon_Position'Input (P_Channel);
      P_Construction     := Construction.Type_Construction'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Perform_Demolition_In - exit");
      end if;

   end Perform_Demolition_In;

   procedure Grant_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect)
   is
   begin

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Grant_Piece_Effect_In- enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Grant_Piece_Effect_In- exit");
      end if;
   end Grant_Piece_Effect_In;

   procedure Revoke_Piece_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Revoke_Piece_Effect_In- enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Revoke_Piece_Effect_In- exit");
      end if;
   end Revoke_Piece_Effect_In;

   procedure Grant_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Grant_Patch_Effect_In- enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);
      P_Area        :=
        new Hexagon.Area.Type_Action_Capabilities_A'
          (Hexagon.Area.Type_Action_Capabilities_A'Input (P_Channel));

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Grant_Patch_Effect_In- exit");
      end if;
   end Grant_Patch_Effect_In;

   procedure Revoke_Patch_Effect_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Action_Type :    out Action.Type_Action_Type;
      P_Piece_Id    :    out Piece.Type_Piece_Id;
      P_Effect      :    out Effect.Type_Effect;
      P_Area : out Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Revoke_Patch_Effect_In- enter");
      end if;

      P_Player_Id   := Player.Type_Player_Id'Input (P_Channel);
      P_Action_Type := Action.Type_Action_Type'Input (P_Channel);
      P_Piece_Id    := Piece.Type_Piece_Id'Input (P_Channel);
      P_Effect      := Effect.Type_Effect'Input (P_Channel);
      P_Area        :=
        new Hexagon.Area.Type_Action_Capabilities_A'
          (Hexagon.Area.Type_Action_Capabilities_A'Input (P_Channel));

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Revoke_Patch_Effect_In- exit");
      end if;
   end Revoke_Patch_Effect_In;

   procedure Get_Map_In (P_Channel : in GNAT.Sockets.Stream_Access) is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Get_Map_In - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Get_Map_In - exit");
      end if;
   end Get_Map_In;

   procedure Get_Map_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Map     : in Landscape.Type_Map)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Get_Map_Out - enter");
      end if;

      Landscape.Type_Map'Write (P_Channel, P_Map);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Get_Map_Out - exit");
      end if;
   end Get_Map_Out;

   procedure Get_Server_Info_In (P_Channel : in GNAT.Sockets.Stream_Access) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Server_Info_In - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Server_Info_In - exit");
      end if;
   end Get_Server_Info_In;

   procedure Get_Server_Info_Out
     (P_Channel     : in GNAT.Sockets.Stream_Access;
      P_Server_Info : in Utilities.RemoteString_List.Vector;
      P_Status      : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Server_Info_Out - enter - exit");
      end if;

      Utilities.RemoteString_List.Vector'Output (P_Channel, P_Server_Info);
      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Get_Server_Info_Out;

   procedure Create_Game_In
     (P_Channel          : in     GNAT.Sockets.Stream_Access;
      P_Create_File_Name :    out Utilities.RemoteString.Type_String;
      P_Player_Name_List :    out Utilities.RemoteString_List.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Create_Game_In - enter");
      end if;

      Utilities.RemoteString.Type_String'Read (P_Channel, P_Create_File_Name);
      Utilities.RemoteString_List.Vector'Read (P_Channel, P_Player_Name_List);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Create_Game_In - exit " &
            Utilities.RemoteString.To_String (P_Create_File_Name) &
            " " &
            Utilities.RemoteString_List.Length (P_Player_Name_List)'Img);
      end if;
   end Create_Game_In;

   procedure Create_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Create_Game_Out - enter - exit");
      end if;

      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Create_Game_Out;

   procedure Save_Game_In
     (P_Channel        : in     GNAT.Sockets.Stream_Access;
      P_Save_File_Name :    out Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Save_Game_In - enter");
      end if;

      Utilities.RemoteString.Type_String'Read (P_Channel, P_Save_File_Name);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Save_Game_In - exit");
      end if;
   end Save_Game_In;

   procedure Save_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Save_Game_Out - enter - exit");
      end if;

      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Save_Game_Out;

   procedure Load_Game_In
     (P_Channel        : in     GNAT.Sockets.Stream_Access;
      P_Load_File_Name :    out Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Load_Game_In - enter");
      end if;

      Utilities.RemoteString.Type_String'Read (P_Channel, P_Load_File_Name);

      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Load_Game_In - exit");
      end if;
   end Load_Game_In;

   procedure Load_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Load_Game_Out - enter - exit");
      end if;

      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Load_Game_Out;

   procedure Join_Game_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Name :    out Utilities.RemoteString.Type_String)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Join_Game_In - enter");
      end if;

      Utilities.RemoteString.Type_String'Read (P_Channel, P_Player_Name);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Join_Game_In - exit P_Player_Name=" &
            Utilities.RemoteString.To_String (P_Player_Name));
      end if;

   end Join_Game_In;

   procedure Join_Game_Out
     (P_Channel   : in GNAT.Sockets.Stream_Access;
      P_Player_Id : in Player.Type_Player_Id;
      P_Status    : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Join_Game_Out - enter - exit P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      Player.Type_Player_Id'Output (P_Channel, P_Player_Id);
      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Join_Game_Out;

   procedure Leave_Game_In
     (P_Channel     : in     GNAT.Sockets.Stream_Access;
      P_Player_Id   :    out Player.Type_Player_Id;
      P_Player_Name :    out Utilities.RemoteString.Type_String)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line ("Generic_Server.ServerRCI.Leave_Game_In - enter");
      end if;

      P_Player_Id := Player.Type_Player_Id'Input (P_Channel);
      Utilities.RemoteString.Type_String'Read (P_Channel, P_Player_Name);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Leave_Game_In - exit P_Player_Name=" &
            Utilities.RemoteString.To_String (P_Player_Name));
      end if;

   end Leave_Game_In;

   procedure Leave_Game_Out
     (P_Channel : in GNAT.Sockets.Stream_Access;
      P_Status  : in Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Leave_Game_Out - enter - exit");
      end if;

      Status.Type_Adm_Status'Output (P_Channel, P_Status);
   end Leave_Game_Out;

   procedure Get_Player_Name_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Player_Name_In - enter - exit");
      end if;

      P_Player_Id := Player.Type_Player_Id'Input (P_Channel);

   end Get_Player_Name_In;

   procedure Get_Player_Name_Out
     (P_Channel     : in GNAT.Sockets.Stream_Access;
      P_Player_Name : in Utilities.RemoteString.Type_String;
      P_Status      : in Status.Type_Adm_Status)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Player_Name_In - enter - exit");
      end if;

      Utilities.RemoteString.Type_String'Write (P_Channel, P_Player_Name);
      Status.Type_Adm_Status'Write (P_Channel, P_Status);

   end Get_Player_Name_Out;

   procedure Get_Updates_Summary_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Updates_Summary_In - enter");
      end if;

      P_Player_Id := Player.Type_Player_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Updates_Summary_In - exit");
      end if;
   end Get_Updates_Summary_In;

   procedure Get_Updates_Summary_Out
     (P_Channel         : in GNAT.Sockets.Stream_Access;
      P_Countdown       : in Positive;
      P_Game_Status     : in Status.Type_Game_Status;
      P_System_Messages : in Observation.Activity.Activity_Report.Vector)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Updates_Summary_Out - enter");
      end if;

      Positive'Output (P_Channel, P_Countdown);

      Status.Type_Game_Status'Output (P_Channel, P_Game_Status);

      Observation.Activity.Activity_Report.Vector'Output
        (P_Channel,
         P_System_Messages);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Get_Updates_Summary_Out - exit");
      end if;
   end Get_Updates_Summary_Out;

   procedure Client_Stopped_In
     (P_Channel   : in     GNAT.Sockets.Stream_Access;
      P_Player_Id :    out Player.Type_Player_Id)
   is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Client_Stopped_In - enter Player_Id=" &
            P_Player_Id'Img);
      end if;

      P_Player_Id := Player.Type_Player_Id'Input (P_Channel);

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Client_Stopped_In - exit");
      end if;

   end Client_Stopped_In;

   procedure Client_Stopped_Out (P_Channel : in GNAT.Sockets.Stream_Access) is

      use Server;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Client_Stopped_Out - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Generic_Server.ServerRCI.Client_Stopped_Out - exit");
      end if;

   end Client_Stopped_Out;

end Server.Generic_ServerRCI;
