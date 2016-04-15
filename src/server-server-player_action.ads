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

package Server.Server.Player_Action is
   procedure Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in     Piece.Type_Piece;
      P_Piece_Id                       :    out Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status;
      P_Force                          : in     Boolean := False);

   procedure Put_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Remove_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path                                    : in     Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   -- client sends us a particular path
   -- we need to validate it as if we had created it ourselves in the server.
   -- The path we received must be usable for this turn until it is "consumed"
   -- otherwise we will return a failiure
   procedure Perform_Move
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Moving_Piece_Id                : in     Piece.Type_Piece_Id;
      P_Path                           : in     Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Perform_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Perform_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

-- The from-to we received must be usable for this turn until it is "consumed"
-- otherwise we will return a failiure
   procedure Perform_Move
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Moving_Piece_Id                : in     Piece.Type_Piece_Id;
      P_From_Pos, P_To_Pos             : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Perform_Ranged_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Construction
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Constructing_Piece_Id          : in     Piece.Type_Piece_Id;
      P_Piece_Pos                      : in     Hexagon.Type_Hexagon_Position;
      P_Construction_Pos               : in     Hexagon.Type_Hexagon_Position;
      P_Construction                   : in     Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Perform_Demolition
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Demolition_Piece_Id            : in     Piece.Type_Piece_Id;
      P_Piece_Pos                      : in     Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos                 : in     Hexagon.Type_Hexagon_Position;
      P_Construction                   : in     Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   procedure Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status);

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Observation_Area
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Type_Action_Capabilities;

   function Movement_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Attack_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

end Server.Server.Player_Action;
