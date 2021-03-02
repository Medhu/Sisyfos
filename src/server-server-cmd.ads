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
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Hexagon.Area.Server_Area;

package Server.Server.Cmd is
   type Type_Cmd_Type is
     (Cmd_Create_Price, Cmd_Put_Piece, Cmd_Remove_Piece, Cmd_Perform_Attack,
      Cmd_Perform_Ranged_Attack, Cmd_Perform_Move, Cmd_Perform_Patch_Effect,
      Cmd_Perform_Piece_Effect, Cmd_Grant_Piece_Effect, Cmd_Revoke_Piece_Effect,
      Cmd_Grant_Patch_Effect, Cmd_Revoke_Patch_Effect);

   type Type_Create_Piece is record
      Player_Id       : Player.Type_Player_Id;
      Action_Type     : Action.Type_Action_Type;
      Pos             : Hexagon.Type_Hexagon_Position;
      Piece_To_Create : Piece.Server.Type_Piece_Access_Class;
   end record;

   type Type_Put_Piece is record
      Player_Id       : Player.Type_Player_Id;
      Action_Type     : Action.Type_Action_Type;
      Pos             : Hexagon.Type_Hexagon_Position;
      Piece_Id_To_Put : Piece.Type_Piece_Id;
   end record;

   type Type_Remove_Piece is record
      Player_Id          : Player.Type_Player_Id;
      Action_Type        : Action.Type_Action_Type;
      Piece_Id_To_Remove : Piece.Type_Piece_Id;
   end record;

   type Type_Perform_Attack is record
      Player_Id          : Player.Type_Player_Id;
      Action_Type        : Action.Type_Action_Type;
      Attacking_Piece_Id : Piece.Type_Piece_Id;
      Attacked_Piece_Id  : Piece.Type_Piece_Id;
   end record;

   type Type_Perform_Ranged_Attack is record
      Player_Id          : Player.Type_Player_Id;
      Action_Type        : Action.Type_Action_Type;
      Attacking_Piece_Id : Piece.Type_Piece_Id;
      Attacked_Piece_Id  : Piece.Type_Piece_Id;
   end record;

   type Type_Perform_Move is record
      Player_Id       : Player.Type_Player_Id;
      Action_Type     : Action.Type_Action_Type;
      Moving_Piece_Id : Piece.Type_Piece_Id;
      To_Pos          : Hexagon.Type_Hexagon_Position;
   end record;

   type Type_Perform_Patch_Effect is record
      Player_Id                     : Player.Type_Player_Id;
      Action_Type                   : Action.Type_Action_Type;
      Piece_Id_To_Perform_Effect_On : Piece.Type_Piece_Id;
      Effect_Name_To_Perform        : Effect.Type_Effect_Name;
      Area                          : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
   end record;

   type Type_Perform_Piece_Effect is record
      Player_Id                           : Player.Type_Player_Id;
      Action_Type                         : Action.Type_Action_Type;
      Piece_Id_To_Perform_Piece_Effect_On : Piece.Type_Piece_Id;
      Effect_Name_To_Perform                   : Effect.Type_Effect_Name;
   end record;

   type Type_Grant_Piece_Effect is record
      Player_Id                         : Player.Type_Player_Id;
      Action_Type                       : Action.Type_Action_Type;
      Piece_Id_To_Grant_Piece_Effect_On : Piece.Type_Piece_Id;
      Effect_To_Grant                   : Effect.Type_Effect;
   end record;

   type Type_Revoke_Piece_Effect is record
      Player_Id                            : Player.Type_Player_Id;
      Action_Type                          : Action.Type_Action_Type;
      Piece_Id_To_Revoke_Piece_Effect_From : Piece.Type_Piece_Id;
      Effect_Name_To_Revoke                : Effect.Type_Effect_Name;
   end record;

   type Type_Grant_Patch_Effect is record
      Player_Id         : Player.Type_Player_Id;
      Action_Type       : Action.Type_Action_Type;
      Granting_Piece_Id : Piece.Type_Piece_Id;
      Effect_To_Grant   : Effect.Type_Effect;
      Area              : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
   end record;

   type Type_Revoke_Patch_Effect is record
      Player_Id             : Player.Type_Player_Id;
      Action_Type           : Action.Type_Action_Type;
      Revoking_Piece_Id     : Piece.Type_Piece_Id;
      Effect_Name_To_Revoke : Effect.Type_Effect_Name;
      Area                  : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
   end record;

   Max_Attempt : constant Positive :=
     Positive'Last; -- Maximum number of attempts to do this command
   type Type_Cmd (P_Cmd_Type : Type_Cmd_Type) is record
      Attempt_Number     : Natural;
      Attempts_Remaining : Natural;

      case P_Cmd_Type is
         when Cmd_Create_Price =>
            Create_Piece_Details : Type_Create_Piece;

         when Cmd_Put_Piece =>
            Put_Piece_Details : Type_Put_Piece;

         when Cmd_Remove_Piece =>
            Remove_Piece_Details : Type_Remove_Piece;

         when Cmd_Perform_Attack =>
            Perform_Attack_Details : Type_Perform_Attack;

         when Cmd_Perform_Ranged_Attack =>
            Perform_Ranged_Attack_Details : Type_Perform_Ranged_Attack;

         when Cmd_Perform_Move =>
            Perform_Move_Details : Type_Perform_Move;

         when Cmd_Perform_Patch_Effect =>
            Perform_Patch_Effect_Details : Type_Perform_Patch_Effect;

         when Cmd_Perform_Piece_Effect =>
            Perform_Piece_Effect_Details : Type_Perform_Piece_Effect;

         when Cmd_Grant_Piece_Effect =>
            Grant_Piece_Effect_Details : Type_Grant_Piece_Effect;

         when Cmd_Revoke_Piece_Effect =>
            Revoke_Piece_Effect_Details : Type_Revoke_Piece_Effect;

         when Cmd_Grant_Patch_Effect =>
            Grant_Patch_Effect_Details : Type_Grant_Patch_Effect;

         when Cmd_Revoke_Patch_Effect =>
            Revoke_Patch_Effect_Details : Type_Revoke_Patch_Effect;

      end case;

   end record;

   type Type_Pointer_To_Cmd is access all Type_Cmd;
   package Cmd_List_Pkg is new Ada.Containers.Vectors (Positive, Type_Pointer_To_Cmd);

   procedure Free_Cmd is new Ada.Unchecked_Deallocation (Object => Type_Cmd,
      Name                                                      => Type_Pointer_To_Cmd);

   procedure Create_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Pos : in Hexagon.Type_Hexagon_Position; P_Piece : in Piece.Server.Type_Piece_Access_Class);

   procedure Put_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Pos : in     Hexagon.Type_Hexagon_Position; P_Piece_Id : in Piece.Type_Piece_Id);

   procedure Remove_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id                      : in     Piece.Type_Piece_Id);

   procedure Perform_Attack (P_Cmd_List         : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id);
   --
   procedure Perform_Ranged_Attack (P_Cmd_List  : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id);
   --
   procedure Perform_Move (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_To_Pos : in Hexagon.Type_Hexagon_Position);
   --
   procedure Perform_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name);
   --
   procedure Perform_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area                                  : in     Hexagon.Area.Type_Action_Capabilities_A);
--
--
   procedure Grant_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect : in Effect.Type_Effect);
--
   procedure Revoke_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name);
--
   procedure Grant_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect : in Effect.Type_Effect;
      P_Area                                : in     Hexagon.Area.Type_Action_Capabilities_A);
--
   procedure Revoke_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area                                 : in     Hexagon.Area.Type_Action_Capabilities_A);

end Server.Server.Cmd;
