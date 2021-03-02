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

package body Server.Server.Cmd is

   Verbose : constant Boolean := False;

   procedure Create_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Pos : in Hexagon.Type_Hexagon_Position; P_Piece : in Piece.Server.Type_Piece_Access_Class)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Create_Piece - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Create_Price, 0, Max_Attempt,
           Type_Create_Piece'(P_Player_Id, P_Action_Type, P_Pos, P_Piece));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Create_Piece - exit");
      end if;
   end Create_Piece;

   procedure Put_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Pos : in     Hexagon.Type_Hexagon_Position; P_Piece_Id : in Piece.Type_Piece_Id)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Put_Piece - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Put_Piece, 0, Max_Attempt,
           Type_Put_Piece'(P_Player_Id, P_Action_Type, P_Pos, P_Piece_Id));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Put_Piece - exit");
      end if;
   end Put_Piece;

--
   procedure Remove_Piece (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id                      : in     Piece.Type_Piece_Id)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Remove_Piece - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Remove_Piece, 0, Max_Attempt,
           Type_Remove_Piece'(P_Player_Id, P_Action_Type, P_Piece_Id));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Remove_Piece - exit");
      end if;
   end Remove_Piece;

   procedure Perform_Attack (P_Cmd_List         : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Cmd.Perform_Attack - enter P_Attacking_Piece_Id=" &
            P_Attacking_Piece_Id'Img & " P_Attacked_Piece_Id=" & P_Attacked_Piece_Id'Img);
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Perform_Attack, 0, Max_Attempt,
           Type_Perform_Attack'
             (P_Player_Id, P_Action_Type, P_Attacking_Piece_Id, P_Attacked_Piece_Id));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack (P_Cmd_List  : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Ranged_Attack - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Perform_Ranged_Attack, 0, Max_Attempt,
           Type_Perform_Ranged_Attack'
             (P_Player_Id, P_Action_Type, P_Attacking_Piece_Id, P_Attacked_Piece_Id));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_To_Pos : in Hexagon.Type_Hexagon_Position)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Move - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Perform_Move, 0, Max_Attempt,
           Type_Perform_Move'(P_Player_Id, P_Action_Type, P_Piece_Id, P_To_Pos));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Move - exit");
      end if;
   end Perform_Move;

   procedure Perform_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area                                  : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Patch_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Perform_Patch_Effect, 0, Max_Attempt,
           Type_Perform_Patch_Effect'
             (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name,
              new Hexagon.Area.Type_Action_Capabilities_A'(P_Area)));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Patch_Effect - exit");
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Piece_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Perform_Piece_Effect, 0, Max_Attempt,
           Type_Perform_Piece_Effect'(P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Perform_Piece_Effect - exit");
      end if;
   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect : in Effect.Type_Effect)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Grant_Piece_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Grant_Piece_Effect, 0, Max_Attempt,
           Type_Grant_Piece_Effect'(P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Grant_Piece_Effect - exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Revoke_Piece_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Revoke_Piece_Effect, 0, Max_Attempt,
           Type_Revoke_Piece_Effect'(P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Revoke_Piece_Effect - exit");
      end if;
   end Revoke_Piece_Effect;

--
   procedure Grant_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect : in Effect.Type_Effect;
      P_Area                                : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Grant_Patch_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Grant_Patch_Effect, 0, Max_Attempt,
           Type_Grant_Patch_Effect'
             (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect,
              new Hexagon.Area.Type_Action_Capabilities_A'(P_Area)));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Grant_Patch_Effect - exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect (P_Cmd_List : in out Cmd_List_Pkg.Vector;
      P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area                                 : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
      Cmd_Details : Type_Pointer_To_Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Revoke_Patch_Effect - enter");
      end if;

      Cmd_Details :=
        new Type_Cmd'
          (Cmd_Revoke_Patch_Effect, 0, Max_Attempt,
           Type_Revoke_Patch_Effect'
             (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect_Name,
              new Hexagon.Area.Type_Action_Capabilities_A'(P_Area)));

      Cmd_List_Pkg.Append (P_Cmd_List, Cmd_Details);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Cmd.Revoke_Patch_Effect - exit");
      end if;
   end Revoke_Patch_Effect;

end Server.Server.Cmd;
