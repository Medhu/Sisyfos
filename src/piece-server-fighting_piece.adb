--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2019  Frank J Jorgensen
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

package body Piece.Server.Fighting_Piece is

   Verbose : constant Boolean := False;

   Piece_Type_Info_List : Type_Piece_Type_Info_List_Access;

   procedure Init
     (P_Fighting_Piece_Class : in Piece.Server.Type_Piece'Class;
      P_Piece_Info           : in Type_Piece_Type_Info_List)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Init - enter");
      end if;

      if P_Fighting_Piece_Class.Id /= Piece.Undefined_Piece_Id then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_Fighting_Piece_Class.Type_Of_Piece /= Piece.Undefined_Piece_Type then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_Fighting_Piece_Class.Player_Id /= Player.Undefined_Player_Id then
         raise Init_Not_Undefined_Player_Id;
      end if;

      Piece_Class          := new Piece.Server.Type_Piece'Class'(P_Fighting_Piece_Class);
      Piece_Type_Info_List := new Type_Piece_Type_Info_List'(P_Piece_Info);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Init - exit");
      end if;
   end Init;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String
   is
   begin
      return Piece_Type_Info_List.all (P_Piece.Type_Of_Piece).Type_Name;
   end Get_Type_Of_Piece_Name;

   function Can_Attack_Here
     (P_Type_Of_Piece : in Type_Piece_Type; P_Landscape : in Landscape.Type_Landscape)
      return Boolean
   is
   begin
      return
        Piece.Server.Fighting_Piece.Piece_Type_Info_List (P_Type_Of_Piece).Attack_Landscape
          (P_Landscape);
   end Can_Attack_Here;

   function Can_Move_Here
     (P_Type_Of_Piece : in Type_Piece_Type; P_Landscape : in Landscape.Type_Landscape)
      return Boolean
   is
   begin
      return
        Piece.Server.Fighting_Piece.Piece_Type_Info_List (P_Type_Of_Piece).Move_Landscape
          (P_Landscape);
   end Can_Move_Here;

   procedure Perform_Attack
     (P_Player_Id : in     Player.Type_Player_Id; P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch; P_Winner : in Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
      Ret_Status : Status.Type_Status;
      A_Patch    : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Status;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Perform_Attack_Step - enter");
      end if;
Text_IO.Put_Line("A");
      if P_Winner = P_Attacked_Piece.Player_Id then
Text_IO.Put_Line("B");
         -- Attacked Piece won
         -- remove loosing piece, winner stays on his position
         Remove_Piece
           (P_Attacking_Piece.Player_Id, P_Action_Type, P_From_Patch, P_Attacking_Piece,
            Ret_Status);

         P_Status := Ret_Status;
--Text_IO.Put_Line("C:" & Ret_Status'Img);
      elsif P_Winner = P_Attacking_Piece.Player_Id then
Text_IO.Put_Line("D");
         -- Attacking Piece won
         -- remove loosing (Attacked Piece) piece
         Remove_Piece
           (P_Attacked_Piece.Player_Id, P_Action_Type, P_To_Patch, P_Attacked_Piece, Ret_Status);
Text_IO.Put_Line("E:" & Ret_Status'Img);

         if Ret_Status = Status.Ok then
--Text_IO.Put_Line("F");
            -- Move the Attacking Piece if patch where we won is free to move into
            A_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_To_Patch.Pos.A, P_To_Patch.Pos.B);

            if Landscape.Server.Is_Patch_Empty (Landscape.Type_Patch (A_Patch.all)) then
               -- move winning piece
Text_IO.Put_Line("G");
               Remove_Piece
                 (P_Attacking_Piece.Player_Id, P_Action_Type, P_From_Patch, P_Attacking_Piece,
                  Ret_Status);
Text_IO.Put_Line("H:" & Ret_Status'Img);
               if Ret_Status = Status.Ok then
                  Put_Piece
                    (P_Attacking_Piece.Player_Id, P_Action_Type, P_To_Patch, P_Attacking_Piece,
                     Ret_Status);
Text_IO.Put_Line("I:" & Ret_Status'Img);
               else
                  P_Status := Ret_Status;
--Text_IO.Put_Line("J");
               end if;
            end if;
         else
            P_Status := Ret_Status;
--Text_IO.Put_Line("K");
         end if;
--Text_IO.Put_Line("L");

      elsif P_Winner /= Player.Undefined_Player_Id then
         -- If the 'Winner' is neither of the two attacking/attacked piece and the winner is also
         -- not 'Undefined_Player_Id', then we raise an exception
         raise Winner_Not_Returned_Properly
           with "Winner Player_Id:" & P_Winner'Img & " is not one of Attacking Player_Id:" &
           P_Attacking_Piece.Player_Id'Img & ", Attacked Player_Id:" &
           P_Attacked_Piece.Player_Id'Img & " or Player.Undefined_Player_Id";

      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Perform_Attack_Step - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Winner : in Player.Type_Player_Id;
      P_Status :    out Status.Type_Status)
   is
      Ret_Status : Status.Type_Status;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Perform_Ranged_Attack - enter");
      end if;

      Ret_Status := Status.Ok;

      if P_Winner = P_Attacking_Piece.Player_Id then
         -- Attacking Piece won
         -- remove loosing (Attacked Piece) piece
         Remove_Piece
           (P_Attacked_Piece.Player_Id, P_Action_Type, P_To_Patch, P_Attacked_Piece, Ret_Status);

         -- In a ranged attack, the attacked piece can never win it can only successfully avoid the
         -- attack. If the attacked piece avoids the attack, we shall receive Undefined_Player_Id
         -- here.
      elsif P_Winner /= Player.Undefined_Player_Id then
         -- If the 'Winner' is neither attacking piece or 'Undefined_Player_Id', then we raise an
         -- exception
         raise Winner_Not_Returned_Properly
           with "Winner Player_Id:" & P_Winner'Img & " is not one of Attacking Player_Id:" &
           P_Attacking_Piece.Player_Id'Img & " or Player.Undefined_Player_Id";
      end if;

      P_Status := Ret_Status;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move_Step
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Moving_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Status :     out  Status.Type_Status)
   is
      Ret_Status1, Ret_Status2 : Status.Type_Status;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Fighting_Piece.Perform_Move_Step - enter");
      end if;

      Remove_Piece
        (P_Moving_Piece.Player_Id, P_Action_Type, P_From_Patch, P_Moving_Piece, Ret_Status1);

      if Ret_Status1 = Status.Ok then
         Put_Piece (P_Moving_Piece.Player_Id, P_Action_Type, P_To_Patch, P_Moving_Piece, Ret_Status2);
         if Ret_Status2 = Status.Ok then
            P_Status := Status.Ok;
         else
            P_Status := Ret_Status2;
         end if;
      else
         P_Status := Ret_Status1;
      end if;


      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Move_Step - exit Status Remove_Piece=" &
            Ret_Status1'Img & " Status Put_Piece=" & Ret_Status2'Img);
      end if;
   end Perform_Move_Step;

end Piece.Server.Fighting_Piece;
