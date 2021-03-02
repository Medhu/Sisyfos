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
with Hexagon.Server_Navigation;

package body Server.Server.Piece_Action is

   Verbose : constant Boolean := False;

   procedure Execute_Cmds (P_Cmd_List : in out Server.Cmd.Cmd_List_Pkg.Vector) is
      Ret_Status : Status.Type_Status;

      A_Cmd : Server.Cmd.Type_Pointer_To_Cmd;
      Trav  : Server.Cmd.Cmd_List_Pkg.Cursor;

      use Status;
      use Server.Cmd;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Execute_Cmds - enter");
      end if;

      Trav := Server.Cmd.Cmd_List_Pkg.First (P_Cmd_List);
      while Server.Cmd.Cmd_List_Pkg.Has_Element (Trav) loop

         A_Cmd := Server.Cmd.Cmd_List_Pkg.Element (Trav);

         if A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Create_Price then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Init_Piece
                    (A_Cmd.all.Create_Piece_Details.Player_Id,
                     A_Cmd.all.Create_Piece_Details.Action_Type, A_Cmd.all.Create_Piece_Details.Pos,
                     A_Cmd.all.Create_Piece_Details.Piece_To_Create, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Init_Piece not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;

         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Put_Piece then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Put_Piece
                    (A_Cmd.all.Put_Piece_Details.Player_Id, A_Cmd.all.Put_Piece_Details.Action_Type,
                     A_Cmd.all.Put_Piece_Details.Pos, A_Cmd.all.Put_Piece_Details.Piece_Id_To_Put,
                     Ret_Status, A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Put_Piece not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Remove_Piece then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Remove_Piece
                    (A_Cmd.all.Remove_Piece_Details.Player_Id,
                     A_Cmd.all.Remove_Piece_Details.Action_Type,
                     A_Cmd.all.Remove_Piece_Details.Piece_Id_To_Remove, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Remove_Piece not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Perform_Attack then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Perform_Attack
                    (A_Cmd.all.Perform_Attack_Details.Player_Id,
                     A_Cmd.all.Perform_Attack_Details.Action_Type,
                     A_Cmd.all.Perform_Attack_Details.Attacking_Piece_Id,
                     A_Cmd.all.Perform_Attack_Details.Attacked_Piece_Id, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Perform_Attack not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Perform_Ranged_Attack then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Perform_Ranged_Attack
                    (A_Cmd.all.Perform_Ranged_Attack_Details.Player_Id,
                     A_Cmd.all.Perform_Ranged_Attack_Details.Action_Type,
                     A_Cmd.all.Perform_Ranged_Attack_Details.Attacking_Piece_Id,
                     A_Cmd.all.Perform_Ranged_Attack_Details.Attacked_Piece_Id, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Perform_Ranged_Attack not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Perform_Move then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Perform_Move
                    (A_Cmd.all.Perform_Move_Details.Player_Id,
                     A_Cmd.all.Perform_Move_Details.Action_Type,
                     A_Cmd.all.Perform_Move_Details.Moving_Piece_Id,
                     A_Cmd.all.Perform_Move_Details.To_Pos, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Perform_Move not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Perform_Patch_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Perform_Patch_Effect
                    (A_Cmd.all.Perform_Patch_Effect_Details.Player_Id,
                     A_Cmd.all.Perform_Patch_Effect_Details.Action_Type,
                     A_Cmd.all.Perform_Patch_Effect_Details.Piece_Id_To_Perform_Effect_On,
                     A_Cmd.all.Perform_Patch_Effect_Details.Effect_Name_To_Perform,
                     A_Cmd.all.Perform_Patch_Effect_Details.Area.all, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Perform_Patch_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Perform_Piece_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Perform_Piece_Effect
                    (A_Cmd.all.Perform_Piece_Effect_Details.Player_Id,
                     A_Cmd.all.Perform_Piece_Effect_Details.Action_Type,
                     A_Cmd.all.Perform_Piece_Effect_Details.Piece_Id_To_Perform_Piece_Effect_On,
                     A_Cmd.all.Perform_Piece_Effect_Details.Effect_Name_To_Perform, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Perform_Piece_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Grant_Piece_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin
                  Server.Piece_Action.Grant_Piece_Effect
                    (A_Cmd.all.Grant_Piece_Effect_Details.Player_Id,
                     A_Cmd.all.Grant_Piece_Effect_Details.Action_Type,
                     A_Cmd.all.Grant_Piece_Effect_Details.Piece_Id_To_Grant_Piece_Effect_On,
                     A_Cmd.all.Grant_Piece_Effect_Details.Effect_To_Grant, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Grant_Piece_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Revoke_Piece_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Revoke_Piece_Effect
                    (A_Cmd.all.Revoke_Piece_Effect_Details.Player_Id,
                     A_Cmd.all.Revoke_Piece_Effect_Details.Action_Type,
                     A_Cmd.all.Revoke_Piece_Effect_Details.Piece_Id_To_Revoke_Piece_Effect_From,
                     A_Cmd.all.Revoke_Piece_Effect_Details.Effect_Name_To_Revoke, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Revoke_Piece_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Grant_Patch_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Grant_Patch_Effect
                    (A_Cmd.all.Grant_Patch_Effect_Details.Player_Id,
                     A_Cmd.all.Grant_Patch_Effect_Details.Action_Type,
                     A_Cmd.all.Grant_Patch_Effect_Details.Granting_Piece_Id,
                     A_Cmd.all.Grant_Patch_Effect_Details.Effect_To_Grant,
                     A_Cmd.all.Grant_Patch_Effect_Details.Area.all, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Grant_Patch_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         elsif A_Cmd.all.P_Cmd_Type = Server.Cmd.Cmd_Revoke_Patch_Effect then
            if A_Cmd.all.Attempts_Remaining > 0 then
               A_Cmd.all.Attempt_Number := A_Cmd.all.Attempt_Number + 1;

               declare
                  Attempts_Remaining_Before_Call : Integer := A_Cmd.all.Attempts_Remaining;
               begin

                  Server.Piece_Action.Revoke_Patch_Effect
                    (A_Cmd.all.Revoke_Patch_Effect_Details.Player_Id,
                     A_Cmd.all.Revoke_Patch_Effect_Details.Action_Type,
                     A_Cmd.all.Revoke_Patch_Effect_Details.Revoking_Piece_Id,
                     A_Cmd.all.Revoke_Patch_Effect_Details.Effect_Name_To_Revoke,
                     A_Cmd.all.Revoke_Patch_Effect_Details.Area.all, Ret_Status,
                     A_Cmd.all.Attempts_Remaining);

                  if Attempts_Remaining_Before_Call = A_Cmd.all.Attempts_Remaining then
                     -- This may cause commands never to complete
                     raise Attempts_Remaining_Not_Updated
                       with "Server.Piece_Action.Revoke_Patch_Effect not changing 'Attempts_Remaining'.";
                  end if;

               end;

               Server.Observe_Game (1);
            end if;
         end if;

         Trav := Cmd_List_Pkg.Next (Trav);
      end loop;

      Trav := Server.Cmd.Cmd_List_Pkg.First (P_Cmd_List);
      while Server.Cmd.Cmd_List_Pkg.Has_Element (Trav) loop

         A_Cmd := Server.Cmd.Cmd_List_Pkg.Element (Trav);

         if A_Cmd.all.Attempts_Remaining <= 0 then
            Server.Cmd.Free_Cmd (A_Cmd);
            Server.Cmd.Cmd_List_Pkg.Delete (P_Cmd_List, Trav);
         end if;

         Trav := Cmd_List_Pkg.Next (Trav);

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Execute_Cmds - exit");
      end if;

   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Execute_Cmds - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Attempting command:" & A_Cmd.all.P_Cmd_Type'Img & " at attempt number:" &
            A_Cmd.all.Attempt_Number'Img & " remaining attempts:" &
            A_Cmd.all.Attempts_Remaining'Img);
         raise;
   end Execute_Cmds;

   procedure New_Piece (P_Piece : in     Piece.Type_Piece;
      P_Piece_Server            :    out Piece.Server.Type_Piece_Access_Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.New_Piece - enter");
      end if;

      Piece.Server.New_Piece (P_Piece, P_Piece_Server);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.New_Piece - exit");
      end if;
   end New_Piece;

   procedure Delete_Piece (P_Piece_Server : in out Piece.Server.Type_Piece_Access_Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.New_Piece - enter");
      end if;

      Piece.Server.Delete_Piece (P_Piece_Server);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.New_Piece - exit");
      end if;
   end Delete_Piece;

   procedure Init_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Piece.Server.Type_Piece_Access_Class; P_Status : out Status.Type_Status;
      P_Attempts_Remaining           : in out Integer; P_Force : in Boolean := False)
   is
      A_Patch       : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Result_Status : Status.Type_Result_Status;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Init_Piece - enter");
      end if;

      P_Status := Status.Ok;

      if not P_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Init Piece Pos not valid. Command will be cancelled."));
      else
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

         if P_Status = Status.Ok then
            Piece.Server.Before_Create_Piece
              (P_Player_Id, P_Action_Type, P_Pos, P_Piece.all, Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Create_Piece;
            end if;
         end if;

         if P_Status = Status.Ok then
            P_Piece.all.Id := Piece.Server.Generate_Piece_Id;

            Piece.Server.Pieces_Server_List.Append
              (Piece.Server.All_Pieces_In_Game,
               Piece.Server.Type_Piece_Position'
                 (P_Piece, Hexagon.Type_Hexagon_Position'(P_Valid => False)));

--
--           -- Put the piece on the map
            Piece.Server.Put_Piece
              (P_Player_Id, P_Action_Type, Landscape.Type_Patch (A_Patch.all), P_Piece.all,
               P_Status);

         end if;

         -- We succeeded in placing the piece
         if P_Status = Status.Ok then
            Piece.Set_Name
              (Piece.Type_Piece (P_Piece.all),
               Utilities.RemoteString.To_Unbounded_String
                 ("Piece " &
                  Utilities.RemoteString.To_String
                    (Piece.Server.Get_Type_Of_Piece_Name (Piece.Type_Piece (P_Piece.all))) &
                  " # " & P_Piece.Id'Img));
         end if;

         Piece.Server.End_Create_Piece
           (P_Player_Id, P_Action_Type, P_Pos, P_Piece.all, P_Status, P_Attempts_Remaining);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Init_Piece - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Init_Piece - Exception:");
         Text_IO.Put
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img);
         if P_Pos.P_Valid then
            Text_IO.Put (Text_IO.Current_Error, " P_Pos:" & P_Pos.A'Img & ", " & P_Pos.B'Img);
         else
            Text_IO.Put (Text_IO.Current_Error, " P_Pos: Invalid");
         end if;
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            " P_Piece:" & P_Piece.all.Id'Img & " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Init_Piece;

   procedure Put_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id                    : in     Piece.Type_Piece_Id; P_Status : out Status.Type_Status;
      P_Attempts_Remaining          : in out Integer)
   is
      A_Patch          : Hexagon.Server_Map.Type_Server_Patch_Adress;
      A_Piece_Position : Piece.Server.Type_Piece_Position;

      Result_Status : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Put_Piece - enter");
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Put Piece Piece_Id:" & P_Piece_Id'Img & " not valid. Command will be cancelled."));
      elsif not P_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Put Piece Position not valid. Command will be cancelled."));
      else
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

         if P_Status = Status.Ok then
            Piece.Server.Before_Put_Piece
              (P_Player_Id, P_Action_Type, P_Pos,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Put_Piece;
            end if;
         end if;

         if P_Status = Status.Ok then
            Piece.Server.Put_Piece
              (P_Player_Id, P_Action_Type, Landscape.Type_Patch (A_Patch.all),
               A_Piece_Position.Actual_Piece.all, P_Status);
         end if;

         Piece.Server.End_Put_Piece
           (P_Player_Id, P_Action_Type, P_Pos,
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Status,
            P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Put_Piece - exit " & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Put_Piece - Exception:");
         Text_IO.Put
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img);
         if P_Pos.P_Valid then
            Text_IO.Put (Text_IO.Current_Error, " P_Pos:" & P_Pos.A'Img & ", " & P_Pos.B'Img);
         else
            Text_IO.Put (Text_IO.Current_Error, " P_Pos:Invalid");
         end if;
         Text_IO.Put_Line
           (Text_IO.Current_Error, " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Put_Piece;

   procedure Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Status :    out Status.Type_Status; P_Attempts_Remaining : in out Integer)
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      A_Patch          : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Result_Status : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Remove_Piece - enter");
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Remove Piece Piece_Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Remove Piece Position not valid. Command will be cancelled."));
      else
         A_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (A_Piece_Position.Actual_Pos.A, A_Piece_Position.Actual_Pos.B);

         if P_Status = Status.Ok then
            Piece.Server.Before_Remove_Piece
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Remove_Piece;
            end if;
         end if;

         if P_Status = Status.Ok then
            Piece.Server.Remove_Piece
              (P_Player_Id, P_Action_Type, Landscape.Type_Patch (A_Patch.all),
               A_Piece_Position.Actual_Piece.all, P_Status);

         end if;

         Piece.Server.End_Remove_Piece
           (P_Player_Id, P_Action_Type, Landscape.Type_Patch (A_Patch.all),
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Status,
            P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Remove_Piece - exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Remove_Piece - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Remove_Piece;

   procedure Perform_Attack (P_Player_Id        : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status :    out Status.Type_Status; P_Attempts_Remaining : in out Integer)
   is
      An_Attacking_Patch, An_Attacked_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      An_Attacking_Piece_Position, An_Attacked_Piece_Position : Piece.Server.Type_Piece_Position;
      Result_Status                                           : Status.Type_Result_Status;

      A_Winner : Player.Type_Player_Id;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Perform_Attack - enter");
      end if;
      P_Status := Status.Ok;

      begin
         An_Attacking_Piece_Position := Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            An_Attacking_Piece_Position.Actual_Piece := null;
            An_Attacking_Piece_Position.Actual_Pos   :=
              Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      begin
         An_Attacked_Piece_Position := Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            An_Attacked_Piece_Position.Actual_Piece := null;
            An_Attacked_Piece_Position.Actual_Pos   :=
              Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if An_Attacking_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Attack Attacking Piece Id:" & P_Attacking_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif An_Attacked_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Attack Attacked Piece Id:" & P_Attacked_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not An_Attacking_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Attack Attacking Piece Position not valid. Command will be cancelled."));
      elsif not An_Attacked_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Attack Attacked Piece Position not valid. Command will be cancelled."));
      else
         An_Attacking_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (An_Attacking_Piece_Position.Actual_Pos.A, An_Attacking_Piece_Position.Actual_Pos.B);
         An_Attacked_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (An_Attacked_Piece_Position.Actual_Pos.A, An_Attacked_Piece_Position.Actual_Pos.B);

         if P_Status = Status.Ok then
            Piece.Server.Fighting_Piece.Before_Perform_Attack
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacked_Piece_Position.Actual_Piece.all),
               An_Attacking_Piece_Position.Actual_Pos, An_Attacked_Piece_Position.Actual_Pos,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Perform_Attack;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Fighting_Piece.Calculate_Attack_Result
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacked_Piece_Position.Actual_Piece.all),
               An_Attacking_Patch.all.Pos, An_Attacked_Patch.all.Pos, A_Winner);

            Piece.Server.Fighting_Piece.Perform_Attack
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece (An_Attacked_Piece_Position.Actual_Piece.all),
               Landscape.Type_Patch (An_Attacking_Patch.all),
               Landscape.Type_Patch (An_Attacked_Patch.all), A_Winner);

         end if;

         Piece.Server.Fighting_Piece.End_Perform_Attack
           (P_Player_Id, P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece'Class
              (An_Attacking_Piece_Position.Actual_Piece.all),
            Piece.Server.Fighting_Piece.Type_Piece'Class
              (An_Attacked_Piece_Position.Actual_Piece.all),
            An_Attacking_Patch.all.Pos, An_Attacked_Patch.all.Pos, A_Winner, P_Status,
            P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Attack - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Perform_Attack - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img &
            " P_Attacking_Piece:" & P_Attacking_Piece_Id'Img & " P_Attacked_Piece:" &
            P_Attacked_Piece_Id'Img & " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Perform_Attack;

   procedure Perform_Ranged_Attack (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Status :    out Status.Type_Status; P_Attempts_Remaining : in out Integer)
   is
      An_Attacking_Piece_Position, An_Attacked_Piece_Position : Piece.Server.Type_Piece_Position;
      An_Attacking_Patch, An_Attacked_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Result_Status                                           : Status.Type_Result_Status;

      A_Winner : Player.Type_Player_Id := Player.Undefined_Player_Id;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Perform_Ranged_Attack - enter");
      end if;
      P_Status := Status.Ok;

      begin
         An_Attacking_Piece_Position := Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            An_Attacking_Piece_Position.Actual_Piece := null;
            An_Attacking_Piece_Position.Actual_Pos   :=
              Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      begin
         An_Attacked_Piece_Position := Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            An_Attacked_Piece_Position.Actual_Piece := null;
            An_Attacked_Piece_Position.Actual_Pos   :=
              Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if An_Attacking_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Ranged Attack Attacking Piece Id:" & P_Attacking_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif An_Attacked_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Ranged Attack Attacked Piece Id:" & P_Attacked_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not An_Attacking_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Ranged Attack Attacking Piece Position not valid. Command will be cancelled."));
      elsif not An_Attacked_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Ranged Attack Attacked Piece Position not valid. Command will be cancelled."));
      else
         An_Attacking_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (An_Attacking_Piece_Position.Actual_Pos.A, An_Attacking_Piece_Position.Actual_Pos.B);
         An_Attacked_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (An_Attacked_Piece_Position.Actual_Pos.A, An_Attacked_Piece_Position.Actual_Pos.B);

         if P_Status = Status.Ok then
            Piece.Server.Fighting_Piece.Before_Perform_Ranged_Attack
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacked_Piece_Position.Actual_Piece.all),
               An_Attacking_Piece_Position.Actual_Pos, An_Attacked_Piece_Position.Actual_Pos,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Perform_Ranged_Attack;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Fighting_Piece.Calculate_Ranged_Attack_Result
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (An_Attacked_Piece_Position.Actual_Piece.all),
               An_Attacking_Patch.all.Pos, An_Attacked_Patch.all.Pos, A_Winner);

            Piece.Server.Fighting_Piece.Perform_Ranged_Attack
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece
                 (An_Attacking_Piece_Position.Actual_Piece.all),
               Piece.Server.Fighting_Piece.Type_Piece (An_Attacked_Piece_Position.Actual_Piece.all),
               Landscape.Type_Patch (An_Attacking_Patch.all),
               Landscape.Type_Patch (An_Attacked_Patch.all), A_Winner);

         end if;

         Piece.Server.Fighting_Piece.End_Perform_Ranged_Attack
           (P_Player_Id, P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece'Class
              (An_Attacking_Piece_Position.Actual_Piece.all),
            Piece.Server.Fighting_Piece.Type_Piece'Class
              (An_Attacked_Piece_Position.Actual_Piece.all),
            An_Attacking_Patch.all.Pos, An_Attacked_Patch.all.Pos, A_Winner, P_Status,
            P_Attempts_Remaining);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Ranged_Attack - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Perform_Ranged_Attack - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img &
            " P_Attacking_Piece:" & P_Attacking_Piece_Id'Img & " P_Attacked_Piece:" &
            P_Attacked_Piece_Id'Img & " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Perform_Ranged_Attack;

   procedure Perform_Move (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_End_Pos : in     Hexagon.Type_Hexagon_Position; P_Status : out Status.Type_Status;
      P_Attempts_Remaining             : in out Integer)
   is
      A_Moving_Piece_Position  : Piece.Server.Type_Piece_Position;
      A_From_Patch, A_To_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;

      A_To_Pos, A_From_Pos : Hexagon.Type_Hexagon_Position;

      Move_Path        : Hexagon.Server_Navigation.Path_Pkg.Vector;
      Next_Path_Cursor : Hexagon.Server_Navigation.Path_Pkg.Cursor;
      Result_Status    : Status.Type_Result_Status;

      use Piece.Server;
      use Hexagon;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Move - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      P_Status := Status.Ok;

      begin
         A_Moving_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Moving_Piece_Position.Actual_Piece := null;
            A_Moving_Piece_Position.Actual_Pos := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Moving_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Move Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Moving_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Move Moving Piece Position not valid. Command will be cancelled."));
      else
         A_From_Pos := A_Moving_Piece_Position.Actual_Pos;
         A_To_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);

         A_From_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (A_From_Pos.A, A_From_Pos.B);

         if P_Status = Status.Ok then
            Hexagon.Server_Navigation.Find_Path
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece_Position.Actual_Piece.all),
               A_From_Pos, P_End_Pos, P_Status, Move_Path);

         end if;

         if P_Status = Status.Ok then
            -- next patch in the current path
            Next_Path_Cursor := Hexagon.Server_Navigation.Path_Pkg.First (Move_Path);
            Next_Path_Cursor := Hexagon.Server_Navigation.Path_Pkg.Next (Next_Path_Cursor);

            A_To_Pos   := Hexagon.Server_Navigation.Path_Pkg.Element (Next_Path_Cursor).all.Pos;
            A_To_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (A_To_Pos.A, A_To_Pos.B);

            Piece.Server.Fighting_Piece.Before_Perform_Move
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece'Class
                 (A_Moving_Piece_Position.Actual_Piece.all),
               A_From_Pos, A_To_Pos, P_End_Pos, Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Perform_Move;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Fighting_Piece.Perform_Move_Step
              (P_Player_Id, P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece_Position.Actual_Piece.all),
               Landscape.Type_Patch (A_From_Patch.all), Landscape.Type_Patch (A_To_Patch.all));

            if A_To_Pos = P_End_Pos then
               P_Status := Status.Completed_Ok;
            end if;

            Hexagon.Server_Navigation.Path_Pkg.Clear (Move_Path);
         end if;

         Piece.Server.Fighting_Piece.End_Perform_Move
           (P_Player_Id, P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece_Position.Actual_Piece.all),
            A_From_Pos, A_To_Pos, P_End_Pos, P_Status, P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Move - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Perform_Move - Exception:");
         Text_IO.Put
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img);
         if P_End_Pos.P_Valid then
            Text_IO.Put
              (Text_IO.Current_Error, " P_End_Pos:" & P_End_Pos.A'Img & ", " & P_End_Pos.B'Img);
         else
            Text_IO.Put (Text_IO.Current_Error, " P_End_Pos:Invalid");
         end if;
         Text_IO.Put_Line
           (Text_IO.Current_Error, " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Perform_Move;

   procedure Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Status :    out Status.Type_Status; P_Attempts_Remaining : in out Integer)
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      A_Patch          : Hexagon.Server_Map.Type_Server_Patch_Adress := null;

      Result_Status : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Patch_Effect - enter P_Piece.all.Id=" &
            P_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Patch Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Patch Effect Piece Position not valid. Command will be cancelled."));
      else
         if not Piece.Server.Validate_Patch_Effect
             (P_Action_Type, Piece.Server.Type_Piece (A_Piece_Position.Actual_Piece.all), P_Effect_Name,
              P_Area)
         then
            P_Status := Status.Patch_Effect_Not_Here;
         end if;

         if P_Status = Status.Ok then
            Piece.Server.Before_Perform_Patch_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect_Name, P_Area,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Perform_Patch_Effect;

            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Perform_Patch_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Area, P_Effect_Name);

         end if;

         Piece.Server.End_Perform_Patch_Effect
           (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect_Name, P_Area,
            P_Status, P_Attempts_Remaining);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Patch_Effect - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Perform_Patch_Effect - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect:" & P_Effect_Name'Img &
            " P_Area: --  P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Status : out Status.Type_Status;
      P_Attempts_Remaining                     : in out Integer)
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      Result_Status    : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Piece_Effect - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Piece Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Perform Piece Effect Piece Position not valid. Command will be cancelled."));
      else

         if not Piece.Server.Validate_Piece_Effect
             (P_Action_Type, Piece.Server.Type_Piece (A_Piece_Position.Actual_Piece.all), P_Effect_Name)
         then
            P_Status := Status.Piece_Effect_Not_Here;

         end if;

         if P_Status = Status.Ok then
            Piece.Server.Before_Perform_Piece_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect_Name,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Perform_Piece_Effect;

            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Perform_Piece_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect_Name);

         end if;

         Piece.Server.End_Perform_Piece_Effect
           (P_Player_Id, P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect_Name, P_Status,
            P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Perform_Piece_Effect - exit P_Status=" & P_Status'Img);
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Perform_Piece_Effect - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect:" & P_Effect_Name'Img &
            " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect : in     Effect.Type_Effect; P_Status : out Status.Type_Status;
      P_Attempts_Remaining                   : in out Integer)
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      Result_Status    : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Grant_Piece_Effect- enter");
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Grant Piece Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Grant Piece Effect Piece Position not valid. Command will be cancelled."));
      else
         if P_Status = Status.Ok then
            Piece.Server.Before_Grant_Piece_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Grant_Piece_Effect;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Grant_Piece_Effect
              (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect, P_Status);
         end if;

         Piece.Server.End_Grant_Piece_Effect
           (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect, P_Status,
            P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Grant_Piece_Effect- exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Grant_Piece_Effect - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect:" & P_Effect.Effect_Name'Img & " " & P_Effect.Aux'Img &
            " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Status : out Status.Type_Status;
      P_Attempts_Remaining                    : in out Integer)
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      Result_Status    : Status.Type_Result_Status;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Revoke_Piece_Effect- enter");
      end if;

      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Revoke Piece Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Revoke Piece Effect Piece Position not valid. Command will be cancelled."));
      else
         if P_Status = Status.Ok then
            Piece.Server.Before_Revoke_Piece_Effect
              (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect_Name,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Revoke_Piece_Effect;

            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Revoke_Piece_Effect
              (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect_Name,
               P_Status);
         end if;

         Piece.Server.End_Revoke_Piece_Effect
           (P_Player_Id, P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Effect_Name,
            P_Status, P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Revoke_Piece_Effect- exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Revoke_Piece_Effect - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect_Name:" & P_Effect_Name'Img & " P_Attempts_Remaining:" &
            P_Attempts_Remaining'Img);
         raise;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect : in     Effect.Type_Effect; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Status :    out Status.Type_Status; P_Attempts_Remaining : in out Integer)
   is
      Result_Status    : Status.Type_Result_Status;
      A_Piece_Position : Piece.Server.Type_Piece_Position;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Grant_Patch_Effect- enter");
      end if;
      P_Status := Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Grant Patch Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Grant Patch Effect Piece Position not valid. Command will be cancelled."));
      else
         if P_Status = Status.Ok then

            Piece.Server.Before_Grant_Patch_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Area, P_Effect,
               Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Grant_Patch_Effect;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Grant_Patch_Effect
              (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect, P_Area,
               P_Status);

         end if;

         Piece.Server.End_Grant_Patch_Effect
           (P_Player_Id, P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Area, P_Effect,
            P_Status, P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Grant_Patch_Effect- exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line (Text_IO.Current_Error, "Server.Server.Piece_Action.Grant_Patch_Effect:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect:" & P_Effect.Effect_Name'Img & " P_Area: -- " &
            P_Effect.Aux'Img & " P_Attempts_Remaining:" & P_Attempts_Remaining'Img);
         raise;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Piece_Id : in Piece.Type_Piece_Id;
      P_Effect_Name                           : in     Effect.Type_Effect_Name;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Status : out Status.Type_Status;
      P_Attempts_Remaining                    : in out Integer)
   is
      Result_Status    : Status.Type_Result_Status;
      A_Piece_Position : Piece.Server.Type_Piece_Position;

      use Piece.Server;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Piece_Action.Revoke_Patch_Effect- enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      if A_Piece_Position.Actual_Piece = null then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Revoke Patch Effect Piece Id:" & P_Piece_Id'Img &
               " not valid. Command will be cancelled."));
      elsif not A_Piece_Position.Actual_Pos.P_Valid then
         P_Attempts_Remaining := 0;

         Server.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Revoke Patch Effect Piece Position not valid. Command will be cancelled."));
      else

         if P_Status = Status.Ok then
            Piece.Server.Before_Revoke_Patch_Effect
              (P_Player_Id, P_Action_Type,
               Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Area,
               P_Effect_Name, Result_Status);

            if Result_Status /= Status.Proceed then
               P_Status := Status.Not_Before_Revoke_Patch_Effect;
            end if;
         end if;

         if P_Status = Status.Ok then

            Piece.Server.Revoke_Patch_Effect
              (P_Player_Id, P_Action_Type, A_Piece_Position.Actual_Piece.all, P_Effect_Name, P_Area,
               P_Status);

         end if;

         Piece.Server.End_Revoke_Patch_Effect
           (P_Player_Id, P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece_Position.Actual_Piece.all), P_Area,
            P_Effect_Name, P_Status, P_Attempts_Remaining);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Revoke_Patch_Effect- exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Server.Server.Piece_Action.Revoke_Patch_Effect - Exception:");
         Text_IO.Put_Line
           (Text_IO.Current_Error,
            "Player_Id:" & P_Player_Id'Img & " P_Action_Type:" & P_Action_Type'Img & " P_Piece:" &
            P_Piece_Id'Img & " P_Effect:" & P_Effect_Name'Img & " P_Attempts_Remaining:" &
            P_Attempts_Remaining'Img);
         raise;
   end Revoke_Patch_Effect;

   function Is_Effect_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                            : in Piece.Type_Piece_Id;
      P_Effect_Name                         : in Effect.Type_Effect_Name) return Boolean
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      Ret              : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Is_Effect_On_Piece- enter");
      end if;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      Ret :=
        Piece.Server.Is_Effect_On_Piece
          (P_Player_Id, A_Piece_Position.Actual_Piece.all, P_Effect_Name);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Is_Effect_On_Piece- exit");
      end if;

      return Ret;
   end Is_Effect_On_Piece;

   function Get_Effect_Aux_On_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Piece_Id                                 : in Piece.Type_Piece_Id;
      P_Effect_Name                              : in Effect.Type_Effect_Name) return Natural
   is
      A_Piece_Position : Piece.Server.Type_Piece_Position;
      Ret              : Natural;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Get_Effect_Aux_On_Piece- enter");
      end if;

      begin
         A_Piece_Position := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      exception
         when Piece.Server.Piece_Not_Found_Piece_Position =>
            A_Piece_Position.Actual_Piece := null;
            A_Piece_Position.Actual_Pos   := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      end;

      Ret :=
        Piece.Server.Get_Effect_Aux_On_Piece
          (P_Player_Id, A_Piece_Position.Actual_Piece.all, P_Effect_Name);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Piece_Action.Get_Effect_Aux_On_Piece- exit");
      end if;

      return Ret;
   end Get_Effect_Aux_On_Piece;

end Server.Server.Piece_Action;
