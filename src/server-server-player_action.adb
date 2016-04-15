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

with Hexagon.Server_Map;
with Text_IO;

package body Server.Server.Player_Action is

   Verbose : constant Boolean := True;

   procedure Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in     Piece.Type_Piece;
      P_Piece_Id                       :    out Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status;
      P_Force                          : in     Boolean := False)
   is
      A_Piece             : Piece.Server.Type_Piece_Access_Class;
      Resources_Available : Boolean;
      A_Patch             : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Create_Piece - enter");
      end if;
      P_Status := Status.Ok;

      -- 1 Instanciate the piece
      Piece.Server.Create_Piece (P_Action_Type, P_Piece, A_Piece, P_Status);
      if P_Status = Status.Ok then
         if not Piece.Server.Validate_Create_Piece
             (P_Action_Type,
              P_Pos,
              A_Piece.all,
              P_Current_Player_Id,
              P_Player_Id)
         then
            P_Status := Status.Not_Players_Turn;
         end if;
      end if;

      if P_Status = Status.Ok then

         -- 2 check the position
         if not P_Force then
            Piece.Server.Validate_Create_Piece_Area (P_Action_Type, A_Piece.all, P_Pos, P_Status);
         end if;
         if P_Status = Status.Ok then

            -- 3 check for resources
            if not P_Force then
               Piece.Server.Spend_Resources_On_Piece
                 (P_Action_Type,
                  A_Piece.all,
                  P_Pos,
                  Resources_Available);
            else
               Resources_Available := True;
            end if;

            if Resources_Available then

               -- 4 set Id
               P_Piece_Id     := Piece.Server.Generate_Piece_Id;
               A_Piece.all.Id := P_Piece_Id;

               Piece.Set_Name
                 (Piece.Type_Piece (A_Piece.all),
                  Piece.Server.Create_Piece_Name (A_Piece.all));

               -- 5 Add the new piece to the server list
               Piece.Server.Pieces_Server_List.Append
                 (Piece.Server.All_Pieces_In_Game,
                  Piece.Server.Type_Piece_Position'
                    (A_Piece, Hexagon.Type_Hexagon_Position'(P_Valid => False)));

               A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB(P_Pos.A, P_Pos.B);
               -- 6 Put the piece on the map
               Piece.Server.Put_Piece (P_Action_Type, Landscape.Type_Patch(A_Patch.all), A_Piece.all, P_Player_Id, P_Status);

               Player_Activity_Report_Append
                 (Observation.Activity.Internal_Details,
                  P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("You placed a " &
                     Utilities.RemoteString.To_String
                       (Piece.Server.Get_Type_Of_Piece_Name (Piece.Type_Piece (A_Piece.all))) &
                     " at " &
                     P_Pos.A'Img &
                     ", " &
                     P_Pos.B'Img &
                     " called " &
                     Utilities.RemoteString.To_String
                       (Piece.Get_Name (Piece.Type_Piece (A_Piece.all)))));

               Piece.Server.After_Create_Piece
                 (P_Action_Type,
                  P_Pos,
                  A_Piece.all,
                  P_Current_Player_Id,
                  P_Player_Id);

            else
               P_Status := Status.Not_Allowed_To_Create_Piece;
            end if;
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Create_Piece - exit P_Status=" & P_Status'Img);
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class;
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Put_Piece - enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      A_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (P_Pos.A,
                    P_Pos.B);

      if not Piece.Server.Validate_Put_Piece
          (P_Action_Type,
           P_Pos,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Put_Piece (P_Action_Type, Landscape.Type_Patch(A_Patch.all), A_Piece.all, P_Player_Id, P_Status);
      end if;

      Observe_Game (1);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Put_Piece - exit " & P_Status'Img);
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class;
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Remove_Piece - enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);
      A_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (P_Pos.A,
                    P_Pos.B);

      if not Piece.Server.Validate_Remove_Piece
          (P_Action_Type,
           P_Pos,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else

         Piece.Server.Remove_Piece (P_Action_Type, Landscape.Type_Patch(A_Patch.all), A_Piece.all, P_Player_Id, P_Status);
         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Remove_Piece - exit");
      end if;
   end Remove_Piece;

   procedure Perform_Attack
     (P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path                                    : in     Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      A_From_Patch,
      A_To_Patch,
      Start_Patch,
      End_Patch                           : Hexagon.Server_Map.Type_Server_Patch_Adress;
      A_Attacking_Piece, A_Attacked_Piece : Piece.Server.Type_Piece_Access_Class;

      From     : Hexagon.Path.Cursor;
      To       : Hexagon.Path.Cursor;
      After_To : Hexagon.Path.Cursor;

      Action_Points : Integer;

      use Piece;
      use Status;
      use Hexagon.Path;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Attack (Path)- enter P_Attacking_Piece_Id=" &
            P_Attacking_Piece_Id'Img &
            " P_Attacked_Piece_Id=" &
            P_Attacked_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      From := Hexagon.Path.First (P_Path);
      To   := Hexagon.Path.Last (P_Path);

      Start_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (Hexagon.Path.Element (From).A,
           Hexagon.Path.Element (From).B);

      End_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (Hexagon.Path.Element (To).A,
           Hexagon.Path.Element (To).B);

      A_Attacking_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id).Actual_Piece);
      A_Attacked_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id).Actual_Piece);

      -- some simple validation
      if not
        (Piece.Server.Is_Piece_Here (Landscape.Type_Patch(Start_Patch.all), Piece.Type_Piece (A_Attacking_Piece.all)) and
         Piece.Server.Is_Piece_Here (Landscape.Type_Patch(End_Patch.all), Piece.Type_Piece (A_Attacked_Piece.all)))
      then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Attacking_Piece.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif A_Attacked_Piece.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Attack
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
           P_Path,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Fighting_Piece.Pre_Validate_Attack
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacked_Piece.all),
            Landscape.Type_Patch (Start_Patch.all),
            Landscape.Type_Patch (End_Patch.all),
            P_Player_Id,
            P_Status);

         if P_Status = Status.Ok then
            Action_Points := A_Attacking_Piece.all.Action_Points;
            if Verbose then
               Text_IO.Put_Line
                 ("Server.Server.Player_Action.Perform_Attack (Path) - validate path - Number_Of_Moves=" &
                  Action_Points'Img);
            end if;

            From := Hexagon.Path.First (P_Path);
            To   := Hexagon.Path.Next (From);

            while (P_Status = Status.Ok and Hexagon.Path.Has_Element (To)) loop

               A_From_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (From).A,
                    Hexagon.Path.Element (From).B);
               A_To_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (To).A,
                    Hexagon.Path.Element (To).B);

               -- If this is alowed it will cost the following action points
               Action_Points :=
                 Action_Points -
                 Piece.Server.Fighting_Piece.Calculate_Attack_Action_Points
                   (P_Action_Type,
                    Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                    Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                    Landscape.Type_Patch (A_From_Patch.all),
                    Landscape.Type_Patch (A_To_Patch.all),
                    P_Player_Id);

               if Verbose then
                  Text_IO.Put_Line
                    ("Server.Server.Player_Action.Perform_Attack (Path) - THE LOOP Action_Points=" &
                     Action_Points'Img);
               end if;

-- only check if patch is occupied on patches leading up to the attecked patch
-- for the attacked patch there are tests elsewhere.
               if To /= Hexagon.Path.Last (P_Path) and
                 not Piece.Server.Fighting_Piece.Validate_Target_Patch_Occupied
                   (Landscape.Type_Patch (A_To_Patch.all),
                    P_Player_Id)
               then
                  P_Status := Status.Target_Patch_Occupied;

               -- Check on all patches in the path (included attacked patch)
               elsif not Piece.Server.Fighting_Piece.Validate_Attack
                   (Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
                    Landscape.Type_Patch (Start_Patch.all),
                    Landscape.Type_Patch (A_To_Patch.all))
               then
                  P_Status := Status.Not_Reachable;
               elsif not Piece.Server.Validate_Target_Patch_And_Piece
                   (Landscape.Type_Patch(A_To_Patch.all),
                    A_Attacking_Piece.all,
                    P_Player_Id)
               then
                  P_Status := Status.Patch_Bad_Terrain;
               elsif Action_Points < 0 then
                  P_Status := Status.Out_Of_Moves;
               end if;

               From := Hexagon.Path.Next (From);
               To   := Hexagon.Path.Next (From);
            end loop;

            if Verbose then
               Text_IO.Put_Line
                 ("Server.Server.Player_Action.Perform_Attack (Path) - validate path - P_Status=" &
                  P_Status'Img);
            end if;

            --everything is ok to execute this attack.
            -- now lets "record it".
            if P_Status = Status.Ok then

               Piece.Server.Fighting_Piece.Calculate_Attack_Result
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                  Landscape.Type_Patch (Start_Patch.all),
                  Landscape.Type_Patch (End_Patch.all),
                  P_Player_Id,
                  P_Winner);

               From     := Hexagon.Path.First (P_Path);
               To       := Hexagon.Path.Next (From);
               After_To := Hexagon.Path.Next (To);

               while Hexagon.Path.Has_Element (After_To) loop
                  A_From_Patch :=
                    Hexagon.Server_Map.Get_Patch_Adress_From_AB
                      (Hexagon.Path.Element (From).A,
                       Hexagon.Path.Element (From).B);
                  A_To_Patch :=
                    Hexagon.Server_Map.Get_Patch_Adress_From_AB
                      (Hexagon.Path.Element (To).A,
                       Hexagon.Path.Element (To).B);

                  Piece.Server.Fighting_Piece.Perform_Move_Step
                    (P_Action_Type,
                     Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                     Landscape.Type_Patch (A_From_Patch.all),
                     Landscape.Type_Patch (A_To_Patch.all),
                     P_Player_Id);

                  A_Attacking_Piece.all.Action_Points :=
                    A_Attacking_Piece.all.Action_Points -
                    Piece.Server.Fighting_Piece.Calculate_Attack_Action_Points
                      (P_Action_Type,
                       Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                       Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                       Landscape.Type_Patch (A_From_Patch.all),
                       Landscape.Type_Patch (A_To_Patch.all),
                       P_Player_Id);

                  Observe_Game (1);

                  From     := Hexagon.Path.Next (From);
                  To       := Hexagon.Path.Next (From);
                  After_To := Hexagon.Path.Next (To);

               end loop;

               -- We are standing in front of the piece we wish to
               --attack, it is in "To"
               A_From_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (From).A,
                    Hexagon.Path.Element (From).B);
               A_To_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (To).A,
                    Hexagon.Path.Element (To).B);

               Piece.Server.Fighting_Piece.Perform_Attack_Step
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                  Landscape.Type_Patch (A_From_Patch.all),
                  Landscape.Type_Patch (A_To_Patch.all),
                  P_Player_Id,
                  P_Winner);

               A_Attacking_Piece.all.Action_Points :=
                 A_Attacking_Piece.all.Action_Points -
                 Piece.Server.Fighting_Piece.Calculate_Attack_Action_Points
                   (P_Action_Type,
                    Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                    Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                    Landscape.Type_Patch (A_From_Patch.all),
                    Landscape.Type_Patch (A_To_Patch.all),
                    P_Player_Id);

               Piece.Server.Fighting_Piece.After_Perform_Attack
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                  Landscape.Type_Patch (A_From_Patch.all),
                  Landscape.Type_Patch (A_To_Patch.all),
                  P_Winner,
                  P_Player_Id);

               Observe_Game (1);
            end if;
         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Attack - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Attack;

   procedure Perform_Attack
     (P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos           : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      A_Attacking_Patch, A_Attacked_Patch --A_From_Patch, A_To_Patch
                                          : Hexagon.Server_Map.Type_Server_Patch_Adress;
      A_Attacking_Piece, A_Attacked_Piece : Piece.Server.Type_Piece_Access_Class;

      Move_Path : Hexagon.Path.Vector;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Perform_Attack (from, to) - enter");
      end if;

      A_Attacking_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id).Actual_Piece);

      A_Attacked_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id).Actual_Piece);

      A_Attacking_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Attacking_Pos.A, P_Attacking_Pos.B);
      A_Attacked_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Attacked_Pos.A, P_Attacked_Pos.B);

      -- some simple validation
      if not
        (Piece.Server.Is_Piece_Here
           (Landscape.Type_Patch(A_Attacking_Patch.all),
            Piece.Type_Piece (A_Attacking_Piece.all)) and
         Piece.Server.Is_Piece_Here (Landscape.Type_Patch(A_Attacked_Patch.all), Piece.Type_Piece (A_Attacked_Piece.all)))
      then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Attacking_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif A_Attacked_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Attack
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
           P_Attacking_Pos,
           P_Attacked_Pos,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      elsif not Piece.Server.Fighting_Piece.Validate_Attack
          (Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
           Landscape.Type_Patch (A_Attacking_Patch.all),
           Landscape.Type_Patch (A_Attacked_Patch.all))
      then
         P_Status := Status.Not_Reachable;
      elsif not Piece.Server.Fighting_Piece.Validate_Target_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (A_Attacked_Piece.all),
           P_Player_Id)
      then
         P_Status := Status.Players_Attacks_Himself;
      else
         Piece.Server.Fighting_Piece.Build_Attack_Steps
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacked_Piece.all),
            Landscape.Type_Patch (A_Attacking_Patch.all),
            Landscape.Type_Patch (A_Attacked_Patch.all),
            P_Player_Id,
            P_Status,
            Move_Path);

         if P_Status = Status.Ok then
            Perform_Attack
              (P_Action_Type,
               A_Attacking_Piece.all.Id,
               A_Attacked_Piece.all.Id,
               Move_Path,
               P_Current_Player_Id,
               P_Player_Id,
               P_Winner,
               P_Status);
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Attack (from, to) - exit P_Status=" &
            P_Status'Img);
      end if;
   end Perform_Attack;

   -- client sends us a particular path
   -- we need to validate it as if we had created it ourselves in the server.
   -- The path we received must be usable for this turn until it is "consumed"
   -- otherwise we will return a failiure
   procedure Perform_Move
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Moving_Piece_Id                : in     Piece.Type_Piece_Id;
      P_Path                           : in     Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_From_Patch,
      A_To_Patch,
      Start_Patch,
      End_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress :=
        null;
      A_Moving_Piece : Piece.Server.Type_Piece_Access_Class := null;

      Action_Points : Integer;

      From : Hexagon.Path.Cursor;
      To   : Hexagon.Path.Cursor;

      use Piece;
      use Status;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Perform_Move (Path) - enter");
      end if;

      P_Status       := Status.Ok;
      A_Moving_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Moving_Piece_Id).Actual_Piece);

      --
      From := Hexagon.Path.First (P_Path);
      To   := Hexagon.Path.Last (P_Path);

      Start_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (Hexagon.Path.Element (From).A,
           Hexagon.Path.Element (From).B);
      End_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (Hexagon.Path.Element (To).A,
           Hexagon.Path.Element (To).B);

      if not Piece.Server.Is_Piece_Here
          (Landscape.Type_Patch(Start_Patch.all),
           Piece.Type_Piece (A_Moving_Piece.all))
      then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Moving_Piece.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Move
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
           P_Path,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Fighting_Piece.Pre_Validate_Perform_Move
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece.all),
            Landscape.Type_Patch (Start_Patch.all),
            Landscape.Type_Patch (End_Patch.all),
            P_Player_Id,
            P_Status);

         if P_Status = Status.Ok then
            Action_Points := A_Moving_Piece.all.Action_Points;

            if Verbose then
               Text_IO.Put_Line
                 ("Server.Server.Player_Action.Perform_Move (Path) - validate path - Number_Of_Moves=" &
                  Action_Points'Img);
            end if;

            From := Hexagon.Path.First (P_Path);
            To   := Hexagon.Path.Next (From);

            while Hexagon.Path.Has_Element (To) and P_Status = Status.Ok loop
               A_From_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (From).A,
                    Hexagon.Path.Element (From).B);
               A_To_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Hexagon.Path.Element (To).A,
                    Hexagon.Path.Element (To).B);

               Action_Points :=
                 Action_Points -
                 Piece.Server.Fighting_Piece.Calculate_Move_Action_Points
                   (P_Action_Type,
                    Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
                    Landscape.Type_Patch (A_From_Patch.all),
                    Landscape.Type_Patch (A_To_Patch.all),
                    P_Player_Id);

               if Verbose then
                  Text_IO.Put_Line
                    ("Server.Server.Player_Action.Perform_Move (Path) - THE LOOP Action_Points=" &
                     Action_Points'Img);
               end if;

               if not Hexagon.Server_Map.Are_Neighbours (A_From_Patch.all, A_To_Patch.all) then

                  P_Status := Status.Patches_Not_Connected;
               elsif not Piece.Server.Fighting_Piece.Validate_Target_Patch_Occupied
                   (Landscape.Type_Patch (A_To_Patch.all),
                    P_Player_Id)
               then
                  P_Status := Status.Target_Patch_Occupied;
               elsif not Piece.Server.Fighting_Piece.Validate_Move
                   (Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece.all),
                    Landscape.Type_Patch (Start_Patch.all), -- based on start of this move
                    Landscape.Type_Patch (A_To_Patch.all))
               then
                  P_Status := Status.Not_Reachable;
               elsif not Piece.Server.Validate_Target_Patch_And_Piece
                   (Landscape.Type_Patch(A_To_Patch.all),
                    A_Moving_Piece.all,
                    P_Player_Id)
               then
                  P_Status := Status.Patch_Bad_Terrain;
               elsif Action_Points < 0 then
                  P_Status := Status.Out_Of_Moves;
               end if;

               From := Hexagon.Path.Next (From);
               To   := Hexagon.Path.Next (From);

            end loop;

            if P_Status = Status.Ok then
               From := Hexagon.Path.First (P_Path);
               To   := Hexagon.Path.Next (From);
               if Verbose then
                  Text_IO.Put_Line
                    ("Server.Server.Player_Action.Perform_Move (Path) - walk path - Number_Of_Moves=" &
                     Action_Points'Img);
               end if;

               while Hexagon.Path.Has_Element (To) and P_Status = Status.Ok loop
                  A_From_Patch :=
                    Hexagon.Server_Map.Get_Patch_Adress_From_AB
                      (Hexagon.Path.Element (From).A,
                       Hexagon.Path.Element (From).B);
                  A_To_Patch :=
                    Hexagon.Server_Map.Get_Patch_Adress_From_AB
                      (Hexagon.Path.Element (To).A,
                       Hexagon.Path.Element (To).B);

                  Piece.Server.Fighting_Piece.Perform_Move_Step
                    (P_Action_Type,
                     Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
                     Landscape.Type_Patch (A_From_Patch.all),
                     Landscape.Type_Patch (A_To_Patch.all),
                     P_Player_Id);

                  A_Moving_Piece.all.Action_Points :=
                    A_Moving_Piece.all.Action_Points -
                    Piece.Server.Fighting_Piece.Calculate_Move_Action_Points
                      (P_Action_Type,
                       Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
                       Landscape.Type_Patch (A_From_Patch.all),
                       Landscape.Type_Patch (A_To_Patch.all),
                       P_Player_Id);

                  Observe_Game (1);

                  From := Hexagon.Path.Next (From);
                  To   := Hexagon.Path.Next (From);
               end loop;

               Player_Activity_Report_Append
                 (Observation.Activity.Internal_Details,
                  P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Your " &
                     A_Moving_Piece.Type_Of_Piece'Img &
                     " moved from (" &
                     Start_Patch.Pos.A'Img &
                     ", " &
                     Start_Patch.Pos.B'Img &
                     ") to  (" &
                     End_Patch.Pos.A'Img &
                     ", " &
                     End_Patch.Pos.B'Img &
                     ") . The piece has " &
                     A_Moving_Piece.all.Action_Points'Img &
                     " left."));

               Piece.Server.Fighting_Piece.After_Perform_Move
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
                  Landscape.Type_Patch (A_From_Patch.all),
                  Landscape.Type_Patch (A_To_Patch.all),
                  P_Player_Id);

               Observe_Game (1);
            end if;
         end if;
      end if; --Consistent parameters

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Move (Path) - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area                           : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Patch       : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Piece       : Piece.Server.Type_Piece_Access_Class        := null;
      Action_Points : Integer;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Patch_Effect - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      Action_Points :=
        A_Piece.all.Action_Points -
        Piece.Server.Fighting_Piece.Calculate_Patch_Effect_Action_Points
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
           Landscape.Type_Patch (A_Patch.all),
           P_Effect,
           P_Area,
           P_Player_Id);

      if not Piece.Server.Is_Piece_Here (Landscape.Type_Patch(A_Patch.all), Piece.Type_Piece (A_Piece.all)) then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Patch_Effect
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
           P_Pos,
           P_Effect,
           P_Area,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      elsif not Piece.Server.Validate_Perform_Patch_Effect_Piece
          (Piece.Server.Type_Piece (A_Piece.all),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Validate_Patch_Effect
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece (A_Piece.all),
           Landscape.Type_Patch (A_Patch.all),
           P_Effect,
           P_Area)
      then
         P_Status := Status.Patch_Effect_Not_Here;
      elsif Action_Points < 0 then
         P_Status := Status.Out_Of_Moves;
      else
         A_Piece.all.Action_Points := Action_Points;

         Piece.Server.Fighting_Piece.Calculate_Patch_Effect
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
            Landscape.Type_Patch (A_Patch.all),
            P_Effect,
            P_Area,
            P_Player_Id);

         -- Game code can deal with "after_perform_patch_effect" by itself.
         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Patch_Effect - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Patch       : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Piece       : Piece.Server.Type_Piece_Access_Class        := null;
      Action_Points : Integer;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Piece_Effect - enter P_Piece_Id=" &
            P_Piece_Id'Img);
      end if;
      P_Status := Status.Ok;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      Action_Points :=
        A_Piece.all.Action_Points -
        Piece.Server.Fighting_Piece.Calculate_Piece_Effect_Action_Points
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
           Landscape.Type_Patch (A_Patch.all),
           P_Effect,
           P_Player_Id);

      if not Piece.Server.Is_Piece_Here (Landscape.Type_Patch(A_Patch.all), Piece.Type_Piece (A_Piece.all)) then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Piece_Effect
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
           P_Pos,
           P_Effect,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      elsif not Piece.Server.Validate_Piece_Effect
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece (A_Piece.all),
           P_Effect)
      then
         P_Status := Status.Piece_Effect_Not_Here;
      elsif Action_Points < 0 then
         P_Status := Status.Out_Of_Moves;
      else
         A_Piece.all.Action_Points := Action_Points;

         Piece.Server.Fighting_Piece.Calculate_Piece_Effect
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece'Class (A_Piece.all),
            Landscape.Type_Patch (A_Patch.all),
            P_Effect,
            P_Player_Id);

         -- Game code can deal with "after_perform_piece_effect" by itself.
         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Piece_Effect - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Piece_Effect;

-- The from-to we received must be usable for this turn until it is "consumed"
-- otherwise we will return a failiure
   procedure Perform_Move
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Moving_Piece_Id                : in     Piece.Type_Piece_Id;
      P_From_Pos, P_To_Pos             : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_From_Patch, A_To_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Moving_Piece           : Piece.Server.Type_Piece_Access_Class        := null;
      Move_Path                : Hexagon.Path.Vector;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Move (from,to)- enter P_Moving_Piece_Id=" &
            P_Moving_Piece_Id'Img);
      end if;

      P_Status := Status.Ok;

      A_Moving_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Moving_Piece_Id).Actual_Piece);

      A_From_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_From_Pos.A, P_From_Pos.B);
      A_To_Patch   := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_To_Pos.A, P_To_Pos.B);

      if not Piece.Server.Is_Piece_Here
          (Landscape.Type_Patch(A_From_Patch.all),
           Piece.Type_Piece (A_Moving_Piece.all))
      then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Moving_Piece.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Move
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Moving_Piece.all),
           P_From_Pos,
           P_To_Pos,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Fighting_Piece.Pre_Validate_Perform_Move
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece.all),
            Landscape.Type_Patch (A_From_Patch.all),
            Landscape.Type_Patch (A_To_Patch.all),
            P_Player_Id,
            P_Status);

         if P_Status = Status.Ok then
            Piece.Server.Fighting_Piece.Build_Move_Steps
              (P_Action_Type,
               Piece.Server.Fighting_Piece.Type_Piece (A_Moving_Piece.all),
               Landscape.Type_Patch (A_From_Patch.all),
               Landscape.Type_Patch (A_To_Patch.all),
               P_Player_Id,
               P_Status,
               Move_Path);

            if P_Status = Status.Ok then
               Perform_Move
                 (P_Action_Type,
                  A_Moving_Piece.all.Id,
                  Move_Path,
                  P_Current_Player_Id,
                  P_Player_Id,
                  P_Status);
            end if;
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Move (from,to)- exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Move;

   procedure Perform_Ranged_Attack
     (P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos           : in     Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id          : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status)
   is
      A_Attacking_Patch, A_Attacked_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      A_Attacking_Piece, A_Attacked_Piece : Piece.Server.Type_Piece_Access_Class;

      Action_Points : Integer;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Perform_Ranged_Attack - enter");
      end if;
      P_Status := Status.Ok;

      A_Attacking_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Attacking_Pos.A, P_Attacking_Pos.B);
      A_Attacked_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Attacked_Pos.A, P_Attacked_Pos.B);

      A_Attacking_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacking_Piece_Id).Actual_Piece);
      A_Attacked_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Attacked_Piece_Id).Actual_Piece);

      -- some simple validation
      if not
        (Piece.Server.Is_Piece_Here
           (Landscape.Type_Patch(A_Attacking_Patch.all),
            Piece.Type_Piece (A_Attacking_Piece.all)) and
         Piece.Server.Is_Piece_Here (Landscape.Type_Patch(A_Attacked_Patch.all), Piece.Type_Piece (A_Attacked_Piece.all)))
      then
         P_Status := Status.Inconsistent_Parameters;
      elsif A_Attacking_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif A_Attacked_Piece.all.Category /= Piece.Fighting_Piece then
         P_Status := Status.Expected_Fighting_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Perform_Ranged_Attack
          (P_Action_Type,
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
           Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
           P_Attacking_Pos,
           P_Attacked_Pos,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Fighting_Piece.Pre_Validate_Ranged_Attack
           (P_Action_Type,
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
            Piece.Server.Fighting_Piece.Type_Piece (A_Attacked_Piece.all),
            Landscape.Type_Patch (A_Attacking_Patch.all),
            Landscape.Type_Patch (A_Attacked_Patch.all),
            P_Player_Id,
            P_Status);

         if P_Status = Status.Ok then
            Action_Points :=
              Piece.Server.Fighting_Piece.Calculate_Ranged_Attack_Action_Points
                (P_Action_Type,
                 Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                 Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                 Landscape.Type_Patch (A_Attacking_Patch.all),
                 Landscape.Type_Patch (A_Attacked_Patch.all),
                 P_Player_Id);

            if A_Attacking_Piece.all.Action_Points - Action_Points < 0 then
               P_Status := Status.Out_Of_Moves;
            else
               A_Attacking_Piece.all.Action_Points :=
                 A_Attacking_Piece.all.Action_Points - Action_Points;

               Piece.Server.Fighting_Piece.Calculate_Attack_Result
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                  Landscape.Type_Patch (A_Attacking_Patch.all),
                  Landscape.Type_Patch (A_Attacked_Patch.all),
                  P_Player_Id,
                  P_Winner);

               Piece.Server.Fighting_Piece.Perform_Ranged_Attack
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece (A_Attacked_Piece.all),
                  Landscape.Type_Patch (A_Attacking_Patch.all),
                  Landscape.Type_Patch (A_Attacked_Patch.all),
                  P_Player_Id,
                  P_Winner);

               Piece.Server.Fighting_Piece.After_Perform_Ranged_Attack
                 (P_Action_Type,
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacking_Piece.all),
                  Piece.Server.Fighting_Piece.Type_Piece'Class (A_Attacked_Piece.all),
                  Landscape.Type_Patch (A_Attacking_Patch.all),
                  Landscape.Type_Patch (A_Attacked_Patch.all),
                  P_Winner,
                  P_Player_Id);

               Observe_Game (1);
            end if;
         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Ranged_Attack - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Construction
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Constructing_Piece_Id          : in     Piece.Type_Piece_Id;
      P_Piece_Pos                      : in     Hexagon.Type_Hexagon_Position;
      P_Construction_Pos               : in     Hexagon.Type_Hexagon_Position;
      P_Construction                   : in     Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece_Patch        : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Construction_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Construction_Piece : Piece.Server.Type_Piece_Access_Class        := null;

      Action_Points : Integer;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Construction - enter P_Piece_Pos.A=" &
            P_Piece_Pos.A'Img &
            " P_Piece_Pos.B=" &
            P_Piece_Pos.B'Img &
            " P_Construction_Pos.A,B=" &
            P_Construction_Pos.A'Img &
            "," &
            P_Construction_Pos.B'Img &
            " P_Construction=" &
            P_Construction'Img);
      end if;

      P_Status := Status.Ok;

      A_Piece_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Piece_Pos.A, P_Piece_Pos.B);
      A_Construction_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Construction_Pos.A, P_Construction_Pos.B);

      A_Construction_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Constructing_Piece_Id).Actual_Piece);

      if A_Construction_Piece.Category /= Piece.House_Piece then
         P_Status := Status.Expected_House_Piece;
      elsif not Piece.Server.House_Piece.Validate_Perform_Construction
          (P_Action_Type,
           Piece.Server.House_Piece.Type_House'Class (A_Construction_Piece.all),
           P_Piece_Pos,
           P_Construction_Pos,
           P_Construction,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.House_Piece.Pre_Validate_Perform_Construction_Or_Demolition
           (P_Action_Type,
            Piece.Server.House_Piece.Type_House (A_Construction_Piece.all),
            Landscape.Type_Patch (A_Piece_Patch.all),
            Landscape.Type_Patch (A_Construction_Patch.all),
            P_Construction,
            P_Player_Id,
            Piece.Server.House_Piece.Construction_Mode,
            P_Status);

         if P_Status = Status.Ok then

            Action_Points :=
              Piece.Server.House_Piece.Calculate_Construction_Action_Points
                (P_Action_Type,
                 Piece.Server.House_Piece.Type_House'Class (A_Construction_Piece.all),
                 Landscape.Type_Patch (A_Piece_Patch.all),
                 Landscape.Type_Patch (A_Construction_Patch.all),
                 P_Construction,
                 P_Player_Id);

            if A_Construction_Piece.all.Action_Points - Action_Points < 0 then
               P_Status := Status.Out_Of_Moves;
            else
               Construction.Construction_List.Include
                 (A_Construction_Patch.all.Constructions_Here,
                  P_Construction);

               A_Construction_Piece.all.Action_Points :=
                 A_Construction_Piece.all.Action_Points - Action_Points;

               Player_Activity_Report_Append
                 (Observation.Activity.Internal_Details,
                  P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("You built " &
                     P_Construction'Img &
                     " on " &
                     P_Construction_Pos.A'Img &
                     ", " &
                     P_Construction_Pos.B'Img));

               Piece.Server.House_Piece.After_Perform_Construction
                 (P_Action_Type,
                  Piece.Server.House_Piece.Type_House'Class (A_Construction_Piece.all),
                  Landscape.Type_Patch (A_Piece_Patch.all),
                  Landscape.Type_Patch (A_Construction_Patch.all),
                  P_Construction,
                  P_Player_Id);

               Observe_Game (1);
            end if;
         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Construction - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Construction;

   procedure Perform_Demolition
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Demolition_Piece_Id            : in     Piece.Type_Piece_Id;
      P_Piece_Pos                      : in     Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos                 : in     Hexagon.Type_Hexagon_Position;
      P_Construction                   : in     Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)

   is
      A_Piece_Patch      : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Demolition_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Demolition_Piece : Piece.Server.Type_Piece_Access_Class        := null;

      Action_Points : Integer;

      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Demolition - enter P_Piece_Pos.A=" &
            P_Piece_Pos.A'Img &
            " P_Piece_Pos.B=" &
            P_Piece_Pos.B'Img &
            " P_Demolition_Pos.A,B=" &
            P_Demolition_Pos.A'Img &
            "," &
            P_Demolition_Pos.B'Img &
            " P_Demolition_Pos=" &
            P_Construction'Img);
      end if;

      P_Status := Status.Ok;

      A_Piece_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Piece_Pos.A, P_Piece_Pos.B);
      A_Demolition_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Demolition_Pos.A, P_Demolition_Pos.B);

      A_Demolition_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Demolition_Piece_Id).Actual_Piece);

      if A_Demolition_Piece.Category /= Piece.House_Piece then
         P_Status := Status.Expected_House_Piece;
      elsif not Piece.Server.House_Piece.Validate_Perform_Demolition
          (P_Action_Type,
           Piece.Server.House_Piece.Type_House'Class (A_Demolition_Piece.all),
           P_Piece_Pos,
           P_Demolition_Pos,
           P_Construction,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.House_Piece.Pre_Validate_Perform_Construction_Or_Demolition
           (P_Action_Type,
            Piece.Server.House_Piece.Type_House (A_Demolition_Piece.all),
            Landscape.Type_Patch (A_Piece_Patch.all),
            Landscape.Type_Patch (A_Demolition_Patch.all),
            P_Construction,
            P_Player_Id,
            Piece.Server.House_Piece.Demolition_Mode,
            P_Status);

         if P_Status = Status.Ok then

            Action_Points :=
              Piece.Server.House_Piece.Calculate_Demolition_Action_Points
                (P_Action_Type,
                 Piece.Server.House_Piece.Type_House'Class (A_Demolition_Piece.all),
                 Landscape.Type_Patch (A_Piece_Patch.all),
                 Landscape.Type_Patch (A_Demolition_Patch.all),
                 P_Construction,
                 P_Player_Id);

            if A_Demolition_Piece.all.Action_Points - Action_Points < 0 then
               P_Status := Status.Out_Of_Moves;
            else
               Construction.Construction_List.Exclude
                 (A_Demolition_Patch.all.Constructions_Here,
                  P_Construction);

               A_Demolition_Piece.all.Action_Points :=
                 A_Demolition_Piece.all.Action_Points - Action_Points;

               Player_Activity_Report_Append
                 (Observation.Activity.Internal_Details,
                  P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("You demolished " &
                     P_Construction'Img &
                     " at " &
                     P_Demolition_Pos.A'Img &
                     ", " &
                     P_Demolition_Pos.B'Img));

               Piece.Server.House_Piece.After_Perform_Demolition
                 (P_Action_Type,
                  Piece.Server.House_Piece.Type_House'Class (A_Demolition_Piece.all),
                  Landscape.Type_Patch (A_Piece_Patch.all),
                  Landscape.Type_Patch (A_Demolition_Patch.all),
                  P_Construction,
                  P_Player_Id);

               Observe_Game (1);
            end if;
         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Perform_Demolition - exit P_Status=" & P_Status'Img);
      end if;
   end Perform_Demolition;

   procedure Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Grant_Piece_Effect- enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      if not Piece.Server.Validate_Grant_Piece_Effect
          (P_Action_Type,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           P_Effect,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Grant_Piece_Effect
           (P_Action_Type,
            A_Piece.all,
            P_Effect,
            P_Player_Id,
            P_Status);
      end if;

      if P_Status = Status.Ok then
         Player_Activity_Report_Append
           (Observation.Activity.Internal_Details,
            P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Your " &
               A_Piece.Type_Of_Piece'Img &
               " were granted the effect " &
               P_Effect.Effect_Name'Img &
               " with value " &
               P_Effect.Aux'Img));

         Piece.Server.After_Grant_Piece_Effect
           (P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece.all),
            P_Effect,
            P_Current_Player_Id,
            P_Player_Id);

         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Grant_Piece_Effect- exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class := null;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Revoke_Piece_Effect- enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      if not Piece.Server.Validate_Revoke_Piece_Effect
          (P_Action_Type,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           P_Effect,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Revoke_Piece_Effect
           (P_Action_Type,
            A_Piece.all,
            P_Effect,
            P_Player_Id,
            P_Status);
      end if;

      if P_Status = Status.Ok then
         Player_Activity_Report_Append
           (Observation.Activity.Internal_Details,
            P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Your " &
               A_Piece.Type_Of_Piece'Img &
               " were revoked the effect " &
               P_Effect.Effect_Name'Img));

         Piece.Server.After_Revoke_Piece_Effect
           (P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece.all),
            P_Effect,
            P_Current_Player_Id,
            P_Player_Id);

         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Revoke_Piece_Effect- exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area                           : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class := null;
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Grant_Patch_Effect- enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if not Piece.Server.Validate_Grant_Patch_Effect
          (P_Action_Type,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           A_Patch.all,
           P_Effect,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Grant_Patch_Effect
           (P_Action_Type,
            A_Piece.all,
            A_Patch.all,
            P_Effect,
            P_Area,
            P_Player_Id,
            P_Status);
      end if;

      if P_Status = Status.Ok then
         Player_Activity_Report_Append
           (Observation.Activity.Internal_Details,
            P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Your " &
               A_Piece.Type_Of_Piece'Img &
               " granted the effect " &
               P_Effect.Effect_Name'Img &
               " with value " &
               P_Effect.Aux'Img));

         Piece.Server.After_Grant_Patch_Effect
           (P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece.all),
            A_Patch.all,
            P_Effect,
            P_Current_Player_Id,
            P_Player_Id);

         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Grant_Patch_Effect- exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece_Id                       : in     Piece.Type_Piece_Id;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Effect                         : in     Effect.Type_Effect;
      P_Area                           : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id;
      P_Status                         :    out Status.Type_Status)
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class := null;
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Revoke_Patch_Effect- enter");
      end if;

      A_Piece :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if not Piece.Server.Validate_Revoke_Patch_Effect
          (P_Action_Type,
           Piece.Server.Type_Piece'Class (A_Piece.all),
           A_Patch.all,
           P_Effect,
           P_Current_Player_Id,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Turn;
      else
         Piece.Server.Revoke_Patch_Effect
           (P_Action_Type,
            A_Piece.all,
            A_Patch.all,
            P_Effect,
            P_Area,
            P_Player_Id,
            P_Status);
      end if;

      if P_Status = Status.Ok then
         Player_Activity_Report_Append
           (Observation.Activity.Internal_Details,
            P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Your " &
               A_Piece.Type_Of_Piece'Img &
               " revoked the effect " &
               P_Effect.Effect_Name'Img));

         Piece.Server.After_Revoke_Patch_Effect
           (P_Action_Type,
            Piece.Server.Type_Piece'Class (A_Piece.all),
            A_Patch.all,
            P_Effect,
            P_Current_Player_Id,
            P_Player_Id);

         Observe_Game (1);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean is
      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.End_Turn - enter Player_Id=" & P_Player_Id'Img);
      end if;

      Game_Engine.Entry_End_Turn (P_Player_Id, Ret);

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.End_Turn - exit");
      end if;

      return Ret;
   end End_Turn;

   -- Information providers.
   function Observation_Area
     (P_Piece_Id : in Piece.Type_Piece_Id) return Hexagon.Area.Type_Action_Capabilities
   is
      Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Observation_Area - enter");
      end if;

      Game_Engine.Entry_Observation_Area (P_Piece_Id, Tmp);

      declare
         Ret : Hexagon.Area.Type_Action_Capabilities := (Tmp.all);
      begin
         -- Free Tmp!!
         if Verbose then
            Text_IO.Put_Line ("Server.Server.Player_Action.Observation_Area - exit");
         end if;
         return Ret;
      end;

   end Observation_Area;

   function Movement_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      A_Piece_Class           : Piece.Server.Type_Piece_Access_Class;
      Ret_Movement_Capability : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;
      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Movement_Capability - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      A_Piece_Class :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      if A_Piece_Class.all in Piece.Server.Fighting_Piece.Type_Piece'Class then
         Ret_Movement_Capability :=
           Piece.Server.Fighting_Piece.Movement_Terrain_Capability
             (Piece.Server.Fighting_Piece.Type_Piece_Access_Class (A_Piece_Class).all);
      else
         Ret_Movement_Capability := null;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Movement_Capability - exit");
      end if;
      return Ret_Movement_Capability;
   end Movement_Capability;

   function Attack_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      A_Piece_Class         : Piece.Server.Type_Piece_Access_Class;
      Ret_Attack_Capability : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Server.Player_Action.Attack_Capability - enter P_Piece_Id=" & P_Piece_Id'Img);
      end if;

      A_Piece_Class :=
        Piece.Server.Type_Piece_Access_Class
          (Piece.Server.Find_Piece_In_List (P_Piece_Id).Actual_Piece);

      if A_Piece_Class.all in Piece.Server.Fighting_Piece.Type_Piece'Class then
         Ret_Attack_Capability :=
           Piece.Server.Fighting_Piece.Attack_Terrain_Capability
             (Piece.Server.Fighting_Piece.Type_Piece_Access_Class (A_Piece_Class).all);
      else
         Ret_Attack_Capability := null;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Server.Server.Player_Action.Attack_Capability - exit");
      end if;
      return Ret_Attack_Capability;
   end Attack_Capability;

end Server.Server.Player_Action;
