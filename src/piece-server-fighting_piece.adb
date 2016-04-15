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
with Hexagon.Utility;

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
      elsif P_Fighting_Piece_Class.Type_Of_Piece /=
        Piece.Undefined_Piece_Type
      then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_Fighting_Piece_Class.Player_Id /= Player.Undefined_Player_Id then
         raise Init_Not_Undefined_Player_Id;
      end if;

      Piece_Class :=
        new Piece.Server.Type_Piece'Class'(P_Fighting_Piece_Class);
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
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean
   is
   begin
      return Piece.Server.Fighting_Piece.Piece_Type_Info_List (P_Type_Of_Piece)
          .Attack_Landscape
          (P_Landscape);
   end Can_Attack_Here;

   function Can_Move_Here
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean
   is
   begin
      return Piece.Server.Fighting_Piece.Piece_Type_Info_List (P_Type_Of_Piece)
          .Move_Landscape
          (P_Landscape);
   end Can_Move_Here;

   function Movement_Terrain_Capability
     (P_Piece : in Type_Piece'Class)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      All_Movement_Capability,
      With_Terrain_Movement_Capability : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access :=
        null;
      A_Pos : Hexagon.Type_Hexagon_Position;
      Pieces_Patch,
      Examine_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress :=
        null;
      Count_Patches : Integer;

      use Landscape;
      use Piece;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Movement_Terrain_Capability - enter P_Piece.Id=" &
            P_Piece.Id'Img);
      end if;

      -- Find the movements that are possible by this piece, as described by the client-developer.
      All_Movement_Capability :=
        Piece.Server.Fighting_Piece.Movement_Capability (P_Piece);
      A_Pos        := Piece.Server.Find_Piece_In_List (P_Piece.Id).Actual_Pos;
      Pieces_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (A_Pos.A, A_Pos.B);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Movement_Terrain_Capability - middle All_Movement_Capability'Length=" &
            All_Movement_Capability'Length'Img &
            " Pos of piece to examine A=" &
            A_Pos.A'Img &
            " B=" &
            A_Pos.B'Img);
      end if;

      -- Count the number of positions that are available - to be used to allocate a correct array
      --later
      Count_Patches := 0;
      for Index in
        All_Movement_Capability'First .. All_Movement_Capability'Last
      loop

         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Movement_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Movement_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);
            if Piece.Server.Fighting_Piece.Can_Move_Here
                (P_Piece.Type_Of_Piece,
                 Examine_Patch.all.Landscape_Here)
            then
               Count_Patches := Count_Patches + 1;
            end if;
         exception
            when others =>
               null;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Movement_Terrain_Capability - middle Count_Patches=" &
            Count_Patches'Img);
      end if;

      -- Allocate the array
      With_Terrain_Movement_Capability :=
        new Hexagon.Area.Type_Action_Capabilities (1 .. Count_Patches);

      -- we populate the new array with only the positions that are available.
      Count_Patches := 1;
      for Index in
        All_Movement_Capability'First .. All_Movement_Capability'Last
      loop
         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Movement_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Movement_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);
            if Piece.Server.Fighting_Piece.Can_Move_Here
                (P_Piece.Type_Of_Piece,
                 Examine_Patch.all.Landscape_Here)
            then
               With_Terrain_Movement_Capability (Count_Patches) :=
                 All_Movement_Capability (Index);
               Count_Patches := Count_Patches + 1;
            end if;

         exception
            when others =>
               null;
         end;

      end loop;
      Hexagon.Area.Server_Area.Free_Action_Capabilities
        (All_Movement_Capability);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Movement_Terrain_Capability - exit");
      end if;

      return With_Terrain_Movement_Capability;
   end Movement_Terrain_Capability;

   function Attack_Terrain_Capability
     (P_Piece : in Type_Piece'Class)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      All_Attack_Capability,
      With_Terrain_Attack_Capability : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access :=
        null;
      A_Pos : Hexagon.Type_Hexagon_Position;
      Pieces_Patch,
      Examine_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress :=
        null;

      Count_Patches : Integer;
      use Landscape;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Attack_Terrain_Capability - enter");
      end if;

      -- Find the movements that are possible by this piece, as described by the client-developer.
      All_Attack_Capability :=
        Piece.Server.Fighting_Piece.Attack_Capability (P_Piece);
      A_Pos        := Piece.Server.Find_Piece_In_List (P_Piece.Id).Actual_Pos;
      Pieces_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (A_Pos.A, A_Pos.B);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Attack_Terrain_Capability - middle 1");
      end if;

      -- Count the number of positions that are available - to be used to allocate a correct array
      --later
      Count_Patches := 0;
      for Index in All_Attack_Capability'First .. All_Attack_Capability'Last
      loop

         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Attack_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Attack_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);

            if Piece.Server.Fighting_Piece.Can_Attack_Here
                (P_Piece.Type_Of_Piece,
                 Examine_Patch.all.Landscape_Here)
            then
               Count_Patches := Count_Patches + 1;
            end if;

         exception
            when others =>
               null;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Attack_Terrain_Capability - middle 2");
      end if;

      -- Allocate the array
      With_Terrain_Attack_Capability :=
        new Hexagon.Area.Type_Action_Capabilities (1 .. Count_Patches);

      -- we populate the new array with only the positions that are available.
      Count_Patches := 1;
      for Index in All_Attack_Capability'First .. All_Attack_Capability'Last
      loop
         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Attack_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Attack_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);

            if Piece.Server.Fighting_Piece.Can_Attack_Here
                (P_Piece.Type_Of_Piece,
                 Examine_Patch.all.Landscape_Here)
            then
               With_Terrain_Attack_Capability (Count_Patches) :=
                 All_Attack_Capability (Index);
               Count_Patches := Count_Patches + 1;
            end if;

         exception
            when others =>
               null;
         end;

      end loop;
      Hexagon.Area.Server_Area.Free_Action_Capabilities
        (All_Attack_Capability);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Attack_Terrain_Capability - exit");
      end if;

      return With_Terrain_Attack_Capability;
   end Attack_Terrain_Capability;

   function Validate_Moving_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Moving_Piece - enter P_Piece.Player_Id=" &
            P_Piece.Player_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      -- Player can only move his own piece
      if P_Piece.Player_Id /= P_Player_Id then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Moving_Piece - exit");
      end if;

      return Ret;
   end Validate_Moving_Piece;

   function Validate_Target_Patch_Occupied
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Target_Patch_Occupied - enter");
      end if;

      -- Must not be a piece there
      if not
        (Landscape.Server.Has_Patch_Free_Slot
           (P_Patch) and
         Piece.Server.Patch_Belongs_To_Player (P_Patch, P_Player_Id))
      then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Target_Patch_Occupied - exit, status=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Target_Patch_Occupied;

   function Validate_Executing_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Executing_Piece - enter");
      end if;

      -- Player can only move his own piece
      if P_Piece.Player_Id /= P_Player_Id then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Executing_Piece - exit=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Executing_Piece;

   function Validate_Target_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Target_Piece - enter");
      end if;

      -- Player can only attack another players piece
      if P_Piece.Player_Id = P_Player_Id then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Target_Piece - exit=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Target_Piece;

   function Validate_Reachable
     (P_Moving_Piece : in Type_Piece'Class) return Boolean
   is
      Reachable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := True;

      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Reachable - enter");
      end if;

      Reachable_Patches :=
        Piece.Server.Fighting_Piece.Movement_Terrain_Capability
          (P_Moving_Piece);
      if Reachable_Patches = null then
         Ret_Status := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Reachable - exit");
      end if;

      return Ret_Status;
   end Validate_Reachable;

   function Validate_Move
     (P_Moving_Piece           : in Type_Piece'Class;
      P_From_Patch, P_To_Patch : in Landscape.Type_Patch) return Boolean
   is
      Reachable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := False;
      use Hexagon;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Move - enter From A=" &
            P_From_Patch.Pos.A'Img &
            " B=" &
            P_From_Patch.Pos.B'Img &
            " To A=" &
            P_To_Patch.Pos.A'Img &
            " B=" &
            P_To_Patch.Pos.B'Img &
            " P_Moving_Piece.Type_Of_Piece=" &
            P_Moving_Piece.Type_Of_Piece'Img);
      end if;

      Reachable_Patches :=
        Piece.Server.Fighting_Piece.Movement_Capability (P_Moving_Piece);
      for Index in Reachable_Patches'First .. Reachable_Patches'Last loop
         if Verbose then
            Text_IO.Put_Line
              ("Piece.Server.Fighting_Piece.Validate_Move - Reachable_Patches (Index).A=" &
               Reachable_Patches (Index).A'Img &
               " Reachable_Patches (Index).B=" &
               Reachable_Patches (Index).B'Img);
         end if;
         begin
            if Hexagon.Type_Hexagon_Numbers
                (Integer (P_From_Patch.Pos.A) +
                 Integer (Reachable_Patches (Index).A)) =
              P_To_Patch.Pos.A and
              Hexagon.Type_Hexagon_Numbers
                  (Integer (P_From_Patch.Pos.B) +
                   Integer (Reachable_Patches (Index).B)) =
                P_To_Patch.Pos.B
            then
               Ret_Status := True;
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.Fighting_Piece.Validate_Move - ok A=" &
                     Reachable_Patches (Index).A'Img &
                     " B=" &
                     Reachable_Patches (Index).B'Img);
               end if;
            end if;
         exception
            when others =>
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.Fighting_Piece.validate exception, exception. probably outside map");
               end if;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Move - exit Status=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Move;

   procedure Pre_Validate_Perform_Move
     (P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece :        Type_Piece;
      P_Start_Patch  : in     Landscape.Type_Patch;
      P_End_Patch    :        Landscape.Type_Patch;
      P_Player_Id    : in     Player.Type_Player_Id;
      P_Status       :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Perform_Move - enter");
      end if;
      P_Status := Status.Ok;

      if not Piece.Server.Fighting_Piece.Validate_Moving_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (P_Moving_Piece),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Reachable
          (Piece.Server.Fighting_Piece.Type_Piece (P_Moving_Piece))
      then
         P_Status := Status.No_Movement_Capability;
      elsif not Validate_Target_Patch_Occupied (P_End_Patch, P_Player_Id) then
         P_Status := Status.Target_Patch_Occupied;
      elsif not Piece.Server.Fighting_Piece.Validate_Move
          (Piece.Server.Fighting_Piece.Type_Piece (P_Moving_Piece),
           Landscape.Type_Patch (P_Start_Patch),
           Landscape.Type_Patch (P_End_Patch))
      then
         P_Status := Status.Not_Reachable;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Perform_Move - exit P_Status=" &
            P_Status'Img);
      end if;
   end Pre_Validate_Perform_Move;

   function Validate_Attackable
     (P_Moving_Piece : in Type_Piece'Class) return Boolean
   is
      Attackable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := True;

      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Attackable - enter");
      end if;

      Attackable_Patches :=
        Piece.Server.Fighting_Piece.Attack_Terrain_Capability (P_Moving_Piece);
      if Attackable_Patches = null then
         Ret_Status := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Attackable - exit=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Attackable;

   function Validate_Attack
     (P_Attacking_Piece : in Type_Piece'Class;
      P_Attacking_Patch,
      P_Attacked_Patch : in Landscape.Type_Patch)
      return Boolean
   is
      Attackable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := False;

      use Hexagon;
      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Piece.Server.Fighting_Piece.Validate_Attack - enter");
      end if;

      Attackable_Patches := Attack_Capability (P_Attacking_Piece);
      for Index in Attackable_Patches'First .. Attackable_Patches'Last loop
         begin
            if Hexagon.Type_Hexagon_Numbers
                (Integer (P_Attacking_Patch.Pos.A) +
                 Integer (Attackable_Patches (Index).A)) =
              P_Attacked_Patch.Pos.A and
              Hexagon.Type_Hexagon_Numbers
                  (Integer (P_Attacking_Patch.Pos.B) +
                   Integer (Attackable_Patches (Index).B)) =
                P_Attacked_Patch.Pos.B
            then
               Ret_Status := True;
            end if;
         exception
            when others =>
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.Fighting_Piece.Validate_Attack -exception");
               end if;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Attack - exit Status=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Attack;

   function Validate_Ranged_Attack
     (P_Attacking_Piece : in Type_Piece'Class;
      P_Attacking_Patch,
      P_Attacked_Patch : in Landscape.Type_Patch)
      return Boolean
   is
      Attackable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := False;

      use Hexagon;
      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Ranged_Attack - enter");
      end if;

      Attackable_Patches := Attack_Capability (P_Attacking_Piece);
      for Index in Attackable_Patches'First .. Attackable_Patches'Last loop
         begin
            if Hexagon.Type_Hexagon_Numbers
                (Integer (P_Attacking_Patch.Pos.A) +
                 Integer (Attackable_Patches (Index).A)) =
              P_Attacked_Patch.Pos.A and
              Hexagon.Type_Hexagon_Numbers
                  (Integer (P_Attacking_Patch.Pos.B) +
                   Integer (Attackable_Patches (Index).B)) =
                P_Attacked_Patch.Pos.B
            then
               Ret_Status := True;
            end if;
         exception
            when others =>
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.Fighting_Piece.Validate_Ranged_Attack -exception");
               end if;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Validate_Ranged_Attack - exit Status=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Ranged_Attack;

   procedure Pre_Validate_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece;
      P_Start_Patch                       : in     Landscape.Type_Patch;
      P_End_Patch                         :        Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Attack - enter");
      end if;
      P_Status := Status.Ok;

      if not Piece.Server.Fighting_Piece.Validate_Executing_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Target_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacked_Piece),
           P_Player_Id)
      then
         P_Status := Status.Players_Attacks_Himself;
      elsif not Piece.Server.Fighting_Piece.Validate_Attackable
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece))
      then
         P_Status := Status.No_Attack_Capability;
      elsif not Piece.Server.Fighting_Piece.Validate_Attack
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece),
           Landscape.Type_Patch (P_Start_Patch),
           Landscape.Type_Patch (P_End_Patch))
      then
         P_Status := Status.No_Attack_Capability;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Attack - exit P_Status=" &
            P_Status'Img);
      end if;
   end Pre_Validate_Attack;

   procedure Pre_Validate_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece;
      P_Start_Patch                       : in     Landscape.Type_Patch;
      P_End_Patch                         :        Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Ranged_Attack - enter");
      end if;
      P_Status := Status.Ok;

      if not Piece.Server.Fighting_Piece.Validate_Executing_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Fighting_Piece.Validate_Target_Piece
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacked_Piece),
           P_Player_Id)
      then
         P_Status := Status.Players_Attacks_Himself;
      elsif not Piece.Server.Fighting_Piece.Validate_Attackable
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece))
      then
         P_Status := Status.No_Attack_Capability;
      elsif not Piece.Server.Fighting_Piece.Validate_Ranged_Attack
          (Piece.Server.Fighting_Piece.Type_Piece (P_Attacking_Piece),
           Landscape.Type_Patch (P_Start_Patch),
           Landscape.Type_Patch (P_End_Patch))
      then
         P_Status := Status.No_Attack_Capability;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Pre_Validate_Ranged_Attack - exit P_Status=" &
            P_Status'Img);
      end if;
   end Pre_Validate_Ranged_Attack;

   procedure Build_Attack_Steps
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_Piece'Class;
      P_Attacking_Patch, P_Attacked_Patch : in     Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status;
      P_Path                              :    out Hexagon.Path.Vector)
   is
      Attackable : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Build_Attack_Steps - enter");
      end if;

      Hexagon.Path.Clear (P_Path);

      Attackable := Attack_Terrain_Capability (P_Attacking_Piece);

      Hexagon.Utility.Find_Accurate_Path
        (P_Player_Id,
         P_Attacking_Patch.Pos.A,
         P_Attacking_Patch.Pos.B,
         P_Attacked_Patch.Pos.A,
         P_Attacked_Patch.Pos.B,
         Attackable,
         Hexagon.Utility.Attack,
         P_Status,
         P_Path);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Build_Attack_Steps - exit");
      end if;
   end Build_Attack_Steps;

   procedure Perform_Attack_Step
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            : in     Player.Type_Player_Id)
   is
      Ret_Status : Status.Type_Status;
      A_Patch    : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Attack_Step - enter");
      end if;

      if P_Winner = P_Attacked_Piece.Player_Id then
         -- Attacked Piece won
         -- remove loosing piece, winner stays on his position
         Remove_Piece
           (P_Action_Type,
            P_From_Patch,
            P_Attacking_Piece,
            P_Attacking_Piece.Player_Id,
            Ret_Status);

      elsif P_Winner = P_Attacking_Piece.Player_Id then
         -- Attacking Piece won
         -- remove loosing (Attacked Piece) piece
         Remove_Piece
           (P_Action_Type,
            P_To_Patch,
            P_Attacked_Piece,
            P_Attacked_Piece.Player_Id,
            Ret_Status);

         -- Move the Attacking Piece if patch where we won is free to move into
         A_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (P_To_Patch.Pos.A,
              P_To_Patch.Pos.B);

         if Landscape.Server.Is_Patch_Empty
             (Landscape.Type_Patch(A_Patch.all))
         then
            -- move winning piece
            Remove_Piece
              (P_Action_Type,
               P_From_Patch,
               P_Attacking_Piece,
               P_Attacking_Piece.Player_Id,
               Ret_Status);
            Put_Piece
              (P_Action_Type,
               P_To_Patch,
               P_Attacking_Piece,
               P_Attacking_Piece.Player_Id,
               Ret_Status);
         end if;

      elsif P_Winner /= Player.Undefined_Player_Id then
         -- Not acceptable:)
         raise Winner_Not_Returned_Properly;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Attack_Step - exit");
      end if;
   end Perform_Attack_Step;

   procedure Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_From_Patch, P_To_Patch            : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            : in     Player.Type_Player_Id)
   is
      Ret_Status : Status.Type_Status;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Ranged_Attack - enter");
      end if;

      if P_Winner = P_Attacking_Piece.Player_Id then
         -- Attacking Piece won
         -- remove loosing (Attacked Piece) piece
         Remove_Piece
           (P_Action_Type,
            P_To_Patch,
            P_Attacked_Piece,
            P_Attacked_Piece.Player_Id,
            Ret_Status);

      -- In a ranged attack, the attacked piece can never win
      -- it can only successfully avoid the attack.
      -- If the attacked piece avoids the attack, we shall receive
      -- Undefined_Player_Id here.
      elsif P_Winner /= Player.Undefined_Player_Id then
         -- Not acceptable:)
         raise Winner_Not_Returned_Properly;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Build_Move_Steps
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in     Type_Piece'Class;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id;
      P_Status                 :    out Status.Type_Status;
      P_Path                   :    out Hexagon.Path.Vector)
   is

      Reachable : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Build_Move_Steps - enter");
      end if;

      Hexagon.Path.Clear (P_Path);

      Reachable := Movement_Terrain_Capability (P_Moving_Piece);

      Hexagon.Utility.Find_Accurate_Path
        (P_Player_Id,
         P_From_Patch.Pos.A,
         P_From_Patch.Pos.B,
         P_To_Patch.Pos.A,
         P_To_Patch.Pos.B,
         Reachable,
         Hexagon.Utility.Move,
         P_Status,
         P_Path);
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Build_Move_Steps - exit P_Status=" &
            P_Status'Img);
      end if;
   end Build_Move_Steps;

   procedure Perform_Move_Step
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id)
   is
      Ret_Status1, Ret_Status2 : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Move_Step - enter");
      end if;

      Remove_Piece
        (P_Action_Type,
         P_From_Patch,
         P_Moving_Piece,
         P_Moving_Piece.Player_Id,
         Ret_Status1);
      Put_Piece
        (P_Action_Type,
         P_To_Patch,
         P_Moving_Piece,
         P_Moving_Piece.Player_Id,
         Ret_Status2);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Perform_Move_Step - exit Status Remove_Piece=" &
            Ret_Status1'Img &
            " Status Put_Piece=" &
            Ret_Status2'Img);
      end if;
   end Perform_Move_Step;

end Piece.Server.Fighting_Piece;
