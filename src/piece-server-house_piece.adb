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

package body Piece.Server.House_Piece is

   Verbose              : constant Boolean := False;
   House_Type_Info_List : Type_House_Type_Info_List_Access;

   procedure Init
     (P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
      P_House_Info        : in Type_House_Type_Info_List)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.House_Piece.Init - enter");
      end if;

      if P_House_Piece_Class.Id /= Piece.Undefined_Piece_Id then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_House_Piece_Class.Type_Of_Piece /= Piece.Undefined_Piece_Type then
         raise Init_Not_Undefined_Piece_Id;
      elsif P_House_Piece_Class.Player_Id /= Player.Undefined_Player_Id then
         raise Init_Not_Undefined_Player_Id;
      end if;

      House_Class := new Piece.Server.Type_Piece'Class'(P_House_Piece_Class);

      House_Type_Info_List := new Type_House_Type_Info_List'(P_House_Info);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.House_Piece.Init - exit");
      end if;
   end Init;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String
   is
   begin
      return House_Type_Info_List.all (P_Piece.Type_Of_Piece).Type_Name;
   end Get_Type_Of_Piece_Name;

   function Can_Construct_On_Land
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean
   is
   begin
      return Piece.Server.House_Piece.House_Type_Info_List (P_Type_Of_Piece)
          .Construct_Landscape
          (P_Landscape);
   end Can_Construct_On_Land;

   function Construction_Terrain_Capability
     (P_Piece : in Type_House'Class)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      All_Construction_Capability,
      With_Terrain_Construction_Capability : Hexagon.Area.Server_Area
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
           ("Piece.Server.House_Piece.Construction_Terrain_Capability - enter P_Piece.Id=" &
            P_Piece.Id'Img);
      end if;

      -- Find the movements that are possible by this piece, as described by the client-developer.
      All_Construction_Capability :=
        Piece.Server.House_Piece.Construction_Capability (P_Piece);
      A_Pos        := Piece.Server.Find_Piece_In_List (P_Piece.Id).Actual_Pos;
      Pieces_Patch :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB (A_Pos.A, A_Pos.B);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Construction_Terrain_Capability - middle All_Construction_Capability'Length=" &
            All_Construction_Capability'Length'Img &
            " Pos of piece to examine A=" &
            A_Pos.A'Img &
            " B=" &
            A_Pos.B'Img);
      end if;

      -- Count the number of positions that are available - to be used to allocate a correct array
      --later
      Count_Patches := 0;
      for Index in
        All_Construction_Capability'First .. All_Construction_Capability'Last
      loop

         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Construction_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Construction_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);
            if Piece.Server.House_Piece.Can_Construct_On_Land
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
           ("Piece.Server.House_Piece.Construction_Terrain_Capability - middle Count_Patches=" &
            Count_Patches'Img);
      end if;

      -- Allocate the array
      With_Terrain_Construction_Capability :=
        new Hexagon.Area.Type_Action_Capabilities (1 .. Count_Patches);

      -- we populate the new array with only the positions that are available.
      Count_Patches := 1;
      for Index in
        All_Construction_Capability'First .. All_Construction_Capability'Last
      loop
         declare
            S_A, S_B : Hexagon.Type_Hexagon_Numbers;
         begin
            S_A :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.A) +
                 Integer (All_Construction_Capability (Index).A));
            S_B :=
              Hexagon.Type_Hexagon_Numbers
                (Integer (Pieces_Patch.Pos.B) +
                 Integer (All_Construction_Capability (Index).B));

            Examine_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB (S_A, S_B);
            if Piece.Server.House_Piece.Can_Construct_On_Land
                (P_Piece.Type_Of_Piece,
                 Examine_Patch.all.Landscape_Here)
            then
               With_Terrain_Construction_Capability (Count_Patches) :=
                 All_Construction_Capability (Index);
               Count_Patches := Count_Patches + 1;
            end if;

         exception
            when others =>
               null;
         end;

      end loop;
      Hexagon.Area.Server_Area.Free_Action_Capabilities
        (All_Construction_Capability);

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Construction_Terrain_Capability - exit");
      end if;

      return With_Terrain_Construction_Capability;
   end Construction_Terrain_Capability;

   function Validate_Constructing_Piece
     (P_Piece     : in Type_House;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Constructing_Piece - enter P_Piece.Player_Id=" &
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
           ("Piece.Server.House_Piece.Validate_Constructing_Piece - exit");
      end if;

      return Ret;
   end Validate_Constructing_Piece;

   function Validate_Construction
     (P_Construction_Piece : in Type_House'Class;
      P_House_Patch        : in Landscape.Type_Patch;
      P_Construction_Patch : in Landscape.Type_Patch) return Boolean
   is
      Construction_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access;
      Ret_Status : Boolean := False;
      use Hexagon;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Construction - enter " &
            " P_House_Patch.Pos.A=" &
            P_House_Patch.Pos.A'Img &
            " P_House_Patch.Pos.B=" &
            P_House_Patch.Pos.B'Img &
            " P_Construction_Patch A=" &
            P_Construction_Patch.Pos.A'Img &
            " B=" &
            P_Construction_Patch.Pos.B'Img &
            " P_Construction_Piece.Type_Of_Piece=" &
            P_Construction_Piece.Type_Of_Piece'Img);
      end if;

      Construction_Patches :=
        Piece.Server.House_Piece.Construction_Capability
          (P_Construction_Piece);
      for Index in Construction_Patches'First .. Construction_Patches'Last loop
         if Verbose then
            Text_IO.Put_Line
              ("Piece.Server.House_Piece.Validate_Construction.. - Construction_Patches (Index).A=" &
               Construction_Patches (Index).A'Img &
               " Reachable_Patches (Index).B=" &
               Construction_Patches (Index).B'Img);
         end if;
         begin
            if Hexagon.Type_Hexagon_Numbers
                (Integer (P_House_Patch.Pos.A) +
                 Integer (Construction_Patches (Index).A)) =
              P_Construction_Patch.Pos.A and
              Hexagon.Type_Hexagon_Numbers
                  (Integer (P_House_Patch.Pos.B) +
                   Integer (Construction_Patches (Index).B)) =
                P_Construction_Patch.Pos.B
            then
               Ret_Status := True;
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.House_Piece.Validate_Construction -.. ok A=" &
                     Construction_Patches (Index).A'Img &
                     " B=" &
                     Construction_Patches (Index).B'Img);
               end if;
            end if;
         exception
            when others =>
               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.House_Piece.Validate_Construction exception, exception. probably outside map");
               end if;
         end;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Construction - exit Status=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Construction;

   function Validate_Construction_Reachable
     (P_Constructing_Piece : in Type_House'Class) return Boolean
   is
      Reachable_Patches : Hexagon.Area.Server_Area
        .Type_Action_Capabilities_Access :=
        null;
      Ret_Status : Boolean := True;

      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Construction_Reachable - enter");
      end if;

      Reachable_Patches :=
        Piece.Server.House_Piece.Construction_Terrain_Capability
          (P_Constructing_Piece);
      if Reachable_Patches = null then
         Ret_Status := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Construction_Reachable - exit");
      end if;

      return Ret_Status;
   end Validate_Construction_Reachable;

   function Validate_Target_Patch_Occupied
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Target_Patch_Occupied - enter");
      end if;

      -- Must not be a piece there
      if not Piece.Server.Patch_Belongs_To_Player (Landscape.Type_Patch(P_Patch), P_Player_Id) then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Target_Patch_Occupied - exit, status=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Target_Patch_Occupied;

   function Validate_Exisiting_Construction
     (P_Patch        : in Landscape.Type_Patch;
      P_Construction : in Construction.Type_Construction) return Boolean
   is
      Ret : Boolean := False;
      n   : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Exisiting_Construction - enter");
      end if;
      n :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (P_Patch.Pos.A,
           P_Patch.Pos.B);

      -- Must not be a piece there
      if Construction.Construction_List.Has_Element
          (Construction.Construction_List.Find
             (P_Patch.Constructions_Here,
              P_Construction))
      then
         Ret := True;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Validate_Exisiting_Construction - exit, status=" &
            Ret'Img);
      end if;

      return Ret;
   end Validate_Exisiting_Construction;

   procedure Pre_Validate_Perform_Construction_Or_Demolition
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Construction_Piece : in     Type_House;
      P_Piece_Patch        : in     Landscape.Type_Patch;
      P_Construction_Patch : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction;
      P_Player_Id          : in     Player.Type_Player_Id;
      P_Mode               : in     Type_Construction_Validation_Mode;
      P_Status             :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Pre_Validate_Perform_Construction_Or_Demolition - enter");
      end if;
      P_Status := Status.Ok;

      if not Piece.Server.House_Piece.Validate_Constructing_Piece
          (Piece.Server.House_Piece.Type_House (P_Construction_Piece),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.House_Piece.Validate_Construction_Reachable
          (Piece.Server.House_Piece.Type_House (P_Construction_Piece))
      then
         P_Status := Status.No_Movement_Capability;
      elsif not Validate_Target_Patch_Occupied
          (P_Construction_Patch,
           P_Player_Id)
      then
         P_Status := Status.Target_Patch_Occupied;
      elsif not Piece.Server.House_Piece.Validate_Construction
          (Piece.Server.House_Piece.Type_House (P_Construction_Piece),
           Landscape.Type_Patch (P_Piece_Patch),
           Landscape.Type_Patch (P_Construction_Patch))
      then
         P_Status := Status.Not_Reachable;
      elsif P_Mode = Construction_Mode and
        Piece.Server.House_Piece.Validate_Exisiting_Construction
          (Landscape.Type_Patch (P_Construction_Patch),
           P_Construction)
      then
         P_Status := Status.Construction_Exists;
      elsif P_Mode = Demolition_Mode and
        not Piece.Server.House_Piece.Validate_Exisiting_Construction
          (Landscape.Type_Patch (P_Construction_Patch),
           P_Construction)
      then
         P_Status := Status.Construction_Doesnt_Exist;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.House_Piece.Pre_Validate_Perform_Construction_Or_Demolition - exit P_Status=" &
            P_Status'Img);
      end if;
   end Pre_Validate_Perform_Construction_Or_Demolition;

end Piece.Server.House_Piece;
