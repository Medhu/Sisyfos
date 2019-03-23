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
with Piece.Server.House_Piece;
with Piece.Server.Fighting_Piece;
with Ada.Streams.Stream_IO;
with Landscape.Server;

package body Piece.Server is
   Verbose : constant Boolean := False;

   Next_Id : Integer := 0;

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Piece.Server.Pieces_Server_List.Cursor
   is
      Trav : Piece.Server.Pieces_Server_List.Cursor;

      Found : Boolean;
   begin
      Found := False;
      Trav  :=
        Piece.Server.Pieces_Server_List.First
          (Piece.Server.All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav) and not Found
      loop
         if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece.all
             .Id =
           P_Piece_Id
         then
            Found := True;
         else
            Trav := Piece.Server.Pieces_Server_List.Next (Trav);
         end if;
      end loop;

      if not Found then
         raise Piece_Not_Found_Piece_Position
           with "Did not find Piece.Id:" & P_Piece_Id'Img;
      end if;

      return Trav;
   end Find_Piece_In_List;

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id) return Type_Piece_Position
   is
      Trav : Piece.Server.Pieces_Server_List.Cursor;

      Ret : Piece.Server.Type_Piece_Position;
   begin
      Trav := Piece.Server.Find_Piece_In_List (P_Piece_Id);
      Ret  := Piece.Server.Pieces_Server_List.Element (Trav);

      return Ret;
   end Find_Piece_In_List;

   function Left_Less_Pieces
     (Left, Right : in Type_Piece_Access_Class) return Boolean
   is
   begin
      return Integer (Left.Id) < Integer (Right.Id);
   end Left_Less_Pieces;

   function Equal_Pieces
     (Left, Right : in Type_Piece_Access_Class) return Boolean
   is
   begin
      return Left = Right;
   end Equal_Pieces;

   function Generate_Piece_Id return Type_Piece_Id is
   begin

      Next_Id := Next_Id + 1;
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Generate_Piece_Id - enter - exit Next_Id := " &
            Next_Id'Img);
      end if;
      return Type_Piece_Id (Next_Id);
   end Generate_Piece_Id;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String
   is
      Ret : Utilities.RemoteString.Type_String;
   begin
      if P_Piece.Category = Piece.Fighting_Piece then
         Ret := Piece.Server.Fighting_Piece.Get_Type_Of_Piece_Name (P_Piece);
      elsif P_Piece.Category = Piece.House_Piece then
         Ret := Piece.Server.House_Piece.Get_Type_Of_Piece_Name (P_Piece);
      end if;

      return Ret;

   end Get_Type_Of_Piece_Name;

   procedure New_Piece
     (P_Piece        : in     Piece.Type_Piece;
      P_Server_Piece :    out Piece.Server.Type_Piece_Access_Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.New_Piece - enter");
      end if;

      if P_Piece.Category = Piece.House_Piece then
         P_Server_Piece :=
           new Piece.Server.Type_Piece'Class'
             (Piece.Server.House_Piece.House_Class.all);

      elsif P_Piece.Category = Piece.Fighting_Piece then
         P_Server_Piece :=
           new Piece.Server.Type_Piece'Class'
             (Piece.Server.Fighting_Piece.Piece_Class.all);

      end if;

      P_Server_Piece.all.Type_Of_Piece := P_Piece.Type_Of_Piece;
      P_Server_Piece.all.Category      := P_Piece.Category;
      P_Server_Piece.all.Player_Id     := P_Piece.Player_Id;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.New_Piece - exit");
      end if;

   end New_Piece;

   procedure Init_Piece
     (P_Server_Piece : in out Piece.Server.Type_Piece_Access_Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Init_Piece - enter");
      end if;

      P_Server_Piece.all.Id := Piece.Server.Generate_Piece_Id;

      -- Add the new piece to the server list
      Piece.Server.Pieces_Server_List.Append
        (Piece.Server.All_Pieces_In_Game,
         Piece.Server.Type_Piece_Position'
           (P_Server_Piece, Hexagon.Type_Hexagon_Position'(P_Valid => False)));

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Init_Piece - exit");
      end if;

   end Init_Piece;

   procedure Delete_Piece
     (P_Server_Piece : in out Piece.Server.Type_Piece_Access_Class)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Delete_Piece - enter");
      end if;

      Piece.Server.Free_Piece (P_Server_Piece);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Delete_Piece - exit");
      end if;

   end Delete_Piece;

   procedure Put (P_Piece : in Type_Piece) is
   begin
      Text_IO.Put
        ("Id=" &
         P_Piece.Id'Img &
         "Type_Of_Piece=" &
         P_Piece.Type_Of_Piece'Img &
         " Player_Id=" &
         P_Piece.Player_Id'Img);
   end Put;

   procedure Reset_Positions is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Reset_Position - enter");
      end if;

      Piece.Server.Pieces_Server_List.Clear (All_Pieces_In_Game);

      Next_Id := 0;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Reset_Position - exit");
      end if;
   end Reset_Positions;

   function Count_Positions return Integer
   is   -- service to unit tests to be able to teste
      --Reset_Positions;
      Num_Of_Positions : Integer := 0;
      Trav             : Piece.Server.Pieces_Server_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Count_Positions - enter");
      end if;

      Trav := Piece.Server.Pieces_Server_List.First (All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav) loop

         if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos
             .P_Valid
         then
            Num_Of_Positions := Num_Of_Positions + 1;
         end if;

         Trav := Piece.Server.Pieces_Server_List.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Count_Positions - exit " & Num_Of_Positions'Img);
      end if;

      return Num_Of_Positions;
   end Count_Positions;

   procedure Set_Position
     (P_Piece : in Type_Piece;
      P_Pos   : in Hexagon.Type_Hexagon_Position)
   is
      Trav                  : Piece.Server.Pieces_Server_List.Cursor;
      Actual_Piece_Position : Type_Piece_Position;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Set_Position - enter Piece.Id=" & P_Piece.Id'Img);
         if P_Pos.P_Valid then
            Text_IO.Put_Line
              (" P_Pos.A=" & P_Pos.A'Img & " P_B=" & P_Pos.B'Img);
         else
            Text_IO.Put_Line ("P_Pos=Invalid");
         end if;

      end if;

      if P_Piece.Id = Piece.Undefined_Piece_Id then
         raise Piece_Id_Not_Init;
      end if;

      Trav := Piece.Server.Find_Piece_In_List (P_Piece.Id);

      Actual_Piece_Position :=
        Type_Piece_Position'
          (Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece,
           Hexagon.Type_Hexagon_Position'(True, P_Pos.A, P_Pos.B));

      Piece.Server.Pieces_Server_List.Replace_Element
        (All_Pieces_In_Game,
         Trav,
         Actual_Piece_Position);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Set_Position - exit");
      end if;
   end Set_Position;

   procedure Unset_Position
     (P_Piece : in Type_Piece;
      P_Pos   : in Hexagon.Type_Hexagon_Position)
   is
      Trav                  : Piece.Server.Pieces_Server_List.Cursor;
      Actual_Piece_Position : Type_Piece_Position;

      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put
           ("Piece.Server.Unset_Position - enter Piece.Id=" & P_Piece.Id'Img);
         if P_Pos.P_Valid then
            Text_IO.Put_Line
              (" P_Pos.A=" & P_Pos.A'Img & " P_B=" & P_Pos.B'Img);
         else
            Text_IO.Put_Line ("P_Pos=Invalid");
         end if;
      end if;

      Trav := Piece.Server.Find_Piece_In_List (P_Piece.Id);

      Actual_Piece_Position := Piece.Server.Pieces_Server_List.Element (Trav);

      Actual_Piece_Position.Actual_Pos :=
        Hexagon.Type_Hexagon_Position'(P_Valid => False);
      Piece.Server.Pieces_Server_List.Replace_Element
        (All_Pieces_In_Game,
         Trav,
         Actual_Piece_Position);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Unset_Position - exit");
      end if;
   end Unset_Position;

   procedure Put_All_Pieces_Positions
     (P_Player_Id : in Player.Type_Player_Id)
   is
      Trav : Piece.Server.Pieces_Server_List.Cursor;

      use Player;
      use Hexagon.Area;
   begin
      Trav := Piece.Server.Pieces_Server_List.First (All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav) loop

         if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece
             .Player_Id =
           P_Player_Id and
           Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.P_Valid
         then
            Text_IO.Put
              ("All_Pieces_In_Game(Trav).Actual_Piece.Id=" &
               Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece.Id'
                 Img &
               "All_Pieces_In_Game(Trav).Actual_Pos.A=" &
               Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.A'
                 Img &
               "All_Pieces_In_Game(Trav).Actual_Pos.B=" &
               Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.B'
                 Img);
         end if;
         Trav := Piece.Server.Pieces_Server_List.Next (Trav);
      end loop;
   end Put_All_Pieces_Positions;

   function Get_All_Pieces_Positions
     return Piece.Server.Pieces_Server_List.Vector
   is
   begin
      return All_Pieces_In_Game;
   end Get_All_Pieces_Positions;

   procedure Get_Effects_On_Piece
     (P_Piece         : in     Piece.Server.Type_Piece;
      P_Pieces_Effect :    out Observation.Observation_Of_Pieces_Effects
        .Observations_Of_Pieces_Effects
        .Set)
   is
      Effect_Cursor : Effect.Effect_List.Cursor;
      An_Effect     : Effect.Type_Effect;

      use Piece;
   begin
   -- Traverse all pieces observed pieces (friendly and enemy that are visible)
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Get_Effects_On_Piece - enter P_Piece.Id=" &
            P_Piece.Id'Img);
      end if;

      Effect_Cursor := Effect.Effect_List.First (P_Piece.Effects_On_Piece);

      while Effect.Effect_List.Has_Element (Effect_Cursor) loop
         An_Effect := Effect.Effect_List.Element (Effect_Cursor);

         Observation.Observation_Of_Pieces_Effects
           .Observations_Of_Pieces_Effects
           .Include
           (P_Pieces_Effect,
            Observation.Observation_Of_Pieces_Effects.Type_Piece_Effect'
              (P_Piece.Id, An_Effect, True));

         Effect_Cursor := Effect.Effect_List.Next (Effect_Cursor);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Effects_On_Piece - exit");
      end if;

   end Get_Effects_On_Piece;

   procedure Get_Effects_On_Patch
     (P_Patch                    : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Observed_Patches_Effects : in out Observation
        .Observation_Of_Patches_Effects
        .Observations_Of_Patches_Effects
        .Set)
   is
      Trav_Patch_Effects : Effect.Effect_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Effects_On_Patch - enter");
      end if;

      Trav_Patch_Effects := Effect.Effect_List.First (P_Patch.Effects_Here);

      while Effect.Effect_List.Has_Element (Trav_Patch_Effects) loop

         Observation.Observation_Of_Patches_Effects
           .Observations_Of_Patches_Effects
           .Include
           (P_Observed_Patches_Effects,
            Observation.Observation_Of_Patches_Effects.Type_Patch_Effect'
              (P_Patch.Pos,
               Effect.Effect_List.Element (Trav_Patch_Effects),
               True));
         Trav_Patch_Effects := Effect.Effect_List.Next (Trav_Patch_Effects);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Effects_On_Patch - exit");
      end if;
   end Get_Effects_On_Patch;

   procedure Get_Pieces_Report
     (P_Player_Id     : in     Player.Type_Player_Id;
      P_Pieces_Report :    out Type_Pieces_Report)
   is
      Area_To_Check : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      Observing_Piece_Pos, Observed_Pos : Hexagon.Type_Hexagon_Position;
      Pos_A, Pos_B                      : Integer;
      Patch_Observer_Adress,
      Patch_Observed_Adress : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Observing_Piece, Observed_Piece : Piece.Server.Type_Piece_Access_Class;
      Trav_Observing_Pieces,
      Trav_Observed_Pieces : Landscape.Pieces_Here_List.Cursor;
      Trav_All_Pieces      : Piece.Server.Pieces_Server_List.Cursor;

      use Player;
      use Hexagon;
      use Hexagon.Area.Server_Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Pieces_Report - enter");
      end if;

      -- traverse all pieces of this player
      Trav_All_Pieces :=
        Piece.Server.Pieces_Server_List.First (All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav_All_Pieces) loop

         if Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces)
             .Actual_Piece
             .Player_Id =
           P_Player_Id
         then
            -- Calculate the area that the piece can observe
            if Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces)
                .Actual_Pos
                .P_Valid
            then

               Observing_Piece_Pos :=
                 Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces)
                   .Actual_Pos;

               if Verbose then
                  Text_IO.Put_Line
                    ("Piece.Server.Get_Pieces_Report - calculate area for piece at A=" &
                     Observing_Piece_Pos.A'Img &
                     " B=" &
                     Observing_Piece_Pos.B'Img);
               end if;

               Patch_Observer_Adress :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Observing_Piece_Pos.A,
                    Observing_Piece_Pos.B);

               Trav_Observing_Pieces :=
                 Landscape.Pieces_Here_List.First
                   (Patch_Observer_Adress.all.Pieces_Here);
               while Landscape.Pieces_Here_List.Has_Element
                   (Trav_Observing_Pieces)
               loop
                  Observing_Piece :=
                    Piece.Server.Find_Piece_In_List
                      (Landscape.Pieces_Here_List.Element
                         (Trav_Observing_Pieces))
                      .Actual_Piece;

                  Area_To_Check := Observation_Area (Observing_Piece.all);

               -- Could not get any area of observation for this Type_Of_Piece
                  if Area_To_Check = null then
                     raise Type_Of_Piece_Missing_Implementation;
                  end if;

                  -- Traverser observasjonsomraadet

                  for Trav_Obs in Area_To_Check'First .. Area_To_Check'Last
                  loop

                     Pos_A :=
                       Integer (Observing_Piece_Pos.A) +
                       Integer (Area_To_Check (Trav_Obs).A);
                     Pos_B :=
                       Integer (Observing_Piece_Pos.B) +
                       Integer (Area_To_Check (Trav_Obs).B);

                     if Pos_A in
                         Integer (Hexagon.Type_Hexagon_Numbers'First) ..
                               Integer (Hexagon.Type_Hexagon_Numbers'Last) and
                       Pos_B in
                         Integer (Hexagon.Type_Hexagon_Numbers'First) ..
                               Integer (Hexagon.Type_Hexagon_Numbers'Last)
                     then

                        Observed_Pos :=
                          Hexagon.Type_Hexagon_Position'
                            (True,
                             Hexagon.Type_Hexagon_Numbers (Pos_A),
                             Hexagon.Type_Hexagon_Numbers (Pos_B));
                        if Verbose then
                           Text_IO.Put_Line
                             ("Piece.Server.Get_Pieces_Report - and area is A=" &
                              Observed_Pos.A'Img &
                              " B=" &
                              Observed_Pos.B'Img);
                        end if;

                        Patch_Observed_Adress :=
                          Hexagon.Server_Map.Get_Patch_Adress_From_AB
                            (Observed_Pos.A,
                             Observed_Pos.B);
                        Observation.Observation_Of_Patches
                          .Observations_Of_Patches
                          .Include
                          (P_Pieces_Report.Observed_Patches,
                           Observation.Observation_Of_Patches
                             .Type_Observed_Patch'
                             (Observed_Pos.A, Observed_Pos.B, True));

                        -- Observe effects on the patches in observation area
                        Get_Effects_On_Patch
                          (Patch_Observed_Adress.all,
                           P_Pieces_Report.Observed_Patches_Effects);

                        Trav_Observed_Pieces :=
                          Landscape.Pieces_Here_List.First
                            (Patch_Observed_Adress.all.Pieces_Here);

                        while Landscape.Pieces_Here_List.Has_Element
                            (Trav_Observed_Pieces)
                        loop
                           Observed_Piece :=
                             Piece.Server.Find_Piece_In_List
                               (Landscape.Pieces_Here_List.Element
                                  (Trav_Observed_Pieces))
                               .Actual_Piece;

               -- Include piece that we have observed in the observation area.
                           Observation.Observation_Of_Pieces_Info
                             .Observations_Of_Pieces_Info
                             .Include
                             (P_Pieces_Report.Observed_Pieces_Info,
                              Observation.Observation_Of_Pieces_Info
                                .Type_Observed_Piece_Info'
                                (Piece.Type_Piece (Observed_Piece.all), True));

                           Observation.Observation_Of_Pieces
                             .Observations_Of_Pieces
                             .Include
                             (P_Pieces_Report.Observed_Pieces,
                              Observation.Observation_Of_Pieces
                                .Type_Observed_Piece'
                                (Observed_Pos, Observed_Piece.Id));

                           -- Effects for this piece
                           --
                           Piece.Server.Get_Effects_On_Piece
                             (Piece.Server.Type_Piece (Observed_Piece.all),
                              P_Pieces_Report.Observed_Pieces_Effects);

                           Trav_Observed_Pieces :=
                             Landscape.Pieces_Here_List.Next
                               (Trav_Observed_Pieces);
                        end loop; -- Trav all pieces on patch

                     end if;

                  end loop;

                  Trav_Observing_Pieces :=
                    Landscape.Pieces_Here_List.Next (Trav_Observing_Pieces);
               end loop; -- Trav All pieces on the patch

            end if;

         end if; -- not this player

         Trav_All_Pieces :=
           Piece.Server.Pieces_Server_List.Next (Trav_All_Pieces);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Pieces_Report - exit");
      end if;
   end Get_Pieces_Report;

   function Get_Pieces_Players
     (P_Patch : in Landscape.Type_Patch) return Player.Type_Player_Id
   is
      A_Piece : Piece.Server.Type_Piece_Access_Class;
      Trav    : Landscape.Pieces_Here_List.Cursor;
      Ret     : Player.Type_Player_Id;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Get_Pieces_Players - enter");
      end if;

      Trav :=
        Landscape.Pieces_Here_List.First
          (P_Patch.Pieces_Here);      ---only one player
      --can be on any
      --patch, so it is
      --enough to test
      --one of them
      A_Piece :=
        Piece.Server.Find_Piece_In_List
          (Landscape.Pieces_Here_List.Element (Trav))
          .Actual_Piece;
      Ret := A_Piece.all.Player_Id;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Get_Pieces_Players -  Piece_Id=" &
            A_Piece.all.Id'Img &
            " Player_Id=" &
            Ret'Img);
      end if;

      return Ret;

   end Get_Pieces_Players;

   function Is_Players_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Fighting_Piece.Is_Players_Piece - enter P_Piece.Player_Id=" &
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
           ("Piece.Server.Fighting_Piece.Is_Players_Piece - exit");
      end if;

      return Ret;
   end Is_Players_Piece;

   function Is_Type_Of_Piece_Here
     (P_Patch         : in Hexagon.Server_Map.Type_Server_Patch;
      P_Type_Of_Piece :    Piece.Type_Piece_Type) return Boolean
   is
      Found   : Boolean := False;
      Trav    : Landscape.Pieces_Here_List.Cursor;
      A_Piece : Piece.Type_Piece;

      use Piece;
   begin
      Trav := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) and not Found loop
         A_Piece :=
           Piece.Type_Piece
             (Piece.Server.Find_Piece_In_List
                (Landscape.Pieces_Here_List.Element (Trav))
                .Actual_Piece.all);
         Found := A_Piece.Type_Of_Piece = P_Type_Of_Piece;

         Landscape.Pieces_Here_List.Next (Trav);
      end loop;

      return Found;
   end Is_Type_Of_Piece_Here;

   function Is_Piece_Here
     (P_Patch : in Landscape.Type_Patch;
      P_Piece : in Piece.Type_Piece) return Boolean
   is
      Found : Boolean := False;
      Trav  : Landscape.Pieces_Here_List.Cursor;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Is_Piece_Here - enter");
      end if;

      Trav := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) and not Found loop
         Found := Landscape.Pieces_Here_List.Element (Trav) = P_Piece.Id;

         Landscape.Pieces_Here_List.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Is_Piece_Here - exit=" & Found'Img);
      end if;

      return Found;
   end Is_Piece_Here;

   function Find_Slot_Of_Pieces
     (P_Patch : in Landscape.Type_Patch;
      P_Piece : in Piece.Type_Piece) return Positive
   is
      Found : Boolean := False;
      Trav  : Landscape.Pieces_Here_List.Cursor;

      use Piece;
   begin
      Trav := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) and not Found loop
         if Landscape.Pieces_Here_List.Element (Trav) = P_Piece.Id then
            Found := True;
         else
            Landscape.Pieces_Here_List.Next (Trav);
         end if;
      end loop;

      return Landscape.Pieces_Here_List.To_Index (Trav);
   end Find_Slot_Of_Pieces;

   function Patch_Belongs_To_Player
     (P_Patch     : in Landscape.Type_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Patch_Belongs_To_Player - enter P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      if Landscape.Server.Is_Patch_Empty (P_Patch) then
         Ret := True; -- Neutral patch belongs to noone and everyone
      else
         Ret := Get_Pieces_Players (P_Patch) = P_Player_Id;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Patch_Belongs_To_Player - exit " & Ret'Img);
      end if;

      return Ret;
   end Patch_Belongs_To_Player;

   procedure Validate_Target_Patch_And_Piece
     (P_Patch     : in     Landscape.Type_Patch;
      P_Piece     : in     Piece.Type_Piece;
      P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Status)
   is

      use Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Target_Patch_And_Piece - enter");
      end if;
      P_Status := Status.Ok;

      if not Landscape.Server.Has_Patch_Free_Slot (P_Patch) or
        not Patch_Belongs_To_Player (P_Patch, P_Player_Id)
      then
         P_Status := Status.Patch_Occupied;
      elsif P_Piece.Category = Piece.Fighting_Piece
        and then
        (not Piece.Server.Fighting_Piece.Can_Move_Here
           (P_Piece.Type_Of_Piece,
            P_Patch.Landscape_Here))
      then
         P_Status := Status.Patch_Bad_Terrain;
      elsif P_Piece.Category = Piece.House_Piece
        and then
        (not Piece.Server.House_Piece.Can_Construct_On_Land
           (P_Piece.Type_Of_Piece,
            P_Patch.Landscape_Here))
      then
         P_Status := Status.Patch_Bad_Terrain;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Target_Patch_And_Piece - exit, status=" &
            P_Status'Img);
      end if;

   end Validate_Target_Patch_And_Piece;

   function Validate_Grant_Revoke_Effect_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Grant_Revoke_Effect_Piece - enter P_Piece.Player_Id=" &
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
           ("Piece.Server.Validate_Grant_Revoke_Effect_Piece - exit");
      end if;

      return Ret;
   end Validate_Grant_Revoke_Effect_Piece;

   procedure Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Type_Piece;
      P_Status      :    out Status.Type_Status)
   is

      use Status;
      use Piece;
      use Landscape;
   begin
      if Verbose then
         if P_Patch.Pos.P_Valid then
            Text_IO.Put_Line
              ("Piece.Server.Put_Piece the piece in patch  P_A=" &
               P_Patch.Pos.A'Img &
               "  P_B=" &
               P_Patch.Pos.B'Img);
         else
            Text_IO.Put_Line
              ("Piece.Server.Put_Piece the piece in patch  invalid position");
         end if;
      end if;

      Validate_Target_Patch_And_Piece
        (P_Patch,
         Piece.Type_Piece (P_Piece),
         P_Player_Id,
         P_Status);

      if P_Status = Status.Ok then
         -- Copy new data from the client patch into the server patch
         Landscape.Pieces_Here_List.Append (P_Patch.Pieces_Here, P_Piece.Id);
         Landscape.Pieces_Here_Sort.Sort (P_Patch.Pieces_Here);
         Piece.Server.Set_Position (P_Piece, P_Patch.Pos);

         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Put_Piece - exit " & P_Status'Img);
      end if;
   end Put_Piece;

   function Validate_Removing_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Removing_Piece - enter P_Piece.Player_Id=" &
            P_Piece.Player_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      -- Player can only move his own piece
      if P_Piece.Player_Id /= P_Player_Id then
         Ret := False;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Validate_Removing_Piece - exit");
      end if;

      return Ret;
   end Validate_Removing_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Patch       : in out Landscape.Type_Patch;
      P_Piece       : in out Type_Piece;
      P_Status      :    out Status.Type_Status)
   is
      Index   : Positive;
      A_Piece : Piece.Server.Type_Piece_Access_Class;

      use Ada.Containers;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Remove_Piece the piece in patch  P_Piece.Id=" &
            P_Piece.Id'Img);
      end if;

      A_Piece := Piece.Server.Find_Piece_In_List (P_Piece.Id).Actual_Piece;

      if not Piece.Server.Validate_Removing_Piece
          (Piece.Server.Type_Piece (A_Piece.all),
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      else
         Index :=
           Piece.Server.Find_Slot_Of_Pieces
             (P_Patch,
              Piece.Type_Piece (P_Piece));

         Landscape.Pieces_Here_List.Delete (P_Patch.Pieces_Here, Index);

         Piece.Server.Unset_Position (P_Piece, P_Patch.Pos);
         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Remove_Piece - exit " & P_Status'Img);
      end if;
   end Remove_Piece;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Grant_Piece_Effect- enter");
      end if;

      if not Piece.Server.Validate_Grant_Revoke_Effect_Piece
          (P_Piece,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      else
         Effect.Effect_List.Include
           (P_Piece.Effects_On_Piece,
            P_Effect.Effect_Name,
            P_Effect);
         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Grant_Piece_Effect- exit length=" &
            Effect.Effect_List.Length (P_Piece.Effects_On_Piece)'Img);
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Revoke_Piece_Effect- enter");
      end if;

      if not Piece.Server.Validate_Grant_Revoke_Effect_Piece
          (P_Piece,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      else
         Effect.Effect_List.Exclude
           (P_Piece.Effects_On_Piece,
            P_Effect.Effect_Name);
         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Revoke_Piece_Effect- exit length=" &
            Effect.Effect_List.Length (P_Piece.Effects_On_Piece)'Img);
      end if;
   end Revoke_Piece_Effect;
   --

   function Validate_Grant_Revoke_Effect_Patch
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id;
      P_Area      : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
      Ret : Boolean := True;

      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Grant_Revoke_Effect_Patch - enter P_Piece.Player_Id=" &
            P_Piece.Player_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

   -- Player can only place effects on patches that he "owns" or noone "owns"
      for Trav_Area in P_Area'First .. P_Area'Last loop
         A_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (P_Area (Trav_Area).A,
              P_Area (Trav_Area).B);

         Ret :=
           Ret and
           Piece.Server.Patch_Belongs_To_Player
             (Landscape.Type_Patch (A_Patch.all),
              P_Player_Id);
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Grant_Revoke_Effect_Patch - exit");
      end if;

      return Ret;
   end Validate_Grant_Revoke_Effect_Patch;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status      :    out Status.Type_Status)
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Grant_Patch_Effect- enter Effect " &
            P_Effect.Effect_Name'Img &
            " Aux=" &
            P_Effect.Aux'Img);
      end if;

      if not Piece.Server.Validate_Grant_Revoke_Effect_Piece
          (P_Piece,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Validate_Grant_Revoke_Effect_Patch
          (P_Piece,
           P_Player_Id,
           P_Area)
      then
         P_Status := Status.Target_Patch_Occupied;
      else
         for Trav_Area in P_Area'First .. P_Area'Last loop
            if P_Area (Trav_Area).P_Valid then
               A_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (P_Area (Trav_Area).A,
                    P_Area (Trav_Area).B);

               Effect.Effect_List.Include
                 (A_Patch.Effects_Here,
                  P_Effect.Effect_Name,
                  P_Effect);
            end if;
         end loop;
         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Grant_Patch_Effect- enter");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Status      :    out Status.Type_Status)
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Revoke_Patch_Effect- enter");
      end if;

      if not Piece.Server.Validate_Grant_Revoke_Effect_Piece
          (P_Piece,
           P_Player_Id)
      then
         P_Status := Status.Not_Players_Piece;
      elsif not Piece.Server.Validate_Grant_Revoke_Effect_Patch
          (P_Piece,
           P_Player_Id,
           P_Area)
      then
         P_Status := Status.Target_Patch_Occupied;
      else
         for Trav_Area in P_Area'First .. P_Area'Last loop
            if P_Area (Trav_Area).P_Valid then
               A_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (P_Area (Trav_Area).A,
                    P_Area (Trav_Area).B);

               Effect.Effect_List.Exclude
                 (A_Patch.Effects_Here,
                  P_Effect.Effect_Name);
            end if;
         end loop;
         P_Status := Status.Ok;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Revoke_Patch_Effect- exit");
      end if;
   end Revoke_Patch_Effect;

   function Validate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece'Class;
      P_Effect      : in Effect.Type_Effect;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
      Found : Boolean := False;
      Trav_Area  : Positive;
      Cur_Effect : Effect.Effect_List.Cursor;
      A_Patch    : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Validate_Patch_Effect - enter");
      end if;

      Trav_Area  := P_Area'First;
      while Trav_Area in P_Area'First .. P_Area'Last and not Found loop

         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB(P_Area(Trav_Area).A, P_Area(Trav_Area).B);
         Cur_Effect :=
           Effect.Effect_List.Find
             (A_Patch.Effects_Here,
              P_Effect.Effect_Name);

         if Effect.Effect_List.Has_Element (Cur_Effect) then
            Found := True;
         end if;

         Trav_Area := Trav_Area + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Patch_Effect - exit Found=" &
            Found'Img);
      end if;

      return Found;
   end Validate_Patch_Effect;

   function Validate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_Piece'Class;
      P_Effect      : in Effect.Type_Effect) return Boolean
   is
      Ret_Status : Boolean := False;

      Cur_Effect : Effect.Effect_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Validate_Piece_Effect - enter");
      end if;

      Cur_Effect :=
        Effect.Effect_List.Find
          (P_Piece.Effects_On_Piece,
           P_Effect.Effect_Name);

      if Effect.Effect_List.Has_Element (Cur_Effect) then
         Ret_Status := True;
      else
         Ret_Status := False;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Server.Validate_Piece_Effect - exit Status=" &
            Ret_Status'Img);
      end if;

      return Ret_Status;
   end Validate_Piece_Effect;

   procedure Print_Pieces_In_Game is
      Trav : Piece.Server.Pieces_Server_List.Cursor;

   begin
      Trav :=
        Piece.Server.Pieces_Server_List.Last (Piece.Server.All_Pieces_In_Game);
      Text_IO.Put_Line
        ("Printing Pieces In Game Length =" &
         Piece.Server.Pieces_Server_List.Length
           (Piece.Server.All_Pieces_In_Game)'
           Img);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav) loop

         Text_IO.Put
           ("Index=" &
            Piece.Server.Pieces_Server_List.To_Index (Trav)'Img &
            " Id=" &
            Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece.Id'
              Img &
            " Type_Of_Piece=" &
            Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece
              .Type_Of_Piece'
              Img &
            " Player_Id=" &
            Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece
              .Player_Id'
              Img);
         if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos
             .P_Valid
         then
            Text_IO.Put
              (" A=" &
               Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.A'
                 Img);
            Text_IO.Put
              (" B=" &
               Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.B'
                 Img);
         else
            Text_IO.Put_Line ("Invalid position");
         end if;
         Text_IO.New_Line;

         Text_IO.Put_Line ("Effects_On_Piece:");
         Effect.Print_Effect_List
           (Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece.all
              .Effects_On_Piece);

         Trav := Piece.Server.Pieces_Server_List.Previous (Trav);
      end loop;
   end Print_Pieces_In_Game;

   procedure Upkeep_All (P_Player_Id : in Player.Type_Player_Id) is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch :=
        Hexagon.Server_Map.Empty;
      A_Piece : Piece.Server.Type_Piece_Access_Class;
      Trav    : Piece.Server.Pieces_Server_List.Cursor;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Upkeep_All - enter");
      end if;

      Trav := Piece.Server.Pieces_Server_List.First (All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav) loop

         if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos
             .P_Valid
         then

            A_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB
                (Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos.A,
                 Piece.Server.Pieces_Server_List.Element (Trav).Actual_Pos
                   .B).all;
            A_Piece :=
              Piece.Server.Type_Piece_Access_Class
                (Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece);

            if A_Piece.all.Player_Id = P_Player_Id then

               if Piece.Server.Pieces_Server_List.Element (Trav).Actual_Piece
                   .Category =
                 Piece.Fighting_Piece
               then
                  begin
                     Piece.Server.Fighting_Piece.Upkeep
                       (A_Patch,
                        Piece.Server.Fighting_Piece.Type_Piece'Class
                          (A_Piece.all));
                  exception
                     when others =>
                        Text_IO.Put_Line
                          (Text_IO.Current_Error,
                           "Piece.Server.Upkeep_All:");
                        Text_IO.Put_Line
                          (Text_IO.Current_Error,
                           "Exception from Piece.Server.Fighting_Piece.Upkeep for Piece.Id:" &
                           A_Piece.all.Id'Img);
                        raise;
                  end;

               elsif Piece.Server.Pieces_Server_List.Element (Trav)
                   .Actual_Piece
                   .Category =
                 Piece.House_Piece
               then
                  begin
                     Piece.Server.House_Piece.Upkeep
                       (A_Patch,
                        Piece.Server.House_Piece.Type_House'Class
                          (A_Piece.all));
                  exception
                     when others =>
                        Text_IO.Put_Line
                          (Text_IO.Current_Error,
                           "Piece.Server.Upkeep_All:");
                        Text_IO.Put_Line
                          (Text_IO.Current_Error,
                           "Exception from Piece.Server.House_Piece.Upkeep for Piece.Id:" &
                           A_Piece.all.Id'Img);
                        raise;
                  end;

               end if;

            end if;

         end if;
         Trav := Piece.Server.Pieces_Server_List.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Upkeep_All - exit");
      end if;
   end Upkeep_All;

   procedure Save_Pieces
     (P_File_Name  : in Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List : in Pieces_Server_List.Vector)
   is
      Trav          : Pieces_Server_List.Cursor;
      An_Element    : Type_Piece_Position;
      Num_Of_Pieces : Ada.Containers.Count_Type;

      Write_File   : Ada.Streams.Stream_IO.File_Type;
      Write_Stream : Ada.Streams.Stream_IO.Stream_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Save_Pieces - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Write_File,
         Ada.Streams.Stream_IO.Out_File,
         Ada.Strings.Unbounded.To_String (P_File_Name));

      Write_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      -- Save the current Next_Id
      Integer'Write (Write_Stream, Next_Id);

      Num_Of_Pieces := Pieces_Server_List.Length (P_Piece_List);

      Ada.Containers.Count_Type'Write (Write_Stream, Num_Of_Pieces);

      Trav := Pieces_Server_List.First (P_Piece_List);
      while Pieces_Server_List.Has_Element (Trav) loop
         An_Element := Pieces_Server_List.Element (Trav);
         Piece.Type_Piece_Id'Write
           (Write_Stream,
            An_Element.Actual_Piece.all.Id);
         Piece.Type_Category'Write
           (Write_Stream,
            An_Element.Actual_Piece.all.Category);
         Piece.Type_Piece'Class'Write
           (Write_Stream,
            Piece.Type_Piece'Class (An_Element.Actual_Piece.all));
         Hexagon.Type_Hexagon_Position'Write
           (Write_Stream,
            An_Element.Actual_Pos);

         Trav := Pieces_Server_List.Next (Trav);
      end loop;

      Ada.Streams.Stream_IO.Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Save_Pieces - exit");
      end if;
   end Save_Pieces;

   procedure Load_Pieces
     (P_File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      P_Piece_List :    out Pieces_Server_List.Vector)
   is
      An_Element    : Type_Piece_Position;
      Piece_Id      : Piece.Type_Piece_Id;
      A_Category    : Piece.Type_Category;
      Num_Of_Pieces : Ada.Containers.Count_Type;

      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Read_File   : Ada.Streams.Stream_IO.File_Type;
      Read_Stream : Ada.Streams.Stream_IO.Stream_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Load_Pieces - enter");
      end if;

      Text_IO.Put_Line
        ("Pieces File Name:" & Ada.Strings.Unbounded.To_String (P_File_Name));

      Ada.Streams.Stream_IO.Open
        (Read_File,
         Ada.Streams.Stream_IO.In_File,
         Ada.Strings.Unbounded.To_String (P_File_Name));

      Read_Stream := Ada.Streams.Stream_IO.Stream (Read_File);

      Integer'Read (Read_Stream, Next_Id);

      Ada.Containers.Count_Type'Read (Read_Stream, Num_Of_Pieces);

      for Trav in 1 .. Num_Of_Pieces loop
         --
         Piece.Type_Piece_Id'Read (Read_Stream, Piece_Id);
         Piece.Type_Category'Read (Read_Stream, A_Category);
         if A_Category = Piece.House_Piece then
            An_Element.Actual_Piece :=
              new Piece.Server.Type_Piece'Class'
                (Piece.Server.House_Piece.House_Class.all);

         elsif A_Category = Piece.Fighting_Piece then
            An_Element.Actual_Piece :=
              new Piece.Server.Type_Piece'Class'
                (Piece.Server.Fighting_Piece.Piece_Class.all);

         end if;
         Piece.Type_Piece'Class'Read
           (Read_Stream,
            Piece.Type_Piece'Class (An_Element.Actual_Piece.all));

         Hexagon.Type_Hexagon_Position'Read
           (Read_Stream,
            An_Element.Actual_Pos);
         Pieces_Server_List.Append (P_Piece_List, An_Element);

         -- Add piece to Landscape.Patch:
         A_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (An_Element.Actual_Pos.A,
              An_Element.Actual_Pos.B);
         Landscape.Pieces_Here_List.Append
           (A_Patch.all.Pieces_Here,
            An_Element.Actual_Piece.all.Id);
         Landscape.Pieces_Here_Sort.Sort
           (A_Patch.all
              .Pieces_Here); -- this sorting will be done too many times - but I don't care for now.
      end loop;

      Ada.Streams.Stream_IO.Close (Read_File);

      if Verbose then
         Text_IO.Put_Line ("Piece.Server.Load_Pieces - exit");
      end if;
   end Load_Pieces;

end Piece.Server;
