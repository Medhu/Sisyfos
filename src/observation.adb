--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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

package body Observation is
   Verbose : constant Boolean := False;

   package body Observation_Of_Patches is
      function Left_Less_Observation (Left, Right : in Type_Observed_Patch) return Boolean is
         use Hexagon;
      begin
         return Integer (Left.A) * 10000000 + Integer (Left.B) * 10000 <
           Integer (Right.A) * 10000000 + Integer (Right.B) * 10000;
      end Left_Less_Observation;

      function Equal_Observation (Left, Right : in Type_Observed_Patch) return Boolean is
      begin
         return Left = Right;
      end Equal_Observation;
      procedure Find_Delta_Observations
        (P_Current, P_Previous : in     Observations_Of_Patches.Set;
         P_Observations        :    out Changes_To_Patches.Vector)
      is

         In_Both, In_Current_Only, In_Previous_Only : Observations_Of_Patches.Set;
         Add                                        : Observations_Of_Patches.Cursor;
         Not_Observed, Observed                     : Type_Observed_Patch;

         procedure Find_Delta_Left_Not_In_Right
           (P_Left, P_Right : in     Observations_Of_Patches.Set;
            P_Delta         :    out Observations_Of_Patches.Set)
         is
            Left  : Observations_Of_Patches.Cursor;
            Right : Observations_Of_Patches.Cursor;
            Found : Boolean;

            use Hexagon;
         begin
            Left := Observations_Of_Patches.First (P_Left);
            while Observations_Of_Patches.Has_Element (Left) loop

               Right := Observations_Of_Patches.First (P_Right);
               Found := False;
               while Observations_Of_Patches.Has_Element (Right) and not Found loop
                  if Observations_Of_Patches.Element (Left).A =
                    Observations_Of_Patches.Element (Right).A and
                    Observations_Of_Patches.Element (Left).B =
                      Observations_Of_Patches.Element (Right).B
                  then
                     Found := True;
                  end if;
                  Right := Observations_Of_Patches.Next (Right);
               end loop;

               if not Found then
                  Observations_Of_Patches.Include (P_Delta, Observations_Of_Patches.Element (Left));
               end if;

               Left := Observations_Of_Patches.Next (Left);
            end loop;
         end Find_Delta_Left_Not_In_Right;

      begin
         Changes_To_Patches.Clear (P_Observations);
         Observations_Of_Patches.Clear (In_Both);
         Observations_Of_Patches.Clear (In_Previous_Only);
         Observations_Of_Patches.Clear (In_Current_Only);

         Find_Delta_Left_Not_In_Right (P_Previous, P_Current, In_Previous_Only);
         Find_Delta_Left_Not_In_Right (P_Current, P_Previous, In_Current_Only);

         Add := Observations_Of_Patches.First (In_Previous_Only);
         while Observations_Of_Patches.Has_Element (Add) loop
            Not_Observed :=
              Type_Observed_Patch'
                (Observations_Of_Patches.Element (Add).A,
                 Observations_Of_Patches.Element (Add).B,
                 False);

            Changes_To_Patches.Append (P_Observations, Not_Observed);

            Add := Observations_Of_Patches.Next (Add);
         end loop;

         Add := Observations_Of_Patches.First (In_Current_Only);
         while Observations_Of_Patches.Has_Element (Add) loop
            Observed :=
              Type_Observed_Patch'
                (Observations_Of_Patches.Element (Add).A,
                 Observations_Of_Patches.Element (Add).B,
                 True);
            Changes_To_Patches.Append (P_Observations, Observations_Of_Patches.Element (Add));

            Add := Observations_Of_Patches.Next (Add);
         end loop;

      end Find_Delta_Observations;

      procedure Print_Pieces_Observed_Patches
        (P_Observed_Patches : in Observations_Of_Patches.Set)
      is
         Current_Observation_Cursor : Observations_Of_Patches.Cursor;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Patches.Length (P_Observed_Patches)'Img);
         Current_Observation_Cursor := Observations_Of_Patches.First (P_Observed_Patches);

         while Observations_Of_Patches.Has_Element (Current_Observation_Cursor) loop
            Text_IO.Put_Line
              (">Observed Patch: " &
               " A=" &
               Observations_Of_Patches.Element (Current_Observation_Cursor).A'Img &
               " B=" &
               Observations_Of_Patches.Element (Current_Observation_Cursor).B'Img --&
               );

            Observations_Of_Patches.Next (Current_Observation_Cursor);
         end loop;
      end Print_Pieces_Observed_Patches;
   end Observation_Of_Patches;

   package body Observation_Of_Pieces is
      function Equal_Observed_Pieces (Left, Right : in Type_Observed_Piece) return Boolean is
      begin
         return Left = Right;
      end Equal_Observed_Pieces;

      function Left_Less_Observed_Pieces (Left, Right : in Type_Observed_Piece) return Boolean is
      begin
         return Integer (Left.Pos.A) * 10000000 +
           Integer (Left.Pos.B) * 10000 +
           Integer (Left.Piece_Here_Id) * 10 <
           Integer (Right.Pos.A) * 10000000 +
             Integer (Right.Pos.B) * 10000 +
             Integer (Right.Piece_Here_Id) * 10;
      end Left_Less_Observed_Pieces;

      procedure Find_Delta_Observed_Pieces
        (P_Current, P_Previous : in     Observations_Of_Pieces.Set;
         P_Observed_Pieces     :    out Changes_To_Pieces.Vector)
      is

         Found              : Boolean;
         Cursor_Current     : Observations_Of_Pieces.Cursor;
         Cursor_Previous    : Observations_Of_Pieces.Cursor;
         Curr_Obs, Prev_Obs : Type_Observed_Piece;

         use Piece;
      begin
         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces - enter");
         end if;

         Changes_To_Pieces.Clear (P_Observed_Pieces);

         Cursor_Current := Observations_Of_Pieces.First (P_Current);
         while Observations_Of_Pieces.Has_Element (Cursor_Current) loop

            Curr_Obs := Observations_Of_Pieces.Element (Cursor_Current);

            Found           := False;
            Cursor_Previous := Observations_Of_Pieces.First (P_Previous);
            while Observations_Of_Pieces.Has_Element (Cursor_Previous) and not Found loop
               Prev_Obs := Observations_Of_Pieces.Element (Cursor_Previous);

               if Prev_Obs.Piece_Here_Id = Curr_Obs.Piece_Here_Id then

                  if Prev_Obs /= Curr_Obs then
                     Changes_To_Pieces.Append (P_Observed_Pieces, Curr_Obs);
                  end if;
                  Found := True;
               end if;

               Cursor_Previous := Observations_Of_Pieces.Next (Cursor_Previous);
            end loop;

            if not Found then
               Changes_To_Pieces.Append
                 (P_Observed_Pieces,
                  Type_Observed_Piece'(Curr_Obs.Pos, Curr_Obs.Piece_Here_Id));
            end if;

            Cursor_Current := Observations_Of_Pieces.Next (Cursor_Current);
         end loop;

         Cursor_Previous := Observations_Of_Pieces.First (P_Previous);
         while Observations_Of_Pieces.Has_Element (Cursor_Previous) loop

            Prev_Obs := Observations_Of_Pieces.Element (Cursor_Previous);

            Found          := False;
            Cursor_Current := Observations_Of_Pieces.First (P_Current);
            while Observations_Of_Pieces.Has_Element (Cursor_Current) loop
               Curr_Obs := Observations_Of_Pieces.Element (Cursor_Current);

               if Prev_Obs.Piece_Here_Id = Curr_Obs.Piece_Here_Id then

                  Found := True;
               end if;

               Cursor_Current := Observations_Of_Pieces.Next (Cursor_Current);
            end loop;

            if not Found then
               Changes_To_Pieces.Append
                 (P_Observed_Pieces,
                  Type_Observed_Piece'
                    (Hexagon.Type_Hexagon_Position'(P_Valid => False), Prev_Obs.Piece_Here_Id));
            end if;

            Cursor_Previous := Observations_Of_Pieces.Next (Cursor_Previous);
         end loop;

         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Pieces.Find_Delta_Observed_Pieces - exit");
         end if;

      end Find_Delta_Observed_Pieces;

      function Find_Piece_Id
        (P_Observed_Pieces : in Observations_Of_Pieces.Set;
         P_Piece_Id        :    Piece.Type_Piece_Id) return Boolean
      is
         Trav  : Observations_Of_Pieces.Cursor;
         Found : Boolean := False;

         use Piece;
      begin
         Trav := Observations_Of_Pieces.First (P_Observed_Pieces);
         while Observations_Of_Pieces.Has_Element (Trav) and not Found loop

            if Observations_Of_Pieces.Element (Trav).Piece_Here_Id = P_Piece_Id then
               Found := True;
            end if;

            Trav := Observations_Of_Pieces.Next (Trav);
         end loop;

         return Found;
      end Find_Piece_Id;

      procedure Print_Pieces_Observed_Pieces (P_Observed_Pieces : in Observations_Of_Pieces.Set) is
         Current_Observation_Cursor : Observations_Of_Pieces.Cursor;
         Current_Piece_Id           : Piece.Type_Piece_Id;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Pieces.Length (P_Observed_Pieces)'Img);
         Current_Observation_Cursor := Observations_Of_Pieces.First (P_Observed_Pieces);

         while Observations_Of_Pieces.Has_Element (Current_Observation_Cursor) loop
            Current_Piece_Id :=
              Observations_Of_Pieces.Element (Current_Observation_Cursor).Piece_Here_Id;

            Text_IO.Put_Line
              (">Observed Piece " &
               " A=" &
               Observations_Of_Pieces.Element (Current_Observation_Cursor).Pos.A'Img &
               " B=" &
               Observations_Of_Pieces.Element (Current_Observation_Cursor).Pos.B'Img &
               " Piece.Id=" &
               Current_Piece_Id'Img);

            Observations_Of_Pieces.Next (Current_Observation_Cursor);
         end loop;

      end Print_Pieces_Observed_Pieces;
   end Observation_Of_Pieces;

   package body Observation_Of_Pieces_Info is
      function Equal_Observed_Pieces_Info
        (Left, Right : in Type_Observed_Piece_Info) return Boolean
      is
      begin
         return Left = Right;
      end Equal_Observed_Pieces_Info;

      function Left_Less_Observed_Pieces_Info
        (Left, Right : in Type_Observed_Piece_Info) return Boolean
      is
         use Piece;
      begin
         return Left.Piece_Here.Id < Right.Piece_Here.Id;
      end Left_Less_Observed_Pieces_Info;

      procedure Find_Delta_Observed_Pieces_Info
        (P_Current, P_Previous  : in     Observations_Of_Pieces_Info.Set;
         P_Observed_Pieces_Info :    out Changes_To_Pieces_Info.Vector)
      is

         Found              : Boolean;
         Cursor_Current     : Observations_Of_Pieces_Info.Cursor;
         Cursor_Previous    : Observations_Of_Pieces_Info.Cursor;
         Curr_Obs, Prev_Obs : Type_Observed_Piece_Info;

         use Piece;
      begin
         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Pieces_Info.Find_Delta_Observed_Pieces_Info - enter");
         end if;

         Changes_To_Pieces_Info.Clear (P_Observed_Pieces_Info);

         Cursor_Current := Observations_Of_Pieces_Info.First (P_Current);
         while Observations_Of_Pieces_Info.Has_Element (Cursor_Current) loop

            Curr_Obs := Observations_Of_Pieces_Info.Element (Cursor_Current);

            Found           := False;
            Cursor_Previous := Observations_Of_Pieces_Info.First (P_Previous);
            while Observations_Of_Pieces_Info.Has_Element (Cursor_Previous) and not Found loop
               Prev_Obs := Observations_Of_Pieces_Info.Element (Cursor_Previous);

               if Prev_Obs.Piece_Here.Id = Curr_Obs.Piece_Here.Id then

                  if Prev_Obs /= Curr_Obs then
                     Changes_To_Pieces_Info.Append
                       (P_Observed_Pieces_Info,
                        Type_Observed_Piece_Info'(Curr_Obs.Piece_Here, True));
                  end if;
                  Found := True;
               end if;

               Cursor_Previous := Observations_Of_Pieces_Info.Next (Cursor_Previous);
            end loop;

            if not Found then
               Changes_To_Pieces_Info.Append
                 (P_Observed_Pieces_Info,
                  Type_Observed_Piece_Info'(Curr_Obs));
            end if;

            Cursor_Current := Observations_Of_Pieces_Info.Next (Cursor_Current);
         end loop;

         Cursor_Previous := Observations_Of_Pieces_Info.First (P_Previous);
         while Observations_Of_Pieces_Info.Has_Element (Cursor_Previous) loop

            Prev_Obs := Observations_Of_Pieces_Info.Element (Cursor_Previous);

            Found          := False;
            Cursor_Current := Observations_Of_Pieces_Info.First (P_Current);
            while Observations_Of_Pieces_Info.Has_Element (Cursor_Current) loop
               Curr_Obs := Observations_Of_Pieces_Info.Element (Cursor_Current);

               if Prev_Obs.Piece_Here.Id = Curr_Obs.Piece_Here.Id then

                  Found := True;
               end if;

               Cursor_Current := Observations_Of_Pieces_Info.Next (Cursor_Current);
            end loop;

            if not Found then
               Changes_To_Pieces_Info.Append
                 (P_Observed_Pieces_Info,
                  Type_Observed_Piece_Info'(Prev_Obs.Piece_Here, False));
            end if;

            Cursor_Previous := Observations_Of_Pieces_Info.Next (Cursor_Previous);
         end loop;

         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Pieces_Info.Find_Delta_Observed_Pieces_Info - exit");
         end if;

      end Find_Delta_Observed_Pieces_Info;

      function Find_Piece_Id
        (P_Observed_Pieces_Info : in Observations_Of_Pieces_Info.Set;
         P_Piece_Id             :    Piece.Type_Piece_Id) return Boolean
      is
         Trav  : Observations_Of_Pieces_Info.Cursor;
         Found : Boolean := False;

         use Piece;
      begin
         Trav := Observations_Of_Pieces_Info.First (P_Observed_Pieces_Info);
         while Observations_Of_Pieces_Info.Has_Element (Trav) and not Found loop

            if Observations_Of_Pieces_Info.Element (Trav).Piece_Here.Id = P_Piece_Id then
               Found := True;
            end if;

            Trav := Observations_Of_Pieces_Info.Next (Trav);
         end loop;

         return Found;
      end Find_Piece_Id;

      procedure Print_Pieces_Observed_Pieces_Info
        (P_Observed_Pieces_Info : in Observations_Of_Pieces_Info.Set)
      is
         Current_Observation_Cursor : Observations_Of_Pieces_Info.Cursor;
         Current_Piece_Id           : Piece.Type_Piece_Id;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Pieces_Info.Length (P_Observed_Pieces_Info)'Img);
         Current_Observation_Cursor := Observations_Of_Pieces_Info.First (P_Observed_Pieces_Info);

         while Observations_Of_Pieces_Info.Has_Element (Current_Observation_Cursor) loop
            Current_Piece_Id :=
              Observations_Of_Pieces_Info.Element (Current_Observation_Cursor).Piece_Here.Id;

            Text_IO.Put_Line (">Observed Piece_Info " & " Piece.Id=" & Current_Piece_Id'Img);

            Observations_Of_Pieces_Info.Next (Current_Observation_Cursor);
         end loop;

      end Print_Pieces_Observed_Pieces_Info;
   end Observation_Of_Pieces_Info;

   package body Observation_Of_Pieces_Effects is

      function Left_Less_Effect (Left, Right : in Type_Piece_Effect) return Boolean is
         use Hexagon;

         Major_Left, Major_Right, Minor_Left, Minor_Right : Integer;
      begin
         Major_Left :=
           Integer (Left.Piece_Id) * 100000 + Positive (Left.Effect_Info.Effect_Name) * 100;

         Major_Right :=
           Integer (Right.Piece_Id) * 100000 + Positive (Right.Effect_Info.Effect_Name) * 100;

         Minor_Left  := Left.Effect_Info.Aux;
         Minor_Right := Right.Effect_Info.Aux;

         if Major_Left < Major_Right then
            return True;
         elsif Major_Left > Major_Right then
            return False;
         else -- Major equal equal
            if Minor_Left < Minor_Right then
               return True;
            elsif Minor_Left > Minor_Right then
               return False;
            else
               return False;
            end if;
         end if;
      end Left_Less_Effect;

      function Equal_Effect (Left, Right : in Type_Piece_Effect) return Boolean is
      begin
         return Left = Right;
      end Equal_Effect;

      procedure Print_Pieces_Effect (P_Player_Pieces : in Observations_Of_Pieces_Effects.Set) is
         Current_Piece_Cursor : Observations_Of_Pieces_Effects.Cursor;
         Current_Piece_Id     : Piece.Type_Piece_Id;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Pieces_Effects.Length (P_Player_Pieces)'Img);
         Current_Piece_Cursor := Observations_Of_Pieces_Effects.First (P_Player_Pieces);

         while Observations_Of_Pieces_Effects.Has_Element (Current_Piece_Cursor) loop
            Current_Piece_Id :=
              Observations_Of_Pieces_Effects.Element (Current_Piece_Cursor).Piece_Id;
            Text_IO.Put_Line
              (">Find_Piece id:" &
               Current_Piece_Id'Img &
               " Information=" &
               Observations_Of_Pieces_Effects.Element (Current_Piece_Cursor).Effect_Info
                 .Effect_Name'
                 Img &
               " Aux=" &
               Observations_Of_Pieces_Effects.Element (Current_Piece_Cursor).Effect_Info.Aux'Img);

            Observations_Of_Pieces_Effects.Next (Current_Piece_Cursor);
         end loop;
      end Print_Pieces_Effect;

      procedure Find_Delta_Effect
        (P_Current, P_Previous : in     Observations_Of_Pieces_Effects.Set;
         P_Effects             :    out Changes_To_Pieces_Effects.Vector)
      is

         In_Current_Only, In_Previous_Only : Observations_Of_Pieces_Effects.Set;
         Add                               : Observations_Of_Pieces_Effects.Cursor;

         procedure Find_Delta_Left_Not_In_Right
           (P_Left, P_Right : in     Observations_Of_Pieces_Effects.Set;
            P_Delta         :    out Observations_Of_Pieces_Effects.Set)
         is
            Left  : Observations_Of_Pieces_Effects.Cursor;
            Right : Observations_Of_Pieces_Effects.Cursor;
            Found : Boolean;

            use Piece;
            use Effect;
         begin
            Left := Observations_Of_Pieces_Effects.First (P_Left);
            while Observations_Of_Pieces_Effects.Has_Element (Left) loop

               Right := Observations_Of_Pieces_Effects.First (P_Right);
               Found := False;
               while Observations_Of_Pieces_Effects.Has_Element (Right) and not Found loop
                  if Observations_Of_Pieces_Effects.Element (Left).Piece_Id =
                    Observations_Of_Pieces_Effects.Element (Right).Piece_Id and
                    Observations_Of_Pieces_Effects.Element (Left).Effect_Info =
                      Observations_Of_Pieces_Effects.Element (Right).Effect_Info
                  then
                     Found := True;
                  end if;
                  Right := Observations_Of_Pieces_Effects.Next (Right);
               end loop;

               if not Found then
                  Observations_Of_Pieces_Effects.Insert
                    (P_Delta,
                     Observations_Of_Pieces_Effects.Element (Left));
               end if;

               Left := Observations_Of_Pieces_Effects.Next (Left);
            end loop;
         end Find_Delta_Left_Not_In_Right;

      begin
         Changes_To_Pieces_Effects.Clear (P_Effects);
         Observations_Of_Pieces_Effects.Clear (In_Previous_Only);
         Observations_Of_Pieces_Effects.Clear (In_Current_Only);

         Find_Delta_Left_Not_In_Right (P_Previous, P_Current, In_Previous_Only);
         Find_Delta_Left_Not_In_Right (P_Current, P_Previous, In_Current_Only);

         Add := Observations_Of_Pieces_Effects.First (In_Current_Only);
         while Observations_Of_Pieces_Effects.Has_Element (Add) loop
            Changes_To_Pieces_Effects.Append
              (P_Effects,
               Type_Piece_Effect'
                 (Observations_Of_Pieces_Effects.Element (Add).Piece_Id,
                  Observations_Of_Pieces_Effects.Element (Add).Effect_Info,
                  True));

            Add := Observations_Of_Pieces_Effects.Next (Add);
         end loop;

         Add := Observations_Of_Pieces_Effects.First (In_Previous_Only);
         while Observations_Of_Pieces_Effects.Has_Element (Add) loop
            Changes_To_Pieces_Effects.Append
              (P_Effects,
               Type_Piece_Effect'
                 (Observations_Of_Pieces_Effects.Element (Add).Piece_Id,
                  Observations_Of_Pieces_Effects.Element (Add).Effect_Info,
                  False));

            Add := Observations_Of_Pieces_Effects.Next (Add);
         end loop;

      end Find_Delta_Effect;

   end Observation_Of_Pieces_Effects;

   package body Observation_Of_Patches_Effects is

      function Left_Less_Effect (Left, Right : in Type_Patch_Effect) return Boolean is
         use Hexagon;
      begin

         return Integer (Left.Pos.A) * 1000000 +
           Integer (Left.Pos.B) * 10000 +
           Positive (Left.Effect_Info.Effect_Name) * 100 +
           Left.Effect_Info.Aux * 10 <
           Integer (Right.Pos.A) * 1000000 +
             Integer (Right.Pos.B) * 10000 +
             Positive (Right.Effect_Info.Effect_Name) * 100 +
             Right.Effect_Info.Aux * 10;
      end Left_Less_Effect;

      function Equal_Effect (Left, Right : in Type_Patch_Effect) return Boolean is
      begin
         return Left = Right;
      end Equal_Effect;

      procedure Print_Patches_Effect (P_Player_Patches : in Observations_Of_Patches_Effects.Set) is
         Current_Patch_Cursor : Observations_Of_Patches_Effects.Cursor;
         Current_Pos          : Hexagon.Type_Hexagon_Position;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Patches_Effects.Length (P_Player_Patches)'Img);
         Current_Patch_Cursor := Observations_Of_Patches_Effects.First (P_Player_Patches);

         while Observations_Of_Patches_Effects.Has_Element (Current_Patch_Cursor) loop
            Current_Pos := Observations_Of_Patches_Effects.Element (Current_Patch_Cursor).Pos;
            Text_IO.Put_Line
              (">Find_Patch Id:" &
               Current_Pos.A'Img &
               " " &
               Current_Pos.B'Img &
               " Information=" &
               Observations_Of_Patches_Effects.Element (Current_Patch_Cursor).Effect_Info
                 .Effect_Name'
                 Img &
               " Aux=" &
               Observations_Of_Patches_Effects.Element (Current_Patch_Cursor).Effect_Info.Aux'Img);

            Observations_Of_Patches_Effects.Next (Current_Patch_Cursor);
         end loop;
      end Print_Patches_Effect;

      procedure Find_Delta_Effect
        (P_Current, P_Previous : in     Observations_Of_Patches_Effects.Set;
         P_Effects             :    out Changes_To_Patches_Effects.Vector)
      is

         In_Current_Only, In_Previous_Only : Observations_Of_Patches_Effects.Set;
         Add                               : Observations_Of_Patches_Effects.Cursor;

         procedure Find_Delta_Left_Not_In_Right
           (P_Left, P_Right : in     Observations_Of_Patches_Effects.Set;
            P_Delta         :    out Observations_Of_Patches_Effects.Set)
         is
            Left  : Observations_Of_Patches_Effects.Cursor;
            Right : Observations_Of_Patches_Effects.Cursor;
            Found : Boolean;

            use Hexagon;
            use Effect;
         begin
            Left := Observations_Of_Patches_Effects.First (P_Left);
            while Observations_Of_Patches_Effects.Has_Element (Left) loop

               Right := Observations_Of_Patches_Effects.First (P_Right);
               Found := False;
               while Observations_Of_Patches_Effects.Has_Element (Right) and not Found loop
                  if Observations_Of_Patches_Effects.Element (Left).Pos =
                    Observations_Of_Patches_Effects.Element (Right).Pos and
                    Observations_Of_Patches_Effects.Element (Left).Effect_Info =
                      Observations_Of_Patches_Effects.Element (Right).Effect_Info
                  then
                     Found := True;
                  end if;
                  Right := Observations_Of_Patches_Effects.Next (Right);
               end loop;

               if not Found then
                  Observations_Of_Patches_Effects.Insert
                    (P_Delta,
                     Observations_Of_Patches_Effects.Element (Left));
               end if;

               Left := Observations_Of_Patches_Effects.Next (Left);
            end loop;
         end Find_Delta_Left_Not_In_Right;

      begin
         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Patches_Effects.Find_Delta_Effect -  enter");
         end if;

         Changes_To_Patches_Effects.Clear (P_Effects);
         Observations_Of_Patches_Effects.Clear (In_Previous_Only);
         Observations_Of_Patches_Effects.Clear (In_Current_Only);

         Find_Delta_Left_Not_In_Right (P_Previous, P_Current, In_Previous_Only);
         Find_Delta_Left_Not_In_Right (P_Current, P_Previous, In_Current_Only);

         Add := Observations_Of_Patches_Effects.First (In_Current_Only);
         while Observations_Of_Patches_Effects.Has_Element (Add) loop
            Changes_To_Patches_Effects.Append
              (P_Effects,
               Type_Patch_Effect'
                 (Observations_Of_Patches_Effects.Element (Add).Pos,
                  Observations_Of_Patches_Effects.Element (Add).Effect_Info,
                  True));

            Add := Observations_Of_Patches_Effects.Next (Add);
         end loop;

         Add := Observations_Of_Patches_Effects.First (In_Previous_Only);
         while Observations_Of_Patches_Effects.Has_Element (Add) loop
            Changes_To_Patches_Effects.Append
              (P_Effects,
               Type_Patch_Effect'
                 (Observations_Of_Patches_Effects.Element (Add).Pos,
                  Observations_Of_Patches_Effects.Element (Add).Effect_Info,
                  False));

            Add := Observations_Of_Patches_Effects.Next (Add);
         end loop;

         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Patches_Effects.Find_Delta_Effect -  exit");
         end if;
      end Find_Delta_Effect;

   end Observation_Of_Patches_Effects;

   package body Observation_Of_Construction is

      function Left_Less_Construction (Left, Right : in Type_Patch_Construction) return Boolean is
         use Hexagon;
      begin
         return Integer (Left.Pos.A) * 1000000 +
           Integer (Left.Pos.B) * 10000 +
           Positive (Left.Construction_Info) * 100 <
           Integer (Right.Pos.A) * 1000000 +
             Integer (Right.Pos.B) * 10000 +
             Positive (Right.Construction_Info) * 100;
      end Left_Less_Construction;

      function Equal_Construction (Left, Right : in Type_Patch_Construction) return Boolean is
      begin
         return Left = Right;
      end Equal_Construction;

      procedure Print_Patches_Construction
        (P_Player_Patches : in Observations_Of_Construction.Set)
      is
         Current_Patch_Cursor : Observations_Of_Construction.Cursor;
         Current_Pos          : Hexagon.Type_Hexagon_Position;
      begin
         Text_IO.Put_Line
           ("Find_Piece length:" & Observations_Of_Construction.Length (P_Player_Patches)'Img);
         Current_Patch_Cursor := Observations_Of_Construction.First (P_Player_Patches);

         while Observations_Of_Construction.Has_Element (Current_Patch_Cursor) loop
            Current_Pos := Observations_Of_Construction.Element (Current_Patch_Cursor).Pos;
            Text_IO.Put_Line
              (">Find_Patch Id:" &
               Current_Pos.A'Img &
               " " &
               Current_Pos.B'Img &
               " Information=" &
               Observations_Of_Construction.Element (Current_Patch_Cursor).Construction_Info'Img);

            Observations_Of_Construction.Next (Current_Patch_Cursor);
         end loop;
      end Print_Patches_Construction;

      procedure Find_Delta_Construction
        (P_Current, P_Previous : in     Observations_Of_Construction.Set;
         P_Construction        :    out Changes_To_Construction.Vector)
      is

         In_Current_Only, In_Previous_Only : Observations_Of_Construction.Set;
         Add                               : Observations_Of_Construction.Cursor;

         procedure Find_Delta_Left_Not_In_Right
           (P_Left, P_Right : in     Observations_Of_Construction.Set;
            P_Delta         :    out Observations_Of_Construction.Set)
         is
            Left  : Observations_Of_Construction.Cursor;
            Right : Observations_Of_Construction.Cursor;
            Found : Boolean;

            use Hexagon;
            use Construction;
         begin
            Left := Observations_Of_Construction.First (P_Left);
            while Observations_Of_Construction.Has_Element (Left) loop

               Right := Observations_Of_Construction.First (P_Right);
               Found := False;
               while Observations_Of_Construction.Has_Element (Right) and not Found loop
                  if Observations_Of_Construction.Element (Left).Pos =
                    Observations_Of_Construction.Element (Right).Pos and
                    Observations_Of_Construction.Element (Left).Construction_Info =
                      Observations_Of_Construction.Element (Right).Construction_Info
                  then
                     Found := True;
                  end if;
                  Right := Observations_Of_Construction.Next (Right);
               end loop;

               if not Found then
                  Observations_Of_Construction.Insert
                    (P_Delta,
                     Observations_Of_Construction.Element (Left));
               end if;

               Left := Observations_Of_Construction.Next (Left);
            end loop;
         end Find_Delta_Left_Not_In_Right;

      begin
         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Construction.Find_Delta_Construction -  enter");
         end if;

         Changes_To_Construction.Clear (P_Construction);
         Observations_Of_Construction.Clear (In_Previous_Only);
         Observations_Of_Construction.Clear (In_Current_Only);

         Find_Delta_Left_Not_In_Right (P_Previous, P_Current, In_Previous_Only);
         Find_Delta_Left_Not_In_Right (P_Current, P_Previous, In_Current_Only);

         Add := Observations_Of_Construction.First (In_Current_Only);
         while Observations_Of_Construction.Has_Element (Add) loop
            Changes_To_Construction.Append
              (P_Construction,
               Type_Patch_Construction'
                 (Observations_Of_Construction.Element (Add).Pos,
                  Observations_Of_Construction.Element (Add).Construction_Info,
                  True));

            Add := Observations_Of_Construction.Next (Add);
         end loop;

         Add := Observations_Of_Construction.First (In_Previous_Only);
         while Observations_Of_Construction.Has_Element (Add) loop
            Changes_To_Construction.Append
              (P_Construction,
               Type_Patch_Construction'
                 (Observations_Of_Construction.Element (Add).Pos,
                  Observations_Of_Construction.Element (Add).Construction_Info,
                  False));

            Add := Observations_Of_Construction.Next (Add);
         end loop;

         if Verbose then
            Text_IO.Put_Line
              ("Observation.Observation_Of_Construction.Find_Delta_Construction -  exit");
         end if;
      end Find_Delta_Construction;

   end Observation_Of_Construction;

   package body Activity is
      function Equal (Left, Right : in Type_Activity_Report) return Boolean is
      begin
         return Left = Right;
      end Equal;
   end Activity;

   package body Frames is
      procedure Clear_Frames (P_Piece_Visibility_Frames : in out Piece_Visibility_Frames.Vector) is
         An_Element  : Type_Visibility_Frames;
         Trav_Frames : Piece_Visibility_Frames.Cursor;
      begin
         if Verbose then
            Text_IO.Put_Line
              ("Observation.Frames.Clear_Frames - enter Length=" &
               Piece_Visibility_Frames.Length (P_Piece_Visibility_Frames)'Img);
         end if;

         Trav_Frames := Piece_Visibility_Frames.First (P_Piece_Visibility_Frames);
         while Piece_Visibility_Frames.Has_Element (Trav_Frames) loop
            An_Element := Piece_Visibility_Frames.Element (Trav_Frames);

            if Verbose then
               Text_IO.Put
                 (" Before Clear An_Element.Observed_Patches=" &
                  Observation_Of_Patches.Changes_To_Patches.Length (An_Element.Observed_Patches)'
                    Img);
               Text_IO.Put
                 (" An_Element.Observed_Pieces=" &
                  Observation_Of_Pieces.Changes_To_Pieces.Length (An_Element.Observed_Pieces)'Img);
               Text_IO.Put
                 (" An_Element.Pieces_Info=" &
                  Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Length (An_Element.Pieces_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Effects_Info=" &
                  Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Length
                    (An_Element.Pieces_Effects_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Patches_Effects_Info=" &
                  Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Length
                    (An_Element.Patches_Effects_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Constructions_Info=" &
                  Observation_Of_Construction.Changes_To_Construction.Length
                    (An_Element.Constructions_Info)'
                    Img);
               Text_IO.Put_Line
                 (" An_Element.Activites_Info=" &
                  Activity.Activity_Report.Length (An_Element.Activities_Info)'Img);
            end if;

            Observation_Of_Patches.Changes_To_Patches.Clear (An_Element.Observed_Patches);
            Observation_Of_Pieces.Changes_To_Pieces.Clear (An_Element.Observed_Pieces);
            Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Clear (An_Element.Pieces_Info);
            Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Clear
              (An_Element.Pieces_Effects_Info);
            Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Clear
              (An_Element.Patches_Effects_Info);
            Observation_Of_Construction.Changes_To_Construction.Clear
              (An_Element.Constructions_Info);
            Activity.Activity_Report.Clear (An_Element.Activities_Info);

            if Verbose then
               Text_IO.Put
                 (" After An_Element.Observed_Patches=" &
                  Observation_Of_Patches.Changes_To_Patches.Length (An_Element.Observed_Patches)'
                    Img);
               Text_IO.Put
                 (" An_Element.Observed_Pieces=" &
                  Observation_Of_Pieces.Changes_To_Pieces.Length (An_Element.Observed_Pieces)'Img);
               Text_IO.Put
                 (" An_Element.Pieces_Info=" &
                  Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Length (An_Element.Pieces_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Effects_Info=" &
                  Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects.Length
                    (An_Element.Pieces_Effects_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Patches_Effects_Info=" &
                  Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Length
                    (An_Element.Patches_Effects_Info)'
                    Img);
               Text_IO.Put
                 (" An_Element.Constructions_Info=" &
                  Observation_Of_Construction.Changes_To_Construction.Length
                    (An_Element.Constructions_Info)'
                    Img);
               Text_IO.Put_Line
                 (" An_Element.Activities_Info=" &
                  Activity.Activity_Report.Length (An_Element.Activities_Info)'Img);
            end if;

            Trav_Frames := Piece_Visibility_Frames.Next (Trav_Frames);
         end loop;
         --

         Piece_Visibility_Frames.Clear (P_Piece_Visibility_Frames);

         if Verbose then
            Text_IO.Put_Line
              ("Observation.Frames.Clear_Frames - exit Length=" &
               Piece_Visibility_Frames.Length (P_Piece_Visibility_Frames)'Img);
         end if;
      end Clear_Frames;
   end Frames;

end Observation;
