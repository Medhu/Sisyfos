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

with Piece.Server;
with Piece.Server.Fighting_Piece;
with Hexagon.Server_Map;
with Text_IO;
with Construction.Server;
with Landscape.Server;
with Hexagon.Area;
use Hexagon.Area;

package body Hexagon.Utility is

   Verbose : constant Boolean := False;

   function ID_Hashed
     (id : Hexagon.Type_Hexagon_Position) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Integer (id.A) * 1000 + Integer (id.B));
   end ID_Hashed;

   function Hexagon_Distance
     (P_From, P_To : in Hexagon.Type_Hexagon_Position) return Integer
   is
      A, B, AB : Integer;
   begin
      -- A bit of analysis on how the hexagon geometry behave is
      -- needed to understand this logic.
      --
      A  := abs (Integer (P_To.A - P_From.A));
      B  := abs (Integer (P_To.B - P_From.B));
      AB := abs (Integer ((P_To.A - P_From.A) + (P_To.B - P_From.B)));

      if A <= AB and then B <= AB then
         return A + B;
      else
         if B <= A then
            return AB + B;
         else
            return AB + A;
         end if;
      end if;

   end Hexagon_Distance;

   procedure Find_Path
     (P_Player_Id       : in     Player.Type_Player_Id;
      P_Type_Piece_Type : in     Piece.Type_Piece_Type;
      P_From, P_To      : in     Hexagon.Type_Hexagon_Position;
      P_Status          :    out Status.Type_Status;
      P_Path            :    out Hexagon.Path.Vector)
   is

      Open_List, Closed_List : Action_Capability_Vector.Map;

      Neigbour_Patch,
      Current_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Current, Neighbour   : Hexagon.Type_Hexagon_Position;
      Trav_Open, Trav_Path : Action_Capability_Vector.Cursor;

      Min_So_Far           : Type_Path_Node;
      Min_So_Far_Cursor, N : Action_Capability_Vector.Cursor;

      Solution_Found : Boolean;
      Done           : Boolean;
      F, G, H        : Integer;

      type Type_Neighbour_Info is record
         Delta_Pos                 : Hexagon.Area.Type_Hexagon_Delta_Position;
         Opposite_Neighbour_Number : Positive; -- This is the index that the neighbour patch has to refer back to Current_Patch
         -- We use this to be able to check for blocking constructions on the way into neighbour patch
      end record;

      type Type_Neighbours is
        array
        (1 ..
             6) of Type_Neighbour_Info; -- Hexagon.Area.Type_Hexagon_Delta_Position;
      Neighbours : Type_Neighbours :=
        (((True, +1, 0), 4),
         ((True, +1, -1), 5),
         ((True, 0, -1), 6),
         ((True, -1, 0), 1),
         ((True, -1, +1), 2),
         ((True, 0, +1), 3));
      use Action_Capability_Vector;

      use Status;
      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Hexagon.Utility.Find_Path - enter P_From_A=" &
            P_From.A'Img &
            " P_From_B=" &
            P_From.B'Img &
            " P_To_A=" &
            P_To.A'Img &
            " P_To_B=" &
            P_To.B'Img);
      end if;

      -- start point
      Action_Capability_Vector.Insert
        (Open_List,
         P_From,
         Type_Path_Node'(P_From, 4, 0, 4, P_From));

      Solution_Found := False;
      while not Solution_Found loop
         -- Find most promising open patch so far
         Trav_Open := Action_Capability_Vector.First (Open_List);
         if Action_Capability_Vector.Has_Element (Trav_Open) then
            Min_So_Far := Action_Capability_Vector.Element (Trav_Open);

            while Trav_Open /= Action_Capability_Vector.No_Element loop
               if Action_Capability_Vector.Element (Trav_Open).F <=
                 Min_So_Far.F
               then
                  Min_So_Far_Cursor := Trav_Open;
                  Min_So_Far := Action_Capability_Vector.Element (Trav_Open);
                  Current           := Min_So_Far.Position;
               end if;

               Trav_Open := Action_Capability_Vector.Next (Trav_Open);
            end loop;

            if Min_So_Far.H = 0 then
               Solution_Found := True;
               P_Status       := Status.Ok;
            else
               Current_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB
                   (Current.A,
                    Current.B);
               -- Find nearby patches
               for Trav in Neighbours'First .. Neighbours'Last loop

                  if not Construction.Server.Is_Blocking_Neighbour_Number
                      (Current_Patch.all.Constructions_Here,
                       Trav)
                  then
                     begin
                        Neighbour :=
                          Hexagon.Type_Hexagon_Position'
                            (True,
                             Hexagon.Type_Hexagon_Numbers
                               (Integer (Current.A) +
                                Integer (Neighbours (Trav).Delta_Pos.A)),
                             Hexagon.Type_Hexagon_Numbers
                               (Integer (Current.B) +
                                Integer (Neighbours (Trav).Delta_Pos.B)));

                        Neigbour_Patch :=
                          Hexagon.Server_Map.Get_Patch_Adress_From_AB
                            (Neighbour.A,
                             Neighbour.B);
                        --
                        -- Walls: When you are about to enter a new patch - check if there is a wall in the
                        -- new patch that is in the way
                        --
                        if Construction.Server.Is_Blocking_Neighbour_Number
                            (Neigbour_Patch.all.Constructions_Here,
                             Neighbours (Trav).Opposite_Neighbour_Number)
                        then
                           Neigbour_Patch :=
                             null; -- not allowed to pass this way
                        end if;

                     exception
                        when others =>
                           Neigbour_Patch := null;
                     end;
                  end if;

                  if Neigbour_Patch /= null then
                     -- if this new patch is not in closed list, then it can be
                     --analyzed
                     -- or if the piece is not passable
                     if
                       ((Action_Capability_Vector.Find
                           (Closed_List,
                            Neighbour) =
                         Action_Capability_Vector.No_Element))
                     then

                        -- For this new position, calculate F, G and H:
                        G := Min_So_Far.G + 1;
                        H := Hexagon_Distance (Neighbour, P_To);
                        F := G + H;

                        if
                          ((Landscape.Server.Has_Patch_Free_Slot
                              (Landscape.Type_Patch (Neigbour_Patch.all)) and
                            Piece.Server.Patch_Belongs_To_Player
                              (Landscape.Type_Patch (Neigbour_Patch.all),
                               P_Player_Id)) and
                           Piece.Server.Fighting_Piece.Can_Move_Here
                             (P_Type_Piece_Type,
                              Neigbour_Patch.all.Landscape_Here))
                        then

                           N :=
                             Action_Capability_Vector.Find
                               (Open_List,
                                Neighbour);
                           if N = Action_Capability_Vector.No_Element then
                              Action_Capability_Vector.Include
                                (Open_List,
                                 Neighbour,
                                 Type_Path_Node'(Neighbour, F, G, H, Current));
                           else
                              if Action_Capability_Vector.Element (N).G >
                                G
                              then
                                 Action_Capability_Vector.Include
                                   (Open_List,
                                    Neighbour,
                                    Type_Path_Node'
                                      (Neighbour, F, G, H, Current));

                              end if;
                           end if;

                        end if;

                     end if;
                  end if;

               end loop; -- Nearby patches

            end if;
            -- We dont need to analyse Current_Patch any more. Put it in closed
            --list
            Action_Capability_Vector.Insert
              (Closed_List,
               Action_Capability_Vector.Element (Min_So_Far_Cursor).Position,
               Action_Capability_Vector.Element (Min_So_Far_Cursor));

            Action_Capability_Vector.Delete (Open_List, Min_So_Far_Cursor);

         else
            P_Status       := Status.No_Path_Found;
            Solution_Found := True;
         end if;

      end loop;   --as long as there are available patches and we have not
      --found end patch yet.

      if P_Status = Status.Ok then
         Done      := False;
         Trav_Path :=
           Action_Capability_Vector.Find (Closed_List, Min_So_Far.Position);
         while Trav_Path /= Action_Capability_Vector.No_Element and not Done
         loop

            Hexagon.Path.Prepend
              (P_Path,
               Action_Capability_Vector.Element (Trav_Path).Position);

            if Action_Capability_Vector.Element (Trav_Path).Position.A /=
              Action_Capability_Vector.Element (Trav_Path).Parent.A or
              Action_Capability_Vector.Element (Trav_Path).Position.B /=
                Action_Capability_Vector.Element (Trav_Path).Parent.B
            then
               Trav_Path :=
                 Action_Capability_Vector.Find
                   (Closed_List,
                    Hexagon.Type_Hexagon_Position'
                      (True,
                       Action_Capability_Vector.Element (Trav_Path).Parent.A,
                       Action_Capability_Vector.Element (Trav_Path).Parent.B));
            else
               Done := True;
            end if;
         end loop;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Utility.Find_Path - exit");
      end if;
   end Find_Path;

   procedure Put_Line (P_Text : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Verbose then
         Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (P_Text));
      end if;
   end Put_Line;

end Hexagon.Utility;
