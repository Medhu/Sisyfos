--
--
--      Fantasy Client/Server logic. This logic is a part of both server and client of Fantasy.
--      Copyright (C) 2019  Frank J Jorgensen
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
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package body Hexagon.Server_Navigation.Modify is
   Verbose : constant Boolean := False;

   function Create_Navigation_Node
     (P_Navigation : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Id : in Type_Navigation_Node_Id;
      P_Pos : in Hexagon.Type_Hexagon_Position)
     return Hexagon.Server_Navigation.Type_Navigation_Node_Access
   is
      A_Navigation_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      A_Navigation_Node :=
        new Hexagon.Server_Navigation.Type_Navigation_Node'
          (P_Id, P_Pos, Navigation_Neighbours_List_Pkg.Empty_Set);

      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Append
        (P_Navigation.Navigation_Node_List, A_Navigation_Node);

      return A_Navigation_Node;
   end Create_Navigation_Node;

   procedure Add_Path_To_Neighbour
     (P_Navigation_Node    : in out Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id)
   is

      use Hexagon.Server_Navigation;
   begin
      Text_IO.Put_Line("Add_Path_To_Neighbour: P_Navigation_Node.Id:"
                       & P_Navigation_Node.Id'Img
                       & " P_Navigation_Node_Id:"
                       & P_Navigation_Node_Id'Img);

      Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Include
        (P_Navigation_Node.Neighbours, P_Navigation_Node_Id);

   end Add_Path_To_Neighbour;

   procedure Remove_Path_To_Neighbour
     (P_Navigation_Node    : in out Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id)
   is
      Trav  : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;
      Found : Boolean;

      use Hexagon.Server_Navigation;
   begin
      Found := False;
      Trav  :=
        Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First (P_Navigation_Node.Neighbours);
      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element (Trav) and not Found loop

         if Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element (Trav) = P_Navigation_Node_Id
         then
            Found := True;
         else
            Trav := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next (Trav);
         end if;

      end loop;

      if Found then
         Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Delete
           (P_Navigation_Node.Neighbours, Trav);
      end if;

   end Remove_Path_To_Neighbour;

   procedure Remove_Path_To_Neighbour
     (P_Navigation         : in     Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node    : in out Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Position           : in     Hexagon.Type_Hexagon_Position)
   is
      Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_Position);

      Remove_Path_To_Neighbour(P_Navigation_Node,
                               Navigation_Node_Access.all.Id);
   end Remove_Path_To_Neighbour;

   procedure Add_Path_To_Neighbour
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_From, P_To  : in     Hexagon.Type_Hexagon_Position)
   is
      From_Navigation_Node_Access,
        To_Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      From_Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_From);
      To_Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_To);

      Add_Path_To_Neighbour
        (From_Navigation_Node_Access.all,
         To_Navigation_Node_Access.all.Id);
   end Add_Path_To_Neighbour;

   procedure Remove_Path_To_Neighbour
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_From, P_To  : in     Hexagon.Type_Hexagon_Position)
   is
      From_Navigation_Node_Access,
        To_Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      From_Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_From);
      To_Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_To);

      Remove_Path_To_Neighbour
        (From_Navigation_Node_Access.all,
         To_Navigation_Node_Access.all.Id);
   end Remove_Path_To_Neighbour;


   procedure Remove_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id)
   is
   type Type_Navigation_Node_Id_List is
     array (1 .. 6) of Hexagon.Server_Navigation.Type_Navigation_Node_Id;

      Navigation_Node_Access  : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Navigation_Node_Cursor  : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;
      Navigation_Node_Id_List : Type_Navigation_Node_Id_List;
      Trav                    : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;
      N_Index                 : Positive;
      Neighbour               : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Navigation_Node_Id      : Hexagon.Server_Navigation.Type_Navigation_Node_Id;

   begin
      Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Id (P_Navigation, P_Navigation_Node_Id);
      Navigation_Node_Cursor := Hexagon.Server_Navigation.Find_Navigation_Node
        (P_Navigation, P_Navigation_Node_Id);
      Navigation_Node_Id := Navigation_Node_Access.all.Id;

      Navigation_Node_Id_List := (others => 0);
      Trav := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First(Navigation_Node_Access.all.Neighbours);
      N_Index := Navigation_Node_Id_List'First;
      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element(Trav) loop

         if Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element (Trav) then
            Navigation_Node_Id_List(N_Index) :=
              Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element (Trav);
         end if;

         Trav := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next (Trav);
         N_Index := N_Index + 1;
      end loop;

      --
      -- We have found the neighbours to the node that shall be deleted.
      -- Visit the neighbours, for each of the neighbour find its reference to the
      -- node we are going to delete.
      --

      for Index in Navigation_Node_Id_List'First .. Navigation_Node_Id_List'Last loop
         if Navigation_Node_Id_List(Index) /= 0 then
            Neighbour := Hexagon.Server_Navigation.Get_Navigation_Node_By_Id
              (P_Navigation, Navigation_Node_Id_List(Index) );

            Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
              (Neighbour.all, Navigation_Node_Id);
         end if;
      end loop;

      Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Clear (Navigation_Node_Access.all.Neighbours);
      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Delete
        (P_Navigation.Navigation_Node_List, Navigation_Node_Cursor);
   end Remove_Navigation_Node;

   procedure Add_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Position    : in     Hexagon.Type_Hexagon_Position)
   is
      Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      Navigation_Node_Access := new Hexagon.Server_Navigation.Type_Navigation_Node'
        (1, P_Position, Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Empty_Set);
      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Append
        (P_Navigation.Navigation_Node_List, Navigation_Node_Access);

   end Add_Navigation_Node;

   procedure Remove_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Position    : in     Hexagon.Type_Hexagon_Position)
   is
      Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      Navigation_Node_Access := Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
        (P_Navigation, P_Position);

      Hexagon.Server_Navigation.Modify.Remove_Navigation_Node(P_Navigation, Navigation_Node_Access.all.Id);

   end Remove_Navigation_Node;


end Hexagon.Server_Navigation.Modify;
