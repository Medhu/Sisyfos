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
with Ada.Containers.Hashed_Maps;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Landscape.Server;
with Hexagon.Server_Map;

package body Hexagon.Server_Navigation is
   Verbose : constant Boolean := False;

   procedure Write_Navigation_Node_Access
     (Stream :    not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Type_Navigation_Node_Access)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Write_Navigation_Node_Access - enter");
      end if;

      Hexagon.Server_Navigation.Type_Navigation_Node_Id'Write (Stream, Item.all.Id);
      Hexagon.Type_Hexagon_Position'Write (Stream, Item.all.Pos);
      Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Set'Write (Stream,
         Item.all.Neighbours);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Write_Navigation_Node_Access - exit");
      end if;
   end Write_Navigation_Node_Access;

   procedure Read_Navigation_Node_Access
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Type_Navigation_Node_Access)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Read_Navigation_Node_Access - enter");
      end if;

      Item := new Hexagon.Server_Navigation.Type_Navigation_Node;
      Hexagon.Server_Navigation.Type_Navigation_Node_Id'Read (Stream, Item.all.Id);
      Hexagon.Type_Hexagon_Position'Read (Stream, Item.all.Pos);
      Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Set'Read (Stream,
         Item.all.Neighbours);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Read_Navigation_Node_Access - exit");
      end if;
   end Read_Navigation_Node_Access;

   function Navigation_Node_Hash
     (P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (P_Navigation_Node_Id);
   end Navigation_Node_Hash;

   function Navigation_Node_Equivalen_Keys
     (P_Left, P_Right : in Hexagon.Server_Navigation.Type_Navigation_Node_Id) return Boolean
   is
   begin
      return P_Left = P_Right;
   end Navigation_Node_Equivalen_Keys;

   function Navigation_Node_Equivalen_Items_Access
     (P_Left, P_Right : in Hexagon.Server_Navigation.Type_Navigation_Node_Access) return Boolean
   is
   begin
      return P_Left.all = P_Right.all;
   end Navigation_Node_Equivalen_Items_Access;

   function Get_Navigation_Node_By_Id (P_Navigation : in Type_Navigation;
      P_Navigation_Node_Id : in Type_Navigation_Node_Id) return Type_Navigation_Node_Access
   is
      Trav       : Navigation_Node_List_Pkg.Cursor;
      An_Element : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Found      : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Get_Navigation_Node_By_Id - enter");
      end if;

      Found := False;
      Trav  := Navigation_Node_List_Pkg.First (P_Navigation.Navigation_Node_List);
      while Navigation_Node_List_Pkg.Has_Element (Trav) and not Found loop
         An_Element := Navigation_Node_List_Pkg.Element (Trav);
         if An_Element.all.Id = P_Navigation_Node_Id then
            Found := True;
         else
            Trav := Navigation_Node_List_Pkg.Next (Trav);
         end if;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Get_Navigation_Node_By_Id - exit");
      end if;

      if Navigation_Node_List_Pkg.Has_Element (Trav) then
         return Navigation_Node_List_Pkg.Element (Trav);
      else
         return null;
      end if;

   end Get_Navigation_Node_By_Id;

   function Get_Navigation_Node_By_Position (P_Navigation : in Type_Navigation;
      P_Position : in Hexagon.Type_Hexagon_Position) return Type_Navigation_Node_Access
   is
      Found_Bucket : Natural := 0;
      Trav_Node    : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;

      A_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

      Navigation_Node_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
      Ret                : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Get_Navigation_Node_By_Position - enter");
      end if;

      Navigation_Node_Id := Hexagon.Server_Navigation.Undefined_Navigation_Node_Id;
      Ret                := False;
      Trav_Node          :=
        Hexagon.Server_Navigation.Navigation_Node_List_Pkg.First
          (P_Navigation.Navigation_Node_List);

      while Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Has_Element (Trav_Node) and not Ret
      loop

         A_Node := Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Element (Trav_Node);

         Ret := P_Position = A_Node.all.Pos;
         if Ret then
            Navigation_Node_Id := A_Node.all.Id;
         end if;

         Trav_Node := Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Next (Trav_Node);
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Hexagon.Server_Navigation.Get_Navigation_Node_By_Position - exit Navigation_Node_Id:" &
            Navigation_Node_Id'Img);
      end if;

      return Get_Navigation_Node_By_Id (P_Navigation, Navigation_Node_Id);

   end Get_Navigation_Node_By_Position;

   function Find_Navigation_Node (P_Navigation : in Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id)
      return Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor
   is
      Trav_Node : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;
      Found     : Boolean;

      use Hexagon;
   begin
      Found     := False;
      Trav_Node :=
        Hexagon.Server_Navigation.Navigation_Node_List_Pkg.First
          (P_Navigation.Navigation_Node_List);
      while Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Has_Element (Trav_Node) and not Found
      loop
         if Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Element (Trav_Node).all.Id =
           P_Navigation_Node_Id then
            Found := True;
         else
            Trav_Node := Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Next (Trav_Node);
         end if;
      end loop;

      return Trav_Node;
   end Find_Navigation_Node;

   function Has_Neighbour (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id) return Boolean
   is
      Trav : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;
   begin
      Trav :=
        Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Find
          (P_Navigation_Node.Neighbours, P_Navigation_Node_Id);

      return Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element (Trav);
   end Has_Neighbour;

   procedure Save_Navigation (P_Filename : in Ada.Strings.Unbounded.Unbounded_String;
      P_Navigation                       : in Hexagon.Server_Navigation.Type_Navigation)
   is
      Write_File : Ada.Streams.Stream_IO.File_Type;
      Out_Stream : Stream_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Save_Navigation - enter");
      end if;

      Create
        (Write_File, Ada.Streams.Stream_IO.Out_File, Ada.Strings.Unbounded.To_String (P_Filename));
      Out_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Vector'Write (Out_Stream,
         P_Navigation.Navigation_Node_List);

      Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Save_Navigation - exit");
      end if;
   end Save_Navigation;

   procedure Load_Navigation (P_Filename : in     Ada.Strings.Unbounded.Unbounded_String;
      P_Navigation                       :    out Hexagon.Server_Navigation.Type_Navigation)
   is
      Read_File : Ada.Streams.Stream_IO.File_Type;
      In_Stream : Stream_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Load_Navigation - enter");
      end if;

      Text_IO.Put_Line ("Navigation File Name:" & Ada.Strings.Unbounded.To_String (P_Filename));
      Open (Read_File, Ada.Streams.Stream_IO.In_File, Ada.Strings.Unbounded.To_String (P_Filename));
      In_Stream := Ada.Streams.Stream_IO.Stream (Read_File);

      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Vector'Read (In_Stream,
         P_Navigation.Navigation_Node_List);

      Close (Read_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Load_Navigation - exit");
      end if;
   end Load_Navigation;

   procedure Print_Navigation_Node
     (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node)
   is
      Trav : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;

      A_Navigation_Node_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Navigation_Node - enter");
      end if;

      Text_IO.Put_Line ("  Id:" & P_Navigation_Node.Id'Img);
      Text_IO.Put_Line ("  Center:" & Hexagon.To_String (P_Navigation_Node.Pos));
      Text_IO.Put ("  Neighbours' Id:");
      Trav :=
        Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First
          (P_Navigation_Node.Neighbours);
      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element (Trav) loop
         A_Navigation_Node_Id :=
           Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element (Trav);
         Text_IO.Put ("  " & A_Navigation_Node_Id'Img);

         Trav := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next (Trav);
      end loop;
      Text_IO.New_Line;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Navigation_Node - exit");
      end if;
   end Print_Navigation_Node;

   procedure Print_Navigation (P_Navigation : in Hexagon.Server_Navigation.Type_Navigation) is
      Trav : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;

      A_Navigation_Node_Id     : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
      A_Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Navigation - enter");
      end if;

      Trav :=
        Hexagon.Server_Navigation.Navigation_Node_List_Pkg.First
          (P_Navigation.Navigation_Node_List);
      while Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Has_Element (Trav) loop

         A_Navigation_Node_Access :=
           Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Element (Trav);
         A_Navigation_Node_Id := A_Navigation_Node_Access.all.Id;

         Text_IO.New_Line;
         Text_IO.Put_Line (" Navigation_Id:" & A_Navigation_Node_Id'Img);
         Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_Access.all);

         Trav := Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Navigation - exit");
      end if;
   end Print_Navigation;

   function ID_Hashed
     (P_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (P_Id);
   end ID_Hashed;

   type Type_Path_Node is record
      Navigation_Id        : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
      F, G, H              : Integer;
      Parent_Navigation_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
   end record;

   package A_Star_Vector is new Ada.Containers.Hashed_Maps
     (Key_Type => Hexagon.Server_Navigation.Type_Navigation_Node_Id, Element_Type => Type_Path_Node,
      Hash     => ID_Hashed, Equivalent_Keys => Hexagon.Server_Navigation."=");

   function Best_Open (P_Open_List : A_Star_Vector.Map) return A_Star_Vector.Cursor is
      Trav_Open         : A_Star_Vector.Cursor;
      Min_So_Far_Cursor : A_Star_Vector.Cursor;
      Min_So_Far        : Type_Path_Node;

      use A_Star_Vector;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Best_Open - enter");
      end if;

      Trav_Open := A_Star_Vector.First (P_Open_List);

      if Trav_Open /= A_Star_Vector.No_Element then
         Min_So_Far := A_Star_Vector.Element (Trav_Open);
      end if;

      while Trav_Open /= A_Star_Vector.No_Element loop
         if A_Star_Vector.Element (Trav_Open).F <= Min_So_Far.F then
            Min_So_Far_Cursor := Trav_Open;
            Min_So_Far        := A_Star_Vector.Element (Trav_Open);
         end if;

         Trav_Open := A_Star_Vector.Next (Trav_Open);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Best_Open - exit");
      end if;

      return Min_So_Far_Cursor;
   end Best_Open;

   function Hexagon_Distance (P_From, P_To : in Hexagon.Type_Hexagon_Position) return Integer is
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

   procedure Find_Path (P_Navigation : in Type_Navigation; P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                  : in     Action.Type_Action_Type;
      P_Piece                        : in out Piece.Server.Fighting_Piece.Type_Piece'Class;
      P_From, P_To : in     Hexagon.Type_Hexagon_Position; P_Status : out Status.Type_Status;
      P_Path                         :    out Hexagon.Server_Navigation.Path_Pkg.Vector)
   is

      Open_List, Closed_List : A_Star_Vector.Map;

      To_Navigation_Node_Id   : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
      From_Navigation_Node_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;

      Neighbour_Cursor : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;

      Current_Navigation_Node_Access, From_Navigation_Node_Access,
      To_Navigation_Node_Access : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Neighbour_Node_Access     : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

      Best_So_Far           : Type_Path_Node;
      Best_So_Far_Cursor, N : A_Star_Vector.Cursor;

      Solution_Found  : Boolean;
      Neighbour_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      procedure Build_Path (P_Closed_List : in     Hexagon.Server_Navigation.A_Star_Vector.Map;
         P_Path                           : in out Hexagon.Server_Navigation.Path_Pkg.Vector)
      is
         Done      : Boolean;
         Trav_Path : A_Star_Vector.Cursor;

         use A_Star_Vector;
      begin
         if Verbose then
            Text_IO.Put_Line ("Hexagon.Server_Navigation.Build_Path - enter");
         end if;

         Done      := False;
         Trav_Path := A_Star_Vector.Find (P_Closed_List, Best_So_Far.Navigation_Id);
         while Trav_Path /= A_Star_Vector.No_Element and not Done loop

            Hexagon.Server_Navigation.Path_Pkg.Prepend
              (P_Path,
               Get_Navigation_Node_By_Id
                 (P_Navigation, A_Star_Vector.Element (Trav_Path).Navigation_Id));

            if A_Star_Vector.Element (Trav_Path).Navigation_Id /=
              A_Star_Vector.Element (Trav_Path).Parent_Navigation_Id then

               Trav_Path :=
                 A_Star_Vector.Find
                   (P_Closed_List, A_Star_Vector.Element (Trav_Path).Parent_Navigation_Id);
            else
               Done := True;
            end if;
         end loop;

         if Verbose then
            Text_IO.Put_Line ("Hexagon.Server_Navigation.Build_Path - exit");
         end if;

      end Build_Path;

      use A_Star_Vector;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Hexagon.Server_Navigation.Find_Path - enter P_From:" & Hexagon.To_String (P_From) &
            " P_To:" & Hexagon.To_String (P_To));
      end if;

      From_Navigation_Node_Access := Get_Navigation_Node_By_Position (P_Navigation, P_From);
      To_Navigation_Node_Access   := Get_Navigation_Node_By_Position (P_Navigation, P_To);

      From_Navigation_Node_Id := From_Navigation_Node_Access.all.Id;
      To_Navigation_Node_Id   := To_Navigation_Node_Access.all.Id;

      -- start point
      A_Star_Vector.Insert
        (Open_List, From_Navigation_Node_Id,
         Type_Path_Node'
           (From_Navigation_Node_Id,
            Hexagon_Distance
              (From_Navigation_Node_Access.all.Pos, To_Navigation_Node_Access.all.Pos),
            0,
            Hexagon_Distance
              (From_Navigation_Node_Access.all.Pos, To_Navigation_Node_Access.all.Pos),
            Hexagon.Server_Navigation.Undefined_Navigation_Node_Id));

      Solution_Found := False;
      while not Solution_Found loop
         -- Find most promising open node so far
         Best_So_Far_Cursor := Best_Open (Open_List);

         if A_Star_Vector.Has_Element (Best_So_Far_Cursor) then
            Best_So_Far := A_Star_Vector.Element (Best_So_Far_Cursor);

            if Best_So_Far.H = 0 then
               Solution_Found := True;
               P_Status       := Status.Ok;
            else
               Current_Navigation_Node_Access :=
                 Get_Navigation_Node_By_Id (P_Navigation, Best_So_Far.Navigation_Id);

               Neighbour_Cursor :=
                 Navigation_Neighbours_List_Pkg.First
                   (Current_Navigation_Node_Access.all.Neighbours);

               while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element
                   (Neighbour_Cursor)
               loop
                  Neighbour_Node_Access :=
                    Hexagon.Server_Navigation.Get_Navigation_Node_By_Id
                      (P_Navigation,
                       Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element
                         (Neighbour_Cursor));

                  Neighbour_Patch :=
                    Hexagon.Server_Map.Get_Patch_Adress_From_AB
                      (Neighbour_Node_Access.all.Pos.A, Neighbour_Node_Access.all.Pos.B);

                  if
                    (Landscape.Server.Has_Patch_Free_Slot
                       (Landscape.Type_Patch (Neighbour_Patch.all)) and
                     Piece.Server.Patch_Belongs_To_Player
                       (Landscape.Type_Patch (Neighbour_Patch.all), P_Player_Id))
                  then

                     declare
                        Neighbour_Node_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
                        Neighbour_Access  : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
                        F, G, H           : Integer;
                     begin
                        Neighbour_Node_Id :=
                          Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element
                            (Neighbour_Cursor);
                        Neighbour_Access :=
                          Hexagon.Server_Navigation.Get_Navigation_Node_By_Id
                            (P_Navigation, Neighbour_Node_Id);

                        if
                          ((A_Star_Vector.Find (Closed_List, Neighbour_Access.all.Id) =
                            A_Star_Vector.No_Element))
                        then

                           -- For this new position, calculate F, G and H:
                           G :=
                             Best_So_Far.G +
                             Piece.Server.Fighting_Piece.Movement_Cost
                               (P_Player_Id, P_Action_Type, P_Piece,
                                Landscape.Type_Patch (Neighbour_Patch.all),
                                Landscape.Type_Patch (Neighbour_Patch.all));

                           H :=
                             Hexagon_Distance
                               (Neighbour_Access.all.Pos, To_Navigation_Node_Access.all.Pos);
                           F := G + H;

                           N := A_Star_Vector.Find (Open_List, Neighbour_Access.all.Id);
                           if N = A_Star_Vector.No_Element then
                              A_Star_Vector.Include
                                (Open_List, Neighbour_Access.all.Id,
                                 Type_Path_Node'
                                   (Neighbour_Access.all.Id, F, G, H, Best_So_Far.Navigation_Id));
                           else
                              if A_Star_Vector.Element (N).G > G then
                                 A_Star_Vector.Include
                                   (Open_List, Neighbour_Access.all.Id,
                                    Type_Path_Node'
                                      (Neighbour_Access.all.Id, F, G, H,
                                       Best_So_Far.Navigation_Id));

                              end if;
                           end if;

                        end if;

                     end;

                  end if;
                  Neighbour_Cursor := Navigation_Neighbours_List_Pkg.Next (Neighbour_Cursor);

               end loop; -- Nearby nodes

            end if;
            -- We dont need to analyse Current_Node any more. Put it in closed
            --list
            A_Star_Vector.Insert
              (Closed_List, A_Star_Vector.Element (Best_So_Far_Cursor).Navigation_Id,
               A_Star_Vector.Element (Best_So_Far_Cursor));

            A_Star_Vector.Delete (Open_List, Best_So_Far_Cursor);

         else
            P_Status       := Status.No_Path_Found;
            Solution_Found := True;
         end if;

      end loop;   --as long as there are available patches and we have not
      --found end patch yet.

      if P_Status = Status.Ok then
         Build_Path (Closed_List, P_Path);

      end if;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Find_Path - exit");
         Print_Path (P_Path);
      end if;
   end Find_Path;

   procedure Print_Path (P_Path : in Hexagon.Server_Navigation.Path_Pkg.Vector) is
      Trav       : Hexagon.Server_Navigation.Path_Pkg.Cursor;
      An_Element : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Path - enter");
      end if;

      Trav := Hexagon.Server_Navigation.Path_Pkg.First (P_Path);
      while Hexagon.Server_Navigation.Path_Pkg.Has_Element (Trav) loop
         An_Element := Hexagon.Server_Navigation.Path_Pkg.Element (Trav);

         Text_IO.Put_Line (An_Element.all.Id'Img & " " & Hexagon.To_String (An_Element.all.Pos));

         Trav := Hexagon.Server_Navigation.Path_Pkg.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Print_Path - exit");
      end if;
   end Print_Path;

   function Get_Navigation (P_Navigation_List : in Hexagon.Server_Navigation.Type_Navigation_List;
      P_Navigation_Index : in Positive) return Hexagon.Server_Navigation.Type_Navigation_Access
   is
   begin
      if Verbose then
         text_IO.Put_Line
           ("Hexagon.Server_Navigation.Get_Navigation - enter - exit:" & P_Navigation_Index'Img);
      end if;

      return Hexagon.Server_Navigation.Navigation_List_Pkg.Element
          (P_Navigation_List.Navigation_List, P_Navigation_Index);
   end Get_Navigation;

   procedure Create_Navigation
     (P_Navigation_List : out Hexagon.Server_Navigation.Type_Navigation_List)
   is
   begin
      -- land navigation
      Hexagon.Server_Navigation.Navigation_List_Pkg.Append
        (P_Navigation_List.Navigation_List,
         new Hexagon.Server_Navigation.Type_Navigation'
           (Navigation_Node_List =>
              Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Empty_Vector));

      -- water navigation
      Hexagon.Server_Navigation.Navigation_List_Pkg.Append
        (P_Navigation_List.Navigation_List,
         new Hexagon.Server_Navigation.Type_Navigation'
           (Navigation_Node_List =>
              Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Empty_Vector));

   end Create_Navigation;

   procedure Reset_Navigation_Node
     (P_Navigation_Node : in out Hexagon.Server_Navigation.Type_Navigation_Node_Access)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation_Node - enter");
      end if;

      Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Clear (P_Navigation_Node.Neighbours);
      Hexagon.Server_Navigation.Free_Navigation_Node (P_Navigation_Node);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation_Node - exit");
      end if;
   end Reset_Navigation_Node;

   procedure Reset_Navigation
     (P_Navigation : in out Hexagon.Server_Navigation.Type_Navigation_Access)
   is
      Trav_Navigation_Node : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;
      A_Navigation_Node    : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation - enter");
      end if;

      Trav_Navigation_Node :=
        Hexagon.Server_Navigation.Navigation_Node_List_Pkg.First
          (P_Navigation.all.Navigation_Node_List);
      while Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Has_Element (Trav_Navigation_Node)
      loop

         A_Navigation_Node :=
           Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Element (Trav_Navigation_Node);
         Hexagon.Server_Navigation.Reset_Navigation_Node (A_Navigation_Node);

         Trav_Navigation_Node :=
           Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Next (Trav_Navigation_Node);
      end loop;

      Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Clear
        (P_Navigation.all.Navigation_Node_List);

      Hexagon.Server_Navigation.Free_Navigation (P_Navigation);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation - exit");
      end if;
   end Reset_Navigation;

   procedure Reset_Navigation_List is

      Trav_Navigation : Hexagon.Server_Navigation.Navigation_List_Pkg.Cursor;
      A_Navigation    : Hexagon.Server_Navigation.Type_Navigation_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation_List - enter");
      end if;

      Trav_Navigation :=
        Hexagon.Server_Navigation.Navigation_List_Pkg.First
          (Hexagon.Server_Navigation.A_Navigation_List.Navigation_List);
      while Hexagon.Server_Navigation.Navigation_List_Pkg.Has_Element (Trav_Navigation) loop

         A_Navigation := Hexagon.Server_Navigation.Navigation_List_Pkg.Element (Trav_Navigation);

         Hexagon.Server_Navigation.Reset_Navigation (A_Navigation);

         Trav_Navigation := Hexagon.Server_Navigation.Navigation_List_Pkg.Next (Trav_Navigation);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Navigation.Reset_Navigation_List - exit:");
      end if;
   end Reset_Navigation_List;

end Hexagon.Server_Navigation;
