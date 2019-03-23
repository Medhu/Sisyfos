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
--

with Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Text_IO;
with Ada.Strings.Unbounded;

package body Hexagon.Server_Map is

   Verbose : constant Boolean := False;

   procedure Put (P_Patch : in Type_Server_Patch) is
   begin
      Text_IO.Put_Line ("" & P_Patch.Pos.A'Img & "a + " & P_Patch.Pos.B'Img & "b ");
      Landscape.Put_Pieces_Here (P_Patch.Pieces_Here);
   end Put;

   function Are_Neighbours (P_From, P_To : in Type_Server_Patch) return Boolean is
      Ret : Boolean := False;
   begin
      for Trav in P_From.Neighbours'First .. P_From.Neighbours'Last loop
         if P_From.Neighbours (Trav) /= null then
            if P_From.Neighbours (Trav).all.Pos = P_To.Pos then
               Ret := True;
            end if;
         end if;
      end loop;

      return Ret;
   end Are_Neighbours;

   procedure Numerate (P_Patch : in out Type_Server_Patch; P_A, P_B : in Type_Hexagon_Numbers) is
      Next : Type_Server_Patch_Adress;
   begin
      if P_Patch.Visited = False then

         P_Patch.Visited := True;

         P_Patch.Pos.A := P_A;
         P_Patch.Pos.B := P_B;

         Next := Type_Server_Patch_Adress (P_Patch.Neighbours (6));
         if Next /= null then
            Numerate (Next.all, P_A, P_B + 1);
         else
            null;
         end if;

         Next := Type_Server_Patch_Adress (P_Patch.Neighbours (3));
         if Next /= null then
            Numerate (Next.all, P_A, P_B - 1);
         else
            null;
         end if;

         Next := Type_Server_Patch_Adress (P_Patch.Neighbours (1));
         if Next /= null then
            Numerate (Next.all, P_A + 1, P_B);
         else
            null;
         end if;

         Next := Type_Server_Patch_Adress (P_Patch.Neighbours (4));
         if Next /= null then
            Numerate (Next.all, P_A - 1, P_B);
         else
            null;
         end if;
      end if;
   end Numerate;

   procedure Init (P_Map : in out Hexagon.Server_Map.Type_Server_Map) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon_Server_Map.Init - enter");
      end if;

      -- Set up map
      --
      for ArrayX in P_Map'First (1) .. P_Map'Last (1) loop -- Horisontal
         for ArrayY in P_Map'First (2) .. P_Map'Last (2) loop -- Vertical
            P_Map (ArrayX, ArrayY) := new Type_Server_Patch'(Hexagon.Server_Map.Empty);

            P_Map (ArrayX, ArrayY).Pieces_Here := Landscape.Pieces_Here_List.Empty_Vector;

         end loop;
      end loop;

      for ArrayX in P_Map'First (1) .. P_Map'Last (1) loop -- Horisontal
         for ArrayY in P_Map'First (2) .. P_Map'Last (2) loop -- Vertical
            --
            begin
               P_Map (ArrayX, ArrayY).Neighbours (1) := (P_Map (ArrayX + 1, ArrayY + 0));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (1) := null;
            end;

            begin
               P_Map (ArrayX, ArrayY).Neighbours (2) := (P_Map (ArrayX + 1, ArrayY - 1));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (2) := null;
            end;

            begin
               P_Map (ArrayX, ArrayY).Neighbours (3) := (P_Map (ArrayX + 0, ArrayY - 1));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (3) := null;
            end;

            begin
               P_Map (ArrayX, ArrayY).Neighbours (4) := (P_Map (ArrayX - 1, ArrayY + 0));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (4) := null;
            end;

            begin
               P_Map (ArrayX, ArrayY).Neighbours (5) := (P_Map (ArrayX - 1, ArrayY + 1));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (5) := null;
            end;

            begin
               P_Map (ArrayX, ArrayY).Neighbours (6) := (P_Map (ArrayX + 0, ArrayY + 1));
            exception
               when others =>
                  P_Map (ArrayX, ArrayY).Neighbours (6) := null;
            end;

            --
            P_Map (ArrayX, ArrayY).all.Pos :=
              Type_Hexagon_Position'
                (True, Type_Hexagon_Numbers (ArrayX), Type_Hexagon_Numbers (ArrayY));
         end loop;
      end loop;

      Numerate (P_Map (1, 1).all, 1, 1);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Init - exit");
      end if;

   end Init;

   function Get_Patch_Adress_From_AB
     (P_A, P_B : in Type_Hexagon_Numbers) return Type_Server_Patch_Adress
   is
      Next : Type_Server_Patch_Adress;
   begin
      Next := A_Map (1, 1);

      if P_A /= 1 then
         for Trav_A in 2 .. P_A loop
            Next := Type_Server_Patch_Adress (Next.all.Neighbours (1));
            if Next = null then
               raise Not_Existing_Patch;
            end if;
         end loop;
      end if;

      if P_B /= 1 then
         for Trav_B in 2 .. P_B loop
            Next := Type_Server_Patch_Adress (Next.all.Neighbours (6));
            if Next = null then
               raise Not_Existing_Patch;
            end if;
         end loop;
      end if;

      return Next;

   end Get_Patch_Adress_From_AB;

   procedure Reset_Visit is
   begin
      for ArrayX in A_Map'First (1) .. A_Map'Last (1) loop -- Horisontal
         for ArrayY in A_Map'First (2) .. A_Map'Last (2) loop -- Vertical
            A_Map (ArrayX, ArrayY).Visited := False;
         end loop;
      end loop;
   end Reset_Visit;

   procedure Reset_Pieces_On_Patches is
      Trav  : Landscape.Pieces_Here_List.Cursor;
      Index : Positive;

   begin
      for ArrayX in A_Map'First (1) .. A_Map'Last (1) loop -- Horisontal
         for ArrayY in A_Map'First (2) .. A_Map'Last (2) loop -- Vertical

            Trav := Landscape.Pieces_Here_List.First (A_Map (ArrayX, ArrayY).all.Pieces_Here);
            while Landscape.Pieces_Here_List.Has_Element (Trav) loop
               Index := Landscape.Pieces_Here_List.To_Index (Trav);

               Landscape.Pieces_Here_List.Delete (A_Map (ArrayX, ArrayY).all.Pieces_Here, Index);
               Landscape.Pieces_Here_List.Next (Trav);
            end loop;
         end loop;
      end loop;
   end Reset_Pieces_On_Patches;

   procedure Traverse
     (P_Patch : in out Type_Server_Patch_Adress;
      P_Visit : in     Type_Visit_Procedure)
   is
      Next : Type_Server_Patch_Adress;
   begin
      if P_Patch.Visited = False then
         P_Patch.Visited := True;

         P_Visit (P_Patch);
         for Visit in P_Patch.Neighbours'First .. P_Patch.Neighbours'Last loop

            Next := Type_Server_Patch_Adress (P_Patch.Neighbours (Visit));

            if Next /= null then
               Traverse (Next, P_Visit);
            end if;

         end loop;
      end if;
   end Traverse;

   -- send only map information.
   -- not information about pieces
   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
      Client_X, Client_Y : Integer;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Get_Map - enter");
      end if;

      for Trav_X in P_Server_Map'First (1) .. P_Server_Map'Last (1) loop
         for Trav_Y in P_Server_Map'First (2) .. P_Server_Map'Last (2) loop

            Client_X := Trav_X;
            Client_Y := Trav_Y;

            P_Server_Map (Client_X, Client_Y).Pos            := A_Map (Trav_X, Trav_Y).Pos;
            P_Server_Map (Client_X, Client_Y).Landscape_Here :=
              A_Map (Trav_X, Trav_Y).Landscape_Here;

         end loop;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Get_Map - exit");
      end if;
   end Get_Map;

   procedure Load_Map (P_Filename : in Ada.Strings.Unbounded.Unbounded_String;
                      P_Map : in out Hexagon.Server_Map.Type_Server_Map) is
      Read_File   : Ada.Streams.Stream_IO.File_Type;
      Inn_Stream  : Stream_Access;
      A_Landscape : Landscape.Type_Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Load_Map - enter");
      end if;

      Text_IO.Put_Line ("Map File Name:" & Ada.Strings.Unbounded.To_String (P_Filename));

      Open (Read_File, Ada.Streams.Stream_IO.In_File, Ada.Strings.Unbounded.To_String (P_Filename));

      Inn_Stream := Ada.Streams.Stream_IO.Stream (Read_File);

      for Trav_X in P_Map'First (1) .. P_Map'Last (1) loop
         for Trav_Y in P_Map'First (2) .. P_Map'Last (2) loop

            Landscape.Type_Landscape'Read (Ada.Streams.Stream_IO.Stream (Read_File), A_Landscape);

            P_Map (Trav_X, Trav_Y).Landscape_Here := A_Landscape;

         end loop;
      end loop;

      Close (Read_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Load_Map - exit");
      end if;
   end Load_Map;

   procedure Save_Map (P_Filename : in Ada.Strings.Unbounded.Unbounded_String;
                      P_Map : in Hexagon.Server_Map.Type_Server_Map) is
      Write_File : Ada.Streams.Stream_IO.File_Type;
      Out_Stream : Stream_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Save_Map - enter");
      end if;

      Create
        (Write_File,
         Ada.Streams.Stream_IO.Out_File,
         Ada.Strings.Unbounded.To_String (P_Filename));
      Out_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      for Trav_X in A_Map'First (1) .. A_Map'Last (1) loop
         for Trav_Y in A_Map'First (2) .. A_Map'Last (2) loop

            Landscape.Type_Landscape'Write
              (Ada.Streams.Stream_IO.Stream (Write_File),
               A_Map (Trav_X, Trav_Y).Landscape_Here);
         end loop;
      end loop;

      Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Save_Map - exit");
      end if;
   end Save_Map;

   procedure Save_Scenario (P_Filename : in Ada.Strings.Unbounded.Unbounded_String) is
      Write_File        : Ada.Streams.Stream_IO.File_Type;
      Out_Stream        : Stream_Access;
      Trav_Piece        : Landscape.Pieces_Here_List.Cursor;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Save_Scenario - enter");
      end if;

      Create
        (Write_File,
         Ada.Streams.Stream_IO.Out_File,
         Ada.Strings.Unbounded.To_String (P_Filename));
      Out_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "<table border=""1""  >");

      for Trav_X in A_Map'First (1) .. A_Map'Last (1) loop
         String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "  <tr>");
         for Trav_Y in A_Map'First (2) .. A_Map'Last (2) loop
            String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "    <td>");

            String'Write
              (Ada.Streams.Stream_IO.Stream (Write_File),
               "(" &
               A_Map (Trav_X, Trav_Y).Pos.A'Img &
               "," &
               A_Map (Trav_X, Trav_Y).Pos.B'Img &
               ") " &
               A_Map (Trav_X, Trav_Y).Landscape_Here'Img);

            Trav_Piece := Landscape.Pieces_Here_List.First (A_Map (Trav_X, Trav_Y).Pieces_Here);
            while Landscape.Pieces_Here_List.Has_Element (Trav_Piece) loop
               String'Write
                 (Ada.Streams.Stream_IO.Stream (Write_File),
                  " Id=" & Landscape.Pieces_Here_List.Element (Trav_Piece)'Img & "-");
               Trav_Piece := Landscape.Pieces_Here_List.Next (Trav_Piece);
            end loop;

            String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "    </td>");
         end loop;
         String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "  </tr>");
      end loop;
      String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "</table>");

      Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Save_Scenario - exit");
      end if;
   end Save_Scenario;

   procedure Put_Map is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Put_Map - enter");
      end if;

      for Trav_X in A_Map'First (1) .. A_Map'Last (1) loop
         Text_IO.New_Line;
         for Trav_Y in A_Map'First (2) .. A_Map'Last (2) loop

            Landscape.Put_Pieces_Here (A_Map (Trav_X, Trav_Y).Pieces_Here);
         end loop;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Server_Map.Put_Map - exit");
      end if;
   end Put_Map;

end Hexagon.Server_Map;
