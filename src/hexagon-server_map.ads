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

with Landscape;
with Ada.Strings.Unbounded;
with Effect.Server;
with Construction;

package Hexagon.Server_Map is

   Not_Existing_Patch : exception;
   Slot_Not_Found_For_Piece : exception;

   type Type_Server_Patch;
   type Type_Server_Patch_Adress is access all Type_Server_Patch;
   type Type_Neighbour_List is array (1 .. 6) of Type_Server_Patch_Adress;

   type Type_Server_Patch is new Landscape.Type_Patch with record
      Visited                : Boolean;
      Neighbours             : Type_Neighbour_List;
   end record;

   type Type_Server_Patch_Area is array (Positive range <>) of Type_Server_Patch_Adress;
   type Type_Server_Patch_Area_Access is access all Type_Server_Patch_Area;

   type Type_Patch_Area is array (Positive range <>) of Type_Server_Patch_Adress;
   type Type_Patch_Area_Access is access all Type_Patch_Area;

   type Type_Server_Map is array (1 .. 100, 1 .. 100) of Type_Server_Patch_Adress;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map);

   function Get_Patch_Adress_From_AB
     (P_A, P_B : in Type_Hexagon_Numbers)
      return     Type_Server_Patch_Adress;

   type Type_Visit_Procedure is access procedure (P_Patch : in out Type_Server_Patch_Adress);

   procedure Reset_Visit;
   procedure Reset_Pieces_On_Patches;

   procedure Numerate (P_Patch : in out Type_Server_Patch; P_A, P_B : in Type_Hexagon_Numbers);

   procedure Traverse
     (P_Patch : in out Type_Server_Patch_Adress;
      P_Visit : in Type_Visit_Procedure);

   procedure Put (P_Patch : in Type_Server_Patch);

   procedure Init;

   procedure Save_Map (P_Filename : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Load_Map (P_Filename : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Save_Scenario (P_Filename : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Put_Map;

   function Are_Neighbours (P_From, P_To : in Type_Server_Patch) return Boolean;

   function Trav_Axis
     (P_Patch : in Hexagon.Server_Map.Type_Server_Patch;
      P_Axis  : in Integer)
      return    Hexagon.Server_Map.Type_Server_Patch_Adress;

   Empty : constant Type_Server_Patch :=
     Type_Server_Patch'
     (True,
      Type_Hexagon_Position'(P_Valid => False),
      Landscape.Undefined_Landscape,
      Landscape.Pieces_Here_List.Empty_Vector,
      Construction.Construction_List.Empty_Set,
      Effect.Effect_List.Empty_Map,
      False,
      Neighbours =>
     Type_Neighbour_List'(Type_Server_Patch_Adress'(null),
                          Type_Server_Patch_Adress'(null),
                          Type_Server_Patch_Adress'(null),
                          Type_Server_Patch_Adress'(null),
                          Type_Server_Patch_Adress'(null),
                          Type_Server_Patch_Adress'(null)));

   A_Map : Type_Server_Map := (others => (others => null));

end Hexagon.Server_Map;
