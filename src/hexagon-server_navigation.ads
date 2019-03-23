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
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Status;
with Player;
with Action;
with Piece.Server.Fighting_Piece;
with Ada.Unchecked_Deallocation;

package Hexagon.Server_Navigation is
   Bucket_Error : exception;

   type Type_Navigation_Node_Id is new Natural;
   Undefined_Navigation_Node_Id : constant Type_Navigation_Node_Id := 0;

   type Type_Navigation_Node;
   type Type_Navigation_Node_Access is access all Type_Navigation_Node;

   procedure Write_Navigation_Node_Access
     (Stream :    not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Type_Navigation_Node_Access);

   for Type_Navigation_Node_Access'Write use Write_Navigation_Node_Access;

   procedure Read_Navigation_Node_Access
     (Stream :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Type_Navigation_Node_Access);

   for Type_Navigation_Node_Access'Read use Read_Navigation_Node_Access;

   function Navigation_Node_Hash
     (P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id)
      return Ada.Containers.Hash_Type;

   function Navigation_Node_Equivalen_Keys
     (P_Left, P_Right : in Hexagon.Server_Navigation.Type_Navigation_Node_Id) return Boolean;

   function Navigation_Node_Equivalen_Items_Access
     (P_Left, P_Right : in Hexagon.Server_Navigation.Type_Navigation_Node_Access) return Boolean;

   package Navigation_Neighbours_List_Pkg is new Ada.Containers.Ordered_Sets
     (Hexagon.Server_Navigation.Type_Navigation_Node_Id);

   package Path_Pkg is new Ada.Containers.Vectors
     (Hexagon.Server_Navigation.Type_Navigation_Node_Id,
      Hexagon.Server_Navigation.Type_Navigation_Node_Access,
      Hexagon.Server_Navigation.Navigation_Node_Equivalen_Items_Access);

   -- Read one file when you are looking for the original map in the scenario.
   -- When you have started to play, then save the "game-in-progress" as another.
   type Type_Navigation_Node is record
      Id         : Type_Navigation_Node_Id;
      Pos        : Hexagon.Type_Hexagon_Position;
      Neighbours : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Set :=
        Navigation_Neighbours_List_Pkg.Empty_Set;
   end record;
   procedure Free_Navigation_Node is new Ada.Unchecked_Deallocation (Type_Navigation_Node,
      Type_Navigation_Node_Access);

   package Navigation_Node_List_Pkg is new Ada.Containers.Vectors (Positive,
      Hexagon.Server_Navigation.Type_Navigation_Node_Access);

   type Type_Navigation is record
      Navigation_Node_List : Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Vector;
   end record;

   type Type_Navigation_Access is access all Type_Navigation;
   procedure Free_Navigation is new Ada.Unchecked_Deallocation (Type_Navigation,
      Type_Navigation_Access);

   function Get_Navigation_Node_By_Id (P_Navigation : in Type_Navigation;
      P_Navigation_Node_Id : in Type_Navigation_Node_Id) return Type_Navigation_Node_Access;

   function Get_Navigation_Node_By_Position (P_Navigation : in Type_Navigation;
      P_Position : in Hexagon.Type_Hexagon_Position) return Type_Navigation_Node_Access;

   function Find_Navigation_Node (P_Navigation : in Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id)
      return Hexagon.Server_Navigation.Navigation_Node_List_Pkg.Cursor;

   function Has_Neighbour (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id) return Boolean;

   -- Use Save_Navigation and Load_Navigation both when loading a scenario (while
   -- creating the game. These functions are also used while loading and saving a
   -- game in progress.
   --
   procedure Save_Navigation (P_Filename : in Ada.Strings.Unbounded.Unbounded_String;
      P_Navigation                       : in Hexagon.Server_Navigation.Type_Navigation);

   procedure Load_Navigation (P_Filename : in     Ada.Strings.Unbounded.Unbounded_String;
      P_Navigation                       :    out Hexagon.Server_Navigation.Type_Navigation);

   type Type_Path is record
      This_Path : Hexagon.Server_Navigation.Path_Pkg.Vector;
      Direction : Integer;
   end record;

   function Hexagon_Distance (P_From, P_To : in Hexagon.Type_Hexagon_Position) return Integer;

   procedure Find_Path (P_Navigation : in Type_Navigation; P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                  : in     Action.Type_Action_Type;
      P_Piece                        : in out Piece.Server.Fighting_Piece.Type_Piece'Class;
      P_From, P_To : in     Hexagon.Type_Hexagon_Position; P_Status : out Status.Type_Status;
      P_Path                         :    out Hexagon.Server_Navigation.Path_Pkg.Vector);

   procedure Print_Path (P_Path : in Hexagon.Server_Navigation.Path_Pkg.Vector);

   procedure Print_Navigation_Node
     (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node);

   procedure Print_Navigation (P_Navigation : in Hexagon.Server_Navigation.Type_Navigation);

   package Navigation_List_Pkg is new Ada.Containers.Vectors (Positive,
      Hexagon.Server_Navigation.Type_Navigation_Access);

   type Type_Navigation_List is record
      Navigation_List : Navigation_List_Pkg.Vector;
   end record;

   function Get_Navigation (P_Navigation_List : in Hexagon.Server_Navigation.Type_Navigation_List;
      P_Navigation_Index : in Positive) return Hexagon.Server_Navigation.Type_Navigation_Access;

   A_Navigation_List : Hexagon.Server_Navigation.Type_Navigation_List;

   procedure Create_Navigation
     (P_Navigation_List : out Hexagon.Server_Navigation.Type_Navigation_List);

   procedure Reset_Navigation_List;

end Hexagon.Server_Navigation;
