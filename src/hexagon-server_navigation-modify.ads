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
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package Hexagon.Server_Navigation.Modify is

   function Create_Navigation_Node
     (P_Navigation : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Id : in Type_Navigation_Node_Id;
      P_Pos : in Hexagon.Type_Hexagon_Position)
     return Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   procedure Add_Path_To_Neighbour
     (P_Navigation_Node    : in out Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id);

   procedure Remove_Path_To_Neighbour
     (P_Navigation_Node    : in out Hexagon.Server_Navigation.Type_Navigation_Node;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id);

   procedure Add_Path_To_Neighbour
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_From, P_To  : in     Hexagon.Type_Hexagon_Position);

   procedure Remove_Path_To_Neighbour
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_From, P_To  : in     Hexagon.Type_Hexagon_Position);

   procedure Remove_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node_Id : in     Hexagon.Server_Navigation.Type_Navigation_Node_Id);

   procedure Add_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Position    : in     Hexagon.Type_Hexagon_Position);

   procedure Remove_Navigation_Node
     (P_Navigation  : in out Hexagon.Server_Navigation.Type_Navigation;
      P_Position    : in     Hexagon.Type_Hexagon_Position);


end Hexagon.Server_Navigation.Modify;







