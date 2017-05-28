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

with Landscape;
with Construction;
with Landscape.Server;

package Piece.Server.House_Piece is

   type Type_House is abstract new Piece.Server.Type_Piece with null record;
   type Type_House_Access is access all Type_House;
   type Type_House_Access_Class is access all Type_House'Class;

   type Type_House_Type_Info is record
      Type_Name           : Utilities.RemoteString.Type_String;
      Category            : Type_Category;
      Construct_Landscape : Landscape.Server
        .Type_List_Landscape_Access; -- on what landscape can this house construct
   end record;

   House_Class : Piece.Server.Type_Piece_Access_Class;

   type Type_House_Type_Info_List is
     array (Piece.Type_Piece_Type range <>) of Type_House_Type_Info;
   type Type_House_Type_Info_List_Access is
     access all Type_House_Type_Info_List;

   procedure Init
     (P_House_Piece_Class : in Piece.Server.Type_Piece'Class;
      P_House_Info        : in Type_House_Type_Info_List);

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece : in out Piece.Server.House_Piece.Type_House) is abstract;

   --
   -- Perform_Construction
   --
   function Validate_Perform_Construction
     (P_Player_Id          : in Player.Type_Player_Id;
      P_Action_Type        : in Action.Type_Action_Type;
      P_Construction_Piece : in Piece.Server.House_Piece.Type_House;
      P_Construction_Pos   : in Hexagon.Type_Hexagon_Position;
      P_Construction       : in Construction.Type_Construction)
      return Boolean is abstract;

   procedure Before_Perform_Construction
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Construction_Piece : in out Piece.Server.House_Piece.Type_House;
      P_Construction_Pos   : in     Hexagon.Type_Hexagon_Position;
      P_Construction       : in     Construction.Type_Construction;
      P_Result             :    out Status.Type_Result_Status) is abstract;

   procedure End_Perform_Construction
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Construction_Piece : in out Piece.Server.House_Piece.Type_House;
      P_Construction_Pos   : in     Hexagon.Type_Hexagon_Position;
      P_Construction       : in     Construction.Type_Construction;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   --
   -- Perform_Demolition
   --
   function Validate_Perform_Demolition
     (P_Player_Id        : in Player.Type_Player_Id;
      P_Action_Type      : in Action.Type_Action_Type;
      P_Demolition_Piece : in Piece.Server.House_Piece.Type_House;
      P_Demolition_Pos   : in Hexagon.Type_Hexagon_Position;
      P_Construction     : in Construction.Type_Construction)
      return Boolean is abstract;

   procedure Before_Perform_Demolition
     (P_Player_Id        : in     Player.Type_Player_Id;
      P_Action_Type      : in     Action.Type_Action_Type;
      P_Demolition_Piece : in out Piece.Server.House_Piece.Type_House;
      P_Demolition_Pos   : in     Hexagon.Type_Hexagon_Position;
      P_Construction     : in     Construction.Type_Construction;
      P_Result           :    out Status.Type_Result_Status) is abstract;

   procedure End_Perform_Demolition
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Demolition_Piece   : in out Piece.Server.House_Piece.Type_House;
      P_Demolition_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_Construction       : in     Construction.Type_Construction;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer) is abstract;

   type Type_Construction_Validation_Mode is
     (Construction_Mode, Demolition_Mode);

   function Validate_Exisiting_Construction
     (P_Patch        : in Landscape.Type_Patch;
      P_Construction : in Construction.Type_Construction) return Boolean;

   function Can_Construct_On_Land
     (P_Type_Of_Piece : in Type_Piece_Type;
      P_Landscape     : in Landscape.Type_Landscape) return Boolean;

   function Get_Type_Of_Piece_Name
     (P_Piece : in Piece.Type_Piece) return Utilities.RemoteString.Type_String;

end Piece.Server.House_Piece;
