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
with Utilities;
package Landscape.Server is

   type Type_List_Landscape_Access is access all Type_List_Landscape;

   type Type_Landscape_Type_Info is record
      Type_Name                 : Utilities.RemoteString.Type_String;
      Max_Pieces_Here           : Positive;
   end record;

   type Type_Landscape_Type_Info_List is array (Landscape.Type_Landscape range <>) of Type_Landscape_Type_Info;
   type Type_Landscape_Type_Info_List_Access is access all Type_Landscape_Type_Info_List;

   procedure Init (P_Landscape_Info : in Type_Landscape_Type_Info_List);

   function Is_Patch_Empty (P_Patch : in Landscape.Type_Patch) return Boolean;
   function Has_Patch_Free_Slot (P_Patch : in Landscape.Type_Patch) return Boolean;

   function Get_Landscape_Info (P_Landscape : in Type_Landscape) return Type_Landscape_Type_Info;


end Landscape.Server;
