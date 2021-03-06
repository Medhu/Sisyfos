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
with Ada.Unchecked_Deallocation;
package Hexagon.Area.Server_Area is

   type Type_Action_Capabilities_Access is access all Type_Action_Capabilities;
   type Type_Action_Capabilities_Access_A is access all Type_Action_Capabilities_A;

   procedure Free_Action_Capabilities is new Ada.Unchecked_Deallocation (
      Type_Action_Capabilities,
      Type_Action_Capabilities_Access);
   procedure Free_Action_Capabilities_A is new Ada.Unchecked_Deallocation (
      Type_Action_Capabilities_A,
      Type_Action_Capabilities_Access_A);

end Hexagon.Area.Server_Area;
