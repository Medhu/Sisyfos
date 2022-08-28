--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2022  Frank J Jorgensen
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

package Attempt is

   type Type_Attempt_Info is private;  --new Natural;
   Initial_Attempt : constant Type_Attempt_Info ;
   Attempt_Done    : constant Type_Attempt_Info ;

   procedure Set_Attempt (P_Attempt_Info : in out Type_Attempt_Info; P_Value : in Natural);
   procedure Set_Initial_Attempt (P_Attempt_Info : in out Type_Attempt_Info);
   procedure Set_Done_Attempt (P_Attempt_Info : in out Type_Attempt_Info);

   function Get_Attempt (P_Attempt_Info : in Type_Attempt_Info) return Natural;
   function To_String (P_Attempt_Info : in Type_Attempt_Info) return String; --Ada.Strings.Unbounded.Unbounded_String;

   Illegal_Attempt_Info : exception;
private
   type Type_Attempt_Info is new Natural;
   Initial_Attempt : constant Type_Attempt_Info := Type_Attempt_Info(1);
   Attempt_Done    : constant Type_Attempt_Info := Type_Attempt_Info(99999);

end Attempt;
