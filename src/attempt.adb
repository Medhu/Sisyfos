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


package body Attempt is

--   Initial_Attempt : constant Type_Attempt_Info := Type_Attempt_Info(1);
--   Attempt_Done    : constant Type_Attempt_Info := Type_Attempt_Info(99999);

   procedure Set_Attempt (P_Attempt_Info : in out Type_Attempt_Info; P_Value : in Natural)
   is
   begin
      if Natural(P_Value) = Natural(Initial_Attempt) then
         raise Illegal_Attempt_Info
         with "Illegal to set the predefines Attempt_Info to " & Initial_Attempt'Img;
      elsif Natural(P_Value) = Natural(Attempt_Done) then
         raise Illegal_Attempt_Info
         with "Illegal to set the predefines Attempt_Info to " & Attempt_Done'Img;
      else
         P_Attempt_Info := Type_Attempt_Info(P_Value);
      end if;

   end Set_Attempt;

   procedure Set_Initial_Attempt (P_Attempt_Info : in out Type_Attempt_Info)
   is
   begin
      P_Attempt_Info := Initial_Attempt;
   end Set_Initial_Attempt;

   procedure Set_Done_Attempt (P_Attempt_Info : in out Type_Attempt_Info)
   is
   begin
      P_Attempt_Info := Attempt_Done;
   end Set_Done_Attempt;

   function Get_Attempt (P_Attempt_Info : in Type_Attempt_Info) return Natural
   is
   begin
      return Natural(P_Attempt_Info);
   end Get_Attempt;

   function To_String (P_Attempt_Info : in Type_Attempt_Info) return String--Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return P_Attempt_Info'img;
   end To_String;
end Attempt;
