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
with Status;

package Attempt is

   type Type_Command_Status is (Initial, Proceed, Failed, Done);
   type Type_Info is new Natural;
   type Type_Attempt_Info is private;


   procedure Set_Attempt_Info (P_Attempt_Info : in out Type_Attempt_Info; P_Info : in Natural);
   function Get_Attempt_Info (P_Attempt_Info : in Type_Attempt_Info) return Natural;

   procedure Set_Command_Status (P_Attempt_Info : in out Type_Attempt_Info; P_Status : in Type_Command_Status);
   function Get_Command_Status (P_Attempt_Info : in Type_Attempt_Info) return Type_Command_Status;

   procedure Set_Attempt_Status (P_Attempt_Info : in out Type_Attempt_Info; P_Status : in Status.Type_Status);
   function Get_Attempt_Status (P_Attempt_Info : in Type_Attempt_Info) return Status.Type_Status;

   function Initial_Attempt return Type_Attempt_Info;
   procedure Set_Done_Attempt (P_Attempt_Info : in out Type_Attempt_Info);
   procedure Set_Proceed_Attempt (P_Attempt_Info : in out Type_Attempt_Info);
   procedure Set_Failed_Attempt (P_Attempt_Info : in out Type_Attempt_Info);

   function To_String (P_Attempt_Info : in Type_Attempt_Info) return String; --Ada.Strings.Unbounded.Unbounded_String;

   Illegal_Attempt_Info : exception;
private
   type Type_Attempt_Info is
      record
         Command_Status : Type_Command_Status;
         Attempt_Status : Status.Type_Status;
         Info   : Natural;
      end record;

end Attempt;
