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

   procedure Set_Attempt_Info (P_Attempt_Info : in out Type_Attempt_Info; P_Info : in Natural)
   is
   begin
         P_Attempt_Info.Info := P_Info;
   end Set_Attempt_Info;

   function Get_Attempt_Info (P_Attempt_Info : in Type_Attempt_Info) return Natural
   is
   begin
      return P_Attempt_Info.Info;
   end Get_Attempt_Info;

   function Initial_Attempt return Type_Attempt_Info
   is
   begin
      return Type_Attempt_Info'(Initial, Status.Ok, 0);
   end Initial_Attempt;

   procedure Set_Done_Attempt (P_Attempt_Info : in out Type_Attempt_Info)
   is
   begin
      P_Attempt_Info.Command_Status := Done;
   end Set_Done_Attempt;

   procedure Set_Proceed_Attempt (P_Attempt_Info : in out Type_Attempt_Info)
   is
   begin
      P_Attempt_Info.Command_Status := Proceed;
   end Set_Proceed_Attempt;

   procedure Set_Failed_Attempt (P_Attempt_Info : in out Type_Attempt_Info)
   is
   begin
      P_Attempt_Info.Command_Status := Failed;
   end Set_Failed_Attempt;


   procedure Set_Command_Status (P_Attempt_Info : in out Type_Attempt_Info; P_Status : in Type_Command_Status)
   is
   begin
      P_Attempt_Info.Command_Status := P_Status;
   end Set_Command_Status;

   function Get_Command_Status (P_Attempt_Info : in Type_Attempt_Info) return Type_Command_Status
   is
   begin
      return P_Attempt_Info.Command_Status;
   end Get_Command_Status;

   procedure Set_Attempt_Status (P_Attempt_Info : in out Type_Attempt_Info; P_Status : in Status.Type_Status)
   is
   begin
      P_Attempt_Info.Attempt_Status := P_Status;
   end Set_Attempt_Status;

   function Get_Attempt_Status (P_Attempt_Info : in Type_Attempt_Info) return Status.Type_Status
   is
   begin
      return P_Attempt_Info.Attempt_Status;
   end Get_Attempt_Status;

   function To_String (P_Attempt_Info : in Type_Attempt_Info) return String--Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return P_Attempt_Info.Command_Status'img & " " & P_Attempt_Info.Info'Img;
   end To_String;
end Attempt;
