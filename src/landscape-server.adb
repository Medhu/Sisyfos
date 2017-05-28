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
with Text_IO;
package body Landscape.Server is
   Verbose : constant Boolean := False;
   Landscape_Type_Info_List : Type_Landscape_Type_Info_List_Access;

   procedure Init (P_Landscape_Info : in Type_Landscape_Type_Info_List)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Landscape.Server.Init - enter");
      end if;

      Landscape_Type_Info_List := new Type_Landscape_Type_Info_List'(P_Landscape_Info);

      if Verbose then
         Text_IO.Put_Line ("Landscape.Server.Init - exit");
      end if;
   end Init;

   function Is_Patch_Empty (P_Patch : in Landscape.Type_Patch) return Boolean is
      use Ada.Containers;
   begin

      return Landscape.Pieces_Here_List.Length (P_Patch.Pieces_Here) = 0;
   end Is_Patch_Empty;

   function Has_Patch_Free_Slot (P_Patch : in Landscape.Type_Patch) return Boolean is
      use Ada.Containers;

   begin

      if Verbose then
         Text_IO.Put_Line
           ("Landscape.Has_Patch_Free_Slot - enter - exit length=" &
            Landscape.Pieces_Here_List.Length (P_Patch.Pieces_Here)'Img);
      end if;

      return Landscape.Pieces_Here_List.Length (P_Patch.Pieces_Here) <
        Ada.Containers.Count_Type (Landscape.Server.Get_Landscape_Info(P_Patch.Landscape_Here).Max_Pieces_Here);
   end Has_Patch_Free_Slot;

   function Get_Landscape_Info (P_Landscape : in Type_Landscape) return Type_Landscape_Type_Info
   is
   begin
      return Landscape_Type_Info_List(P_Landscape);
   end Get_Landscape_Info;

end Landscape.Server;
