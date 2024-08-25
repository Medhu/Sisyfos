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

   function Get_Landscape_Info (P_Landscape : in Type_Landscape) return Type_Landscape_Type_Info
   is
   begin
      return Landscape_Type_Info_List(P_Landscape);
   end Get_Landscape_Info;

   procedure Print_Patch (P_Patch : in Landscape.Type_Patch) is
--      Trav : Landscape.Pieces_Here_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Landscape.Server.Print_Patch - enter");
      end if;

--      Trav := Landscape.Pieces_Here_List.First(P_Patch.Pieces_Here);
--      while Landscape.Pieces_Here_List.Has_Element(Trav) loop

--         Text_IO.Put_Line("" & Landscape.Pieces_Here_List.Element(Trav)'Img);

--         Trav := Landscape.Pieces_Here_List.Next(Trav);
--      end loop;

      if Verbose then
         Text_IO.Put_Line ("Landscape.Server.Print_Patch - exit");
      end if;
   end Print_Patch;


end Landscape.Server;
