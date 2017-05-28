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

with Ada.Strings.Unbounded;
with Ada.Streams;
with Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;

package body Utilities is

   Verbose : constant Boolean := False;

   package body RemoteString is
      function To_Unbounded_String (P_s : in String) return Type_String is
      begin
         return Utilities.RemoteString.Type_String
             (Ada.Strings.Unbounded.To_Unbounded_String (P_s));
      end To_Unbounded_String;

      function To_String (P_s : in Type_String) return String is
         Ret : String (1 .. Length (P_s));
      begin
         Ret := Slice (P_s, 1, Length (P_s));
         return Ret;
      end To_String;

      function "="
        (P_Left  : in Type_String;
         P_Right : in String) return Boolean
      is
      begin
         return To_String (P_Left) = P_Right;
      end "=";

      function "&"
        (P_Left  : in String;
         P_Right : in Type_String) return Type_String
      is
      begin
         return To_Unbounded_String (P_Left) & P_Right;
      end "&";

      function "&"
        (P_Left  : in Type_String;
         P_Right : in String) return Type_String
      is
      begin
         return P_Left & To_Unbounded_String (P_Right);
      end "&";

      procedure Type_String_Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item   : Type_String)
      is
      begin
         if Verbose then
            Text_IO.Put_Line ("Type_String_Write-enter");
         end if;

         Type_String'Output (Stream, Item);

         String'Write
           (Stream,
            Ada.Strings.Unbounded.To_String
              (Ada.Strings.Unbounded.Unbounded_String (Item)));

         if Verbose then
            Text_IO.Put_Line ("Type_String_Write-exit");
         end if;

      end Type_String_Write;
      --
      procedure Type_String_Read
        (Stream :     access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Type_String)
      is
      begin
         if Verbose then
            Text_IO.Put_Line ("Type_String_Read-enter");
         end if;

         declare
            Read_String : Type_String := Type_String'Input (Stream);
            The_String  : String
            (1 .. Utilities.RemoteString.Length (Read_String));
         begin
            String'Read (Stream, The_String);

            Item :=
              Type_String
                (Ada.Strings.Unbounded.To_Unbounded_String (The_String));
         end;

         if Verbose then
            Text_IO.Put_Line ("Type_String_Read-exit");
         end if;

      end Type_String_Read;

      procedure Type_String_Output
        (Stream :    access Ada.Streams.Root_Stream_Type'Class;
         Item   : in Type_String)
      is
         String_Length : Integer;
      begin
         if Verbose then
            Text_IO.Put_Line ("Type_String_Output-enter");
         end if;

         String_Length :=
           Ada.Strings.Unbounded.Length
             (Ada.Strings.Unbounded.Unbounded_String (Item));
         Positive'Write (Stream, String_Length);

         if Verbose then
            Text_IO.Put_Line ("Type_String_Output-exit");
         end if;
      end Type_String_Output;

      function Type_String_Input
        (Stream : access Ada.Streams.Root_Stream_Type'Class) return Type_String
      is
         Ret           : Type_String;
         String_Length : Integer;
      begin
         if Verbose then
            Text_IO.Put_Line ("Type_String_Input-enter");
         end if;

         Positive'Read (Stream, String_Length);

         Ret :=
           Type_String
             (Ada.Strings.Unbounded.To_Unbounded_String
                (Length => String_Length));

         if Verbose then
            Text_IO.Put_Line ("Type_String_Input-exit");
         end if;

         return Ret;
      end Type_String_Input;

   end RemoteString;

   function Starting_With
     (P_String        : in Utilities.RemoteString.Type_String;
      P_Starting_With : in RemoteString.Type_String) return Boolean
   is
      Does_Start_With : Boolean;
      Pos             : Natural;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Utilities.Starting_With - enter " &
            " P_String=" &
            Utilities.RemoteString.To_String (P_String) &
            " P_Starting_With=" &
            Utilities.RemoteString.To_String (P_Starting_With));
      end if;

      Pos :=
        Ada.Strings.Unbounded.Index
          (Ada.Strings.Unbounded.To_Unbounded_String
             (Utilities.RemoteString.To_String (P_String)),
           Utilities.RemoteString.To_String (P_Starting_With),
           1);

      if Pos > 0 then
         Does_Start_With := True;
      else
         Does_Start_With := False;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Utilities.Starting_With - exit");
      end if;

      return Does_Start_With;
   end Starting_With;

   procedure Delete_Starting_With
     (P_Properties    : in out RemoteString_List.Vector;
      P_Starting_With : in     RemoteString.Type_String)
   is
      An_Element : RemoteString.Type_String;

      Trav : RemoteString_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Utilities.Delete_Starting_With - enter " &
            Utilities.RemoteString.To_String (P_Starting_With));
      end if;

      Trav := RemoteString_List.First (P_Properties);
      while RemoteString_List.Has_Element (Trav) loop

         An_Element := RemoteString_List.Element (Trav);

         if Starting_With (An_Element, P_Starting_With) then
            RemoteString_List.Delete (P_Properties, Trav);
            Trav := RemoteString_List.First (P_Properties);
         else
            Trav := RemoteString_List.Next (Trav);
         end if;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Utilities.Delete_Starting_With - exit");
      end if;

   end Delete_Starting_With;

   procedure Get_Files
     (P_Properties : in out RemoteString_List.Vector;
      P_Directory  : in     String;
      P_Property   : in     String)
   is
      A_Search      : Ada.Directories.Search_Type;
      A_Search_Item : Ada.Directories.Directory_Entry_Type;
   begin
      if Verbose then
         Text_IO.Put_Line ("Utilities.Get_Files - enter " & P_Directory);
      end if;

      Ada.Directories.Start_Search (A_Search, P_Directory, Pattern => "*.dat");
      while Ada.Directories.More_Entries (A_Search) loop
         Ada.Directories.Get_Next_Entry (A_Search, A_Search_Item);

         Utilities.RemoteString_List.Append
           (P_Properties,
            Utilities.RemoteString.To_Unbounded_String
              (P_Property & Ada.Directories.Simple_Name (A_Search_Item)));
      end loop;
      Ada.Directories.End_Search (Search => A_Search);

      if Verbose then
         Text_IO.Put_Line ("Utilities.Get_Files - exit");
      end if;
   exception
      when others =>
         Text_IO.Put_Line("Server failed while searching for the directory: '" & P_Directory & "'");
         raise;
   end Get_Files;

   function Number_To_Fixed_String
     (P_Number : in Natural;
      P_Length : in Positive) return String
   is
      Res : String (1 .. P_Length);
   begin
      Ada.Strings.Fixed.Move
        (Source =>
           Ada.Strings.Fixed.Trim (Natural'Image (P_Number), Ada.Strings.Left),
         Target  => Res,
         Drop    => Ada.Strings.Right,
         Justify => Ada.Strings.Right,
         Pad     => ' ');

      return Res;
   end Number_To_Fixed_String;

end Utilities;
