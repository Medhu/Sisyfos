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

private with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Streams;
with Ada.Containers.Vectors;

package Utilities is

   pragma Remote_Types;

   package RemoteString is

      type Type_String is private;
      procedure Type_String_Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item   : Type_String);
      procedure Type_String_Read
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item   : out Type_String);
      for Type_String'Write use Type_String_Write;
      for Type_String'Read use Type_String_Read;

      procedure Type_String_Output
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item   : in Type_String);
      function Type_String_Input
        (Stream : access Ada.Streams.Root_Stream_Type'Class)
         return   Type_String;
      for Type_String'Output use Type_String_Output;
      for Type_String'Input use Type_String_Input;

      function To_Unbounded_String (P_s : in String) return Type_String;
      function To_String (P_s : in Type_String) return String;
      function "=" (P_Left : in Type_String; P_Right : in String) return Boolean;
      function "&" (P_Left : in String; P_Right : in Type_String) return Type_String;
      function "&" (P_Left : in Type_String; P_Right : in String) return Type_String;

      type Type_Command_Parameters is array (1..3) of Utilities.RemoteString.Type_String;

   private
      type Type_String is new Ada.Strings.Unbounded.Unbounded_String;

   end RemoteString;

   package RemoteString_List is new Ada.Containers.Vectors(Positive, Utilities.RemoteString.Type_String,
                                                             Utilities.RemoteString."=");

   function Starting_With
     (P_String        : in Utilities.RemoteString.Type_String;
      P_Starting_With : in RemoteString.Type_String) return Boolean;

   procedure Delete_Starting_With (P_Properties : in out RemoteString_List.Vector;
                                   P_Starting_With : in RemoteString.Type_String);


   procedure Get_Files (P_Properties : in out RemoteString_List.Vector;
                        P_Directory : in String;
                        P_Property : in String);

   function Number_To_Fixed_String
     (P_Number : in Natural;
      P_Length : in Positive) return String;

end Utilities;
