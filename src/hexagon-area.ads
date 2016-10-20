--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2016  Frank J Jorgensen
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

package Hexagon.Area is
   pragma Remote_Types;

   Delta_Max_Number : constant Integer := 10;
   Delta_Min_Number : constant Integer := -10;

   type Type_Hexagon_Delta_Numbers is range Delta_Min_Number .. Delta_Max_Number;

   type Type_Hexagon_Delta_Position (P_Valid : Boolean := False) is record
      case P_Valid is
         when True =>
            A, B : Type_Hexagon_Delta_Numbers;
         when False =>
            null;
      end case;
   end record;

   type Type_Action_Capabilities is array (Positive range <>) of Type_Hexagon_Delta_Position;

   type Type_Action_Capabilities_A is array (Positive range <>) of Type_Hexagon_Position;

   function Find
     (P_Reachable : in Type_Action_Capabilities;
      P_A, P_B    : in Type_Hexagon_Delta_Numbers)
      return        Boolean;

   procedure Read_Type_Hexagon_Delta_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Hexagon_Delta_Position);
   for Type_Hexagon_Delta_Position'Read use Read_Type_Hexagon_Delta_Position;

   procedure Write_Type_Hexagon_Delta_Position
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Hexagon_Delta_Position);
   for Type_Hexagon_Delta_Position'Write use Write_Type_Hexagon_Delta_Position;

   --
   --
   --
   --

   procedure Read_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : out Type_Action_Capabilities);
   for Type_Action_Capabilities'Read use Read_Type_Action_Capabilities;

   procedure Write_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Action_Capabilities);
   for Type_Action_Capabilities'Write use Write_Type_Action_Capabilities;

   procedure Output_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class;
      Item   : in Type_Action_Capabilities);
   for Type_Action_Capabilities'Output use Output_Type_Action_Capabilities;

   function Input_Type_Action_Capabilities
     (Stream : access Root_Stream_Type'Class)
      return   Type_Action_Capabilities;
   for Type_Action_Capabilities'Input use Input_Type_Action_Capabilities;

end Hexagon.Area;
