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
with Ada.Containers.Ordered_Maps;

package Effect is
   pragma Remote_Types;

   type Type_Effect_Name is new Positive;

   type Type_Effect is record
      Effect_Name : Type_Effect_Name;
      Aux         : Natural;
   end record;

   function Effect_Less_Than_Keys (Left, Right : Type_Effect_Name) return Boolean;
   function Effect_Equal_Element (Left, Right : Type_Effect) return Boolean;


   package Effect_List is new Ada.Containers.Ordered_Maps
     (Type_Effect_Name,
      Type_Effect,
      Effect_Less_Than_Keys,
      Effect_Equal_Element
     );

   procedure Print_Effect_List (P_Effect_List : in Effect.Effect_List.Map);

end Effect;
