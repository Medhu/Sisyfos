--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2019  Frank J Jorgensen
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
with AUnit;
with AUnit.Assertions;
with Hexagon.Server_Navigation.Modify;
with Ada.Containers.Ordered_Sets;

package body Tc_Navigation_Modify is
   Verbose : constant Boolean := True;

   type Type_List_Id is
     array
       (Hexagon.Server_Navigation.Type_Navigation_Node_Id range <>) of Hexagon.Server_Navigation
       .Type_Navigation_Node_Id;
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Set_Up - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Set_Up - exit");
      end if;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down_Case (T : in out Test_Case) is
      pragma UNREFERENCED (T);
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Tear_Down - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Tear_Down - exit");
      end if;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Test functions in Hexagon Navigation Modify");
   end Name;

   function Ids_Among_Neighbours
     (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node;
      P_List_Id         : in Type_List_Id) return Boolean
   is
      Trav : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;

      Trav_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;
      Found : Boolean := True;

      use Hexagon.Server_Navigation;
   begin
      Trav_Id := P_List_Id'First;
      while Trav_Id in P_List_Id'First .. P_List_Id'Last and Found loop
         Found := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element
             (Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Find
                (P_Navigation_Node.Neighbours, P_List_Id (Trav_Id)));

         Trav_Id := Trav_Id + 1;
      end loop;

--      Text_IO.Put_Line("Found:" & Found);
      return Found;
   end Ids_Among_Neighbours;

   function Neighbours_Among_Ids
     (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node;
      P_List_Id         : in Type_List_Id) return Boolean
   is
      Trav : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;
      Found : Boolean := True;

      function In_Array (P_List_Id         : in Type_List_Id;
                        P_Navigation_Node_Id : in Hexagon.Server_Navigation.Type_Navigation_Node_Id)
                         return Boolean
      is
         Found : Boolean := False;
         Trav_Id : Hexagon.Server_Navigation.Type_Navigation_Node_Id;

         use Hexagon.Server_Navigation;
      begin
         Trav_Id := P_List_Id'First;
         while Trav_Id in P_List_Id'First .. P_List_Id'Last and not Found loop
            Found := P_List_Id(Trav_Id) = P_Navigation_Node_Id;

            Trav_Id := Trav_Id + 1;
         end loop;

         return Found;
      end In_Array;

      use Hexagon.Server_Navigation;
   begin

      Trav :=
        Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First
          (P_Navigation_Node.Neighbours);
      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element (Trav) and Found loop
--         Text_IO.Put_Line ("Trav");


            if In_Array(P_List_Id, Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Element (Trav))
            then
               Found := True;
            else
               Found := False;
            end if;

         Trav := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next (Trav);
      end loop;

      return Found;
   end Neighbours_Among_Ids;

   function Correct_Neighbours (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node;
                                P_List_Id         : in Type_List_Id)
                                return Boolean
   is
   begin
      return Ids_Among_Neighbours
        (P_Navigation_Node, P_List_Id)
        and
          Neighbours_Among_Ids
            (P_Navigation_Node, P_List_Id);
   end Correct_Neighbours;

   function No_Neighbours (P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node)
                                return Boolean
   is
      use Ada.Containers;
   begin
      return Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Length
        ( P_Navigation_Node.Neighbours ) = 0;
   end No_Neighbours;

   -- setup node 1-2 in the example map.
   procedure Test_1_2_Navigation_Node_Id (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1,
      A_Navigation_Node_2 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Navigation_Node_Id - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2)),
         Message   => "Expected neighbour 2");

      AUnit.Assertions.Assert
        (Condition => Ids_Among_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbour 1");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Navigation_Node_Id - exit");
      end if;

   end Test_1_2_Navigation_Node_Id;

   -- setup node 1-3 in the example map.
   procedure Test_1_3_Navigation_Node_Id (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Navigation_Node_Id - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2, 2 => 3)),
         Message   => "Expected neighbours 2 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1, 2 => 3)),
         Message   => "Expected neighbours 1 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all, Type_List_Id'(1 => 1, 2 => 2)),
         Message   => "Expected neighbours 1 and 2");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Navigation_Node_Id - exit");
      end if;

   end Test_1_3_Navigation_Node_Id;

   -- setup node 1-7 in the example map.
   procedure Test_1_7_Navigation_Node_Id (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3, A_Navigation_Node_4,
      A_Navigation_Node_5, A_Navigation_Node_6,
      A_Navigation_Node_7 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_7_Navigation_Node_Id - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_4 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (4),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_5 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (5),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_6 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (6),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_7 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (7),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      -- Navigation_Node_1 connected to: 2, 3, 4, 5, 6, 7
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_4.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_5.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_6.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_7.all.Id);


      -- Navigation_Node_2 connected to: 7, 1, 3
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_7.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_3.all.Id);


      -- Navigation_Node_3 connected to: 2, 1, 4
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_4.all.Id);


      -- Navigation_Node_4 connected to: 3, 1, 5
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_4.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_4.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_4.all, A_Navigation_Node_5.all.Id);


      -- Navigation_Node_5 connected to: 4, 1, 6
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_5.all, A_Navigation_Node_4.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_5.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_5.all, A_Navigation_Node_6.all.Id);


      -- Navigation_Node_6 connected to: 5, 1, 7
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_6.all, A_Navigation_Node_5.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_6.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_6.all, A_Navigation_Node_7.all.Id);


      -- Navigation_Node_7 connected to: 2, 1, 6
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_7.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_7.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_7.all, A_Navigation_Node_6.all.Id);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all,
         Type_List_Id'(1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 => 6, 6 => 7)),
         Message   => "Expected neighbours 2, 3, 4, 5, 6, 7");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all,
         Type_List_Id'(1 => 7, 2 => 1, 3=> 3)),
         Message   => "Expected neighbours 7, 1, 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all,
         Type_List_Id'(1 => 2, 2 => 1, 3 => 4)),
         Message   => "Expected neighbours 2, 1, 4");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_4.all,
         Type_List_Id'(1 => 3, 2 => 1, 3 => 5)),
         Message   => "Expected neighbours 3, 1, 5");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_5.all,
         Type_List_Id'(1 => 4, 2 => 1, 3 => 6)),
         Message   => "Expected neighbours 4, 1, 6");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_6.all,
         Type_List_Id'(1 => 5, 2 => 1, 3 => 7)),
         Message   => "Expected neighbours 5, 1, 7");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_7.all,
         Type_List_Id'(1 => 2, 2 => 1, 3 => 6)),
         Message   => "Expected neighbours 2, 1, 6");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_7_Navigation_Node_Id - exit");
      end if;

   end Test_1_7_Navigation_Node_Id;


   -- setup node 1-2 in the example map.
   -- then remove path from node 1 to node 2
   procedure Test_1_2_Navigation_Node_Id_Remove_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1,
      A_Navigation_Node_2 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Navigation_Node_Id_Remove_1 - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      AUnit.Assertions.Assert
        (Condition => No_Neighbours (A_Navigation_Node_1.all),
         Message   => "Expected no neighbours");

      AUnit.Assertions.Assert
        (Condition => Ids_Among_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbour 1");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Navigation_Node_Id_Remove_1 - exit");
      end if;

   end Test_1_2_Navigation_Node_Id_Remove_1;


   -- setup node 1-3 in the example map.
   -- then remove path from node 2 to node 3
   procedure Test_1_3_Navigation_Node_Id_Remove_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Navigation_Node_Id_Remove_2 - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 2, 2));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_1.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_3.all, A_Navigation_Node_2.all.Id);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
        (A_Navigation_Node_2.all, A_Navigation_Node_3.all.Id);
      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2, 2 => 3)),
         Message   => "Expected neighbours 2 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbours 1");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all, Type_List_Id'(1 => 1, 2 => 2)),
         Message   => "Expected neighbours 1 and 2");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Navigation_Node_Id_Remove_2 - exit");
      end if;

   end Test_1_3_Navigation_Node_Id_Remove_2;

   --
   --
   -- setup node 1-2 in the example map.
   procedure Test_1_2_Position (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1,
      A_Navigation_Node_2 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Position - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 10, 10));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 11, 11));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2)),
         Message   => "Expected neighbour 2");

      AUnit.Assertions.Assert
        (Condition => Ids_Among_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbour 1");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Position - exit");
      end if;

   end Test_1_2_Position;

   -- setup node 1-3 in the example map.
   procedure Test_1_3_Position (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Position - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 10, 10));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 11, 11));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 11, 10));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_3.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_3.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2, 2 => 3)),
         Message   => "Expected neighbours 2 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1, 2 => 3)),
         Message   => "Expected neighbours 1 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all, Type_List_Id'(1 => 1, 2 => 2)),
         Message   => "Expected neighbours 1 and 2");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Position - exit");
      end if;

   end Test_1_3_Position;


   procedure Test_1_7_Position (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3, A_Navigation_Node_4,
      A_Navigation_Node_5, A_Navigation_Node_6,
      A_Navigation_Node_7 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_7_Position - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 10, 10));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 11, 11));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 11, 10));

      A_Navigation_Node_4 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (4),
           Hexagon.Type_Hexagon_Position'(True, 10, 9));

      A_Navigation_Node_5 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (5),
           Hexagon.Type_Hexagon_Position'(True, 9, 9));

      A_Navigation_Node_6 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (6),
           Hexagon.Type_Hexagon_Position'(True, 9, 10));

      A_Navigation_Node_7 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (7),
           Hexagon.Type_Hexagon_Position'(True, 10, 11));

      -- Navigation_Node_1 connected to: 2, 3, 4, 5, 6, 7
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation_Node_1.all, A_Navigation_Node_3.all.Id);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_4.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_5.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_6.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_7.all.Pos);


      -- Navigation_Node_2 connected to: 7, 1, 3
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_7.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_3.all.Pos);


      -- Navigation_Node_3 connected to: 2, 1, 4
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_4.all.Pos);


      -- Navigation_Node_4 connected to: 3, 1, 5
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_4.all.Pos,
         A_Navigation_Node_3.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_4.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_4.all.Pos,
         A_Navigation_Node_5.all.Pos);


      -- Navigation_Node_5 connected to: 4, 1, 6
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_5.all.Pos,
         A_Navigation_Node_4.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_5.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_5.all.Pos,
         A_Navigation_Node_6.all.Pos);


      -- Navigation_Node_6 connected to: 5, 1, 7
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_6.all.Pos,
         A_Navigation_Node_5.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_6.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_6.all.Pos,
         A_Navigation_Node_7.all.Pos);


      -- Navigation_Node_7 connected to: 2, 1, 6
      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_7.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_7.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_7.all.Pos,
         A_Navigation_Node_6.all.Pos);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all,
         Type_List_Id'(1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 => 6, 6 => 7)),
         Message   => "Expected neighbours 2, 3, 4, 5, 6, 7");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all,
         Type_List_Id'(1 => 7, 2 => 1, 3=> 3)),
         Message   => "Expected neighbours 7, 1, 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all,
         Type_List_Id'(1 => 2, 2 => 1, 3 => 4)),
         Message   => "Expected neighbours 2, 1, 4");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_4.all,
         Type_List_Id'(1 => 3, 2 => 1, 3 => 5)),
         Message   => "Expected neighbours 3, 1, 5");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_5.all,
         Type_List_Id'(1 => 4, 2 => 1, 3 => 6)),
         Message   => "Expected neighbours 4, 1, 6");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_6.all,
         Type_List_Id'(1 => 5, 2 => 1, 3 => 7)),
         Message   => "Expected neighbours 5, 1, 7");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_7.all,
         Type_List_Id'(1 => 2, 2 => 1, 3 => 6)),
         Message   => "Expected neighbours 2, 1, 6");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_7_Position - exit");
      end if;

   end Test_1_7_Position;

   -- setup node 1-2 in the example map.
   -- then remove path from node 1 to node 2
   procedure Test_1_2_Position_Remove_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1,
      A_Navigation_Node_2 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Position_Remove_1 - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 10, 10));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 11, 11));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      AUnit.Assertions.Assert
        (Condition => No_Neighbours (A_Navigation_Node_1.all),
         Message   => "Expected no neighbours");

      AUnit.Assertions.Assert
        (Condition => Ids_Among_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbour 1");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_2_Position_Remove_1 - exit");
      end if;

   end Test_1_2_Position_Remove_1;

   -- setup node 1-3 in the example map.
   -- then remove path from node 2 to node 3
   procedure Test_1_3_Position_Remove_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Navigation : Hexagon.Server_Navigation.Type_Navigation;
      A_Navigation_Node_1, A_Navigation_Node_2,
      A_Navigation_Node_3 : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Position_Remove_2 - enter");
      end if;

      A_Navigation_Node_1 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (1),
           Hexagon.Type_Hexagon_Position'(True, 10, 10));

      A_Navigation_Node_2 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (2),
           Hexagon.Type_Hexagon_Position'(True, 11, 11));

      A_Navigation_Node_3 :=
        Hexagon.Server_Navigation.Modify.Create_Navigation_Node
          (A_Navigation, Hexagon.Server_Navigation.Type_Navigation_Node_Id (3),
           Hexagon.Type_Hexagon_Position'(True, 11, 10));

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_1.all.Pos,
         A_Navigation_Node_3.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_3.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_1.all.Pos);

      Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_3.all.Pos,
         A_Navigation_Node_2.all.Pos);

      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_1.all);
      Hexagon.Server_Navigation.Print_Navigation_Node (A_Navigation_Node_2.all);

      Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
        (A_Navigation,
         A_Navigation_Node_2.all.Pos,
         A_Navigation_Node_3.all.Pos);

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_1.all, Type_List_Id'(1 => 2, 2 => 3)),
         Message   => "Expected neighbours 2 and 3");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_2.all, Type_List_Id'(1 => 1)),
         Message   => "Expected neighbours 1");

      AUnit.Assertions.Assert
        (Condition => Correct_Neighbours (A_Navigation_Node_3.all, Type_List_Id'(1 => 1, 2 => 2)),
         Message   => "Expected neighbours 1 and 2");

      if Verbose then
         Text_IO.Put_Line ("Tc_Navigation_Modify.Test_1_3_Position_Remove_2 - exit");
      end if;

   end Test_1_3_Position_Remove_2;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin

      -- Based on Navigation_Node_Id
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_2_Navigation_Node_Id'Access,
         Name => "Connect Node 1 with Node 2 based by Navigation_Node_Id");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_3_Navigation_Node_Id'Access,
         Name => "Connect Node 1 with Node 2 and 3, and Node 2 and Node 3 based on Navigation_Node_Id");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_7_Navigation_Node_Id'Access,
         Name => "Connect Node 1 to 7 based on Navigation_Node_Id");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_2_Navigation_Node_Id_Remove_1'Access,
         Name => "Connect Node 1 with Node 2. Remove path from 1 to 2 based on Navigation_Node_Id");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_3_Navigation_Node_Id_Remove_2'Access,
         Name => "Connect Node 1 with Node 2 and 3, and Node 2 and Node 3.  Remove path from 2 to 3 based on Navigation_Node_Id");

      -- Based on Position
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_2_Position'Access, Name => "Connect Node 1 with Node 2 based on positions");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_3_Position'Access,
         Name => "Connect Node 1 with Node 2 and 3, and Node 2 and Node 3 based on positions");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_7_Position'Access, Name => "Connect Node 1 to 7 based on position");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_2_Position_Remove_1'Access,
         Name => "Connect Node 1 with Node 2. Remove path from 1 to 2 - based on position");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test => T, Routine => Test_1_3_Position_Remove_2'Access,
         Name => "Connect Node 1 with Node 2 and 3, and Node 2 and Node 3.  Remove path from 2 to 3 - based on position");

   end Register_Tests;

end Tc_Navigation_Modify;
