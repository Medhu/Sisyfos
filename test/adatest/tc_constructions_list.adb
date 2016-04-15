--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013  Frank J Jorgensen
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
with AUnit.Assertions;
with Construction;
with Ada.Containers;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Test_Piece;
with Construction.Server;

package body Tc_Constructions_List is

   Verbose : constant Boolean := True;
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Testing functionality for including/excluding and updating in Construction_Lists");
   end Name;

   procedure Test_Constructions_Include_One_Construction
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Construction_List : Construction.Construction_List.Set;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_One_Construction - enter");
      end if;

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall1));

        AUnit.Assertions.Assert
          (Condition => Construction.Construction_List.Length(A_Construction_List) = 1,
           Message   => "Did not include one construction as expected");

      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_One_Construction - exit");
      end if;

   end Test_Constructions_Include_One_Construction;

   procedure Test_Constructions_Include_Three_Constructions
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Construction_List : Construction.Construction_List.Set;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_Three_Constructions - enter");
      end if;

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall1));
      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall2));
      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall3));

        AUnit.Assertions.Assert
          (Condition => Construction.Construction_List.Length(A_Construction_List) = 3,
           Message   => "Expected 3 items");

      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_Three_Constructions - exit");
      end if;

   end Test_Constructions_Include_Three_Constructions;

   procedure Test_Constructions_Include_Three_Constructions_Update
       (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Construction_List : Construction.Construction_List.Set;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_Three_Constructions_Update - enter");
      end if;

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall1));
      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall2));
      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall3));

        AUnit.Assertions.Assert
          (Condition => Construction.Construction_List.Length(A_Construction_List) = 3,
           Message   => "Expected 3 items (a)");

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall2));

        AUnit.Assertions.Assert
          (Condition => Construction.Construction_List.Length(A_Construction_List) = 3,
           Message   => "Expected 3 items (b)");

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall2));

        AUnit.Assertions.Assert
          (Condition => Construction.Construction_List.Length(A_Construction_List) = 3,
           Message   => "Expected 3 items (b)");

      if Verbose then
         Text_IO.Put_Line("tc_constructions_list.Test_Constructions_Include_Three_Constructions_Update - exit");
      end if;

   end Test_Constructions_Include_Three_Constructions_Update;

   procedure Test_Blocking_Way_1
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --A_Piece                       : Piece.Type_Piece;
      --A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      --Ret_Status                    : Status.Type_Status;

--        Current_Player_Id : Player.Type_Player_Id;
--        Countdown         : Positive;
--        System_Messages   : Observation.Activity.Activity_Report.Vector;
--        Game_Status       : Status.Type_Game_Status;
--
--        use Ada.Containers;
--        use Status;
--        use Piece;
--        use Hexagon;
      A_Construction_List : Construction.Construction_List.Set;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Blocking_Way_1 - enter");
      end if;

      Construction.Server.Init(Test_Piece.Construction_Type_Info_List);

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall1));
      --Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall2));
      --Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall3));

        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 1),
           Message   => "Expected Way 1 to be blocked because we put a 'Wall1' which is defined to block way 1");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 2),
           Message   => "Expected Way 2 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 3),
           Message   => "Expected Way 3 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 4),
           Message   => "Expected Way 4 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 5),
           Message   => "Expected Way 5 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 6),
           Message   => "Expected Way 6 NOT to be blocked");

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall3));

        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 1),
           Message   => "Expected Way 1 to be blocked because we put a 'Wall1' which is defined to block way 1");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 2),
           Message   => "Expected Way 2 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 3),
           Message   => "Expected Way 3 to be blocked  because we put a 'Wall1' which is defined to block way 1");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 4),
           Message   => "Expected Way 4 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 5),
           Message   => "Expected Way 5 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 6),
           Message   => "Expected Way 6 NOT to be blocked");

      Construction.Construction_List.Include(A_Construction_List, Construction.Type_Construction'(Test_Piece.Construction_Wall6));

        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 1),
           Message   => "Expected Way 1 to be blocked because we put a 'Wall1' which is defined to block way 1");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 2),
           Message   => "Expected Way 2 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 3),
           Message   => "Expected Way 3 to be blocked  because we put a 'Wall3' which is defined to block way 3");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 4),
           Message   => "Expected Way 4 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => not Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 5),
           Message   => "Expected Way 5 NOT to be blocked");
        AUnit.Assertions.Assert
          (Condition => Construction.Server.Is_Blocking_Neighbour_Number(A_Construction_List, 6),
           Message   => "Expected Way 6 to be blocked because we put a 'Wall6' which is defined to block way 6");

   end Test_Blocking_Way_1;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Constructions_Include_One_Construction'Access,
         Name    => "Test including one construction");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Constructions_Include_Three_Constructions'Access,
         Name    => "Test including three constructions");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Constructions_Include_Three_Constructions_Update'Access,
         Name    => "Test including three construction, and update one of them (not so relevant for constructions, but it test include of an existing construction)");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Blocking_Way_1'Access,
         Name    => "Test including three construction, and update one of them (not so relevant for constructions, but it test include of an existing construction)");

   end Register_Tests;

end Tc_Constructions_List;
