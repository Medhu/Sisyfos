--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2016  Frank J Jorgensen
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

with AUnit.Assertions;
with Text_IO;
with Utilities;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package body Tc_Server_Properties is

   Verbose : constant Boolean := False;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format(
"Test that list for server properties works as intended (helper functionality)");
   end Name;

   procedure Test_Remove_Properties_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_1 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 1,
           Message   => "Should have had 1 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 0,
           Message   => "Should have had 0 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_1 - exit");
      end if;
   end Test_Remove_Properties_1;

   procedure Test_Remove_Properties_2 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_2 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 1,
           Message   => "Should have had 1 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Wrong:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 1,
           Message   => "Should have had 0 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_2 - exit");
      end if;
   end Test_Remove_Properties_2;

   procedure Test_Remove_Properties_3 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_3 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata A") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata B") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 2,
           Message   => "Should have had 2 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 0,
           Message   => "Should have had 0 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_3 - exit");
      end if;
   end Test_Remove_Properties_3;

   procedure Test_Remove_Properties_4 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_4 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Test:Somedata 1") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata 1") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata 2") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 3,
           Message   => "Should have had 3 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 1,
           Message   => "Should have had 1 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_4 - exit");
      end if;
   end Test_Remove_Properties_4;

   procedure Test_Remove_Properties_5 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_5 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata E") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata F") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Test:Somedata 1") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 3,
           Message   => "Should have had 3 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 1,
           Message   => "Should have had 1 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_5 - exit");
      end if;
   end Test_Remove_Properties_5;

   procedure Test_Remove_Properties_6 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is

      Server_Info : Utilities.RemoteString_List.Vector;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_6 - enter");
      end if;

      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Test A:Somedata 1") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata E") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:Somedata F") );
      Utilities.RemoteString_List.Append(Server_Info, Utilities.RemoteString.To_Unbounded_String("Test B:Somedata 1") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 4,
           Message   => "Should have had 4 element in property list");
      Utilities.Delete_Starting_With(Server_Info, Utilities.RemoteString.To_Unbounded_String("Category:") );

        AUnit.Assertions.Assert
          (Condition => Utilities.RemoteString_List.Length(Server_Info) = 2,
           Message   => "Should have had 2 element in property list");

      Utilities.RemoteString_List.Clear(Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tc_Server_Properties.Test_Remove_Properties_6 - exit");
      end if;
   end Test_Remove_Properties_6;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_1'Access,
         Name    => "Test that property lists for server can be removed");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_2'Access,
         Name    => "Test that property lists for server can be removed");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_3'Access,
         Name    => "Test that property lists for server can be removed");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_4'Access,
         Name    => "Test that property lists for server can be removed");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_5'Access,
         Name    => "Test that property lists for server can be removed");
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Remove_Properties_6'Access,
         Name    => "Test that property lists for server can be removed");
   end Register_Tests;

end Tc_Server_Properties;
