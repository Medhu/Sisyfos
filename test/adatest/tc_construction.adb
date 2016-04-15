with AUnit.Assertions;
with Test_ServerRCI;
with Text_IO;
with Piece;
with Piece.Client_Piece;
with Piece.Server;
with Landscape;
with Hexagon.Area.Server_Area;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Utilities;
with Ada.Strings.Unbounded;
with Player;
with Server.Server;
with Test_Piece;
with Status;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Observation;
with Construction;
with Action;

package body Tc_Construction is
   Verbose : constant Boolean := True;

   Player_Id_1, Player_Id_2   : Player.Type_Player_Id;
   Map_Player_1, Map_Player_2 : Hexagon.Client_Map.Type_Client_Map_Info;
   Test_Class1                : Test_Piece.Type_My_Test_Piece_Access_Class;
   Test_Class2                : Test_Piece.Type_My_Test_House_Access_Class;

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
      return Format ("Test construction procedure");
   end Name;

   procedure Test_Game_Start (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Game_Start - enter");
      end if;

      Test_Class1               := new Test_Piece.Type_My_Test_Piece;
      Test_Class1.Id            := Piece.Undefined_Piece_Id;
      Test_Class1.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class1.Player_Id     := Player.Undefined_Player_Id;

      Test_Class2               := new Test_Piece.Type_My_Test_House;
      Test_Class2.Id            := Piece.Undefined_Piece_Id;
      Test_Class2.Type_Of_Piece := Piece.Undefined_Piece_Type;
      Test_Class2.Player_Id     := Player.Undefined_Player_Id;

      Server.Server.Init
        (Test_Class1.all,
         Test_Class2.all,
         Test_Piece.Landscapes_Type_Info_List,
         Test_Piece.Pieces_Type_Info_List,
         Test_Piece.Houses_Type_Info_List,
         Test_Piece.Construction_Type_Info_List,
         Test_Piece.Effect_Type_Info_List,
         Test_Piece.Test_Creating_Game'Access,
         Test_Piece.Test_Saving_Game'Access,
         Test_Piece.Test_Loading_Game'Access,
         Test_Piece.Test_Joining_Game'Access,
         Test_Piece.Test_Leaving_Game'Access,
         Test_Piece.Test_Start_Game'Access,
         Test_Piece.Test_Upkeep_Game'Access,
         Test_Piece.Test_Start_Turn'Access,
         Test_Piece.Test_End_Turn'Access,
         Test_Piece.Test_End_Game'Access);
      Server.Server.Start;

      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User A"));
      Utilities.RemoteString_List.Append(Player_Name_List, Utilities.RemoteString.To_Unbounded_String ("User B"));
      Test_ServerRCI.Create_Game
        (Utilities.RemoteString.To_Unbounded_String ("test0000.dat"),
         Player_Name_List,
         Adm_Status);

      Test_ServerRCI.Join_Game
        (Utilities.RemoteString.To_Unbounded_String ("User A"),
         Adm_Status,
         Player_Id_1);
      Test_ServerRCI.Join_Game
        (Utilities.RemoteString.To_Unbounded_String ("User B"),
         Adm_Status,
         Player_Id_2);
      AUnit.Assertions.Assert
        (Condition => Player_Id_1 /= Player_Id_2,
         Message   => "Player Id's from Game engine are equal");

      AUnit.Assertions.Assert
        (Condition => Player_Id_1 = 1 or Player_Id_2 = 1 or Player_Id_1 = 2 or Player_Id_2 = 2,
         Message   => "Expected Player Id's to be 1 or 2");

      Hexagon.Client_Map.Init_Client_Map (Map_Player_1);
      Hexagon.Client_Map.Init_Client_Map (Map_Player_2);

      Hexagon.Client_Map.Get_Map (1, Map_Player_1);
      Hexagon.Client_Map.Get_Map (2, Map_Player_2);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Game_Start_01.html"));

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Game_Start - exit");
      end if;
   end Test_Game_Start;

   procedure Test_Perform_Construction_AP_1
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_AP_1 - enter");
      end if;

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_AP_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_Piece.Construction_Action_Point_Example := 10;

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);

      Test_Piece.Construction_Action_Point_Example := 1;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_AP_1_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "Expected Out_Of_Moves, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_AP_1 - exit");
      end if;

   end Test_Perform_Construction_AP_1;

   procedure Test_Perform_Construction_Allowed_Patch_1
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;
      A_Server_Piece                : Piece.Server.Type_Piece_Access_Class;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Allowed_Patch_1 - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Allowed_Patch_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      A_Server_Piece := Piece.Server.Find_Piece_In_List(A_Piece.Id).Actual_Piece;

      AUnit.Assertions.Assert
        (Condition =>  A_Server_Piece.all.Action_Points = 5,
         Message   => "Expected Action Point=5 before Perform_Construction, but found " & A_Server_Piece.all.Action_Points'Img);


      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Allowed_Patch_1_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Could not create piece " & Ret_Status'Img);

      A_Server_Piece := Piece.Server.Find_Piece_In_List(A_Piece.Id).Actual_Piece;

      AUnit.Assertions.Assert
        (Condition =>  A_Server_Piece.all.Action_Points = 4,
         Message   => "Expected Action Point=5 after Perform_Construction, but found " & A_Server_Piece.all.Action_Points'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Allowed_Patch_1 - exit");
      end if;

   end Test_Perform_Construction_Allowed_Patch_1;

   procedure Test_Perform_Construction_Occupied_Patch
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;

      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Occupied_Patch - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Occupied_Patch_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 5);

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Expected Target_Patch_Occupied got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Occupied_Patch - exit");
      end if;

   end Test_Perform_Construction_Occupied_Patch;

   procedure Test_Perform_Construction_Outside_Capability
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Outside_Capability - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Outside_Capability_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 50, 50);

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Outside_Capability_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Reachable,
         Message   => "Expected Not_Reachable got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Outside_Capability - exit");
      end if;

   end Test_Perform_Construction_Outside_Capability;

   procedure Test_Perform_Construction_Not_Players_Turn
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Not_Players_Turn - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Not_Players_Turn_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 6);

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         2,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Not_Players_Turn_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   => "Expected Not_Players_Turn, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Not_Players_Turn - exit");
      end if;

   end Test_Perform_Construction_Not_Players_Turn;

   procedure Test_Perform_Construction_Construct_Two_Things
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("tc_construction.Test_Perform_Construction_Construct_Two_Things - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_02.html"));

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall2,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Two_Things_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected Ok, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Two_Things - exit");
      end if;

   end Test_Perform_Construction_Construct_Two_Things;

   procedure Test_Perform_Construction_Construct_Twice
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Twice - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Twice_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_ServerRCI.Perform_Construction
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Construction_Construct_Twice_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Construction_Exists,
         Message   => "Expected Construction_Exists, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Construction_Construct_Twice - exit");
      end if;

   end Test_Perform_Construction_Construct_Twice;

   procedure Test_Perform_Demolition_AP_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_AP_1 - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_AP_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_AP_1_02.html"));

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_Piece.Demolition_Action_Point_Example := 10;

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);

      Test_Piece.Demolition_Action_Point_Example := 1;

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_AP_1_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Out_Of_Moves,
         Message   => "Expected Out_Of_Moves got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_AP_1 - exit");
      end if;

   end Test_Perform_Demolition_AP_1;


   procedure Test_Perform_Demolition_1 (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;
      A_Server_Piece    : Piece.Server.Type_Piece_Access_Class;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_1 - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_02.html"));

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      A_Server_Piece := Piece.Server.Find_Piece_In_List(A_Piece.Id).Actual_Piece;

      AUnit.Assertions.Assert
        (Condition =>  A_Server_Piece.all.Action_Points = 5,
         Message   => "Expected Action Point=5 before Perform_Demolition, but found " & A_Server_Piece.all.Action_Points'Img);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path & "tc_construction-Test_Perform_Demolition_1_03.html"));

      A_Server_Piece := Piece.Server.Find_Piece_In_List(A_Piece.Id).Actual_Piece;

      AUnit.Assertions.Assert
        (Condition =>  A_Server_Piece.all.Action_Points = 4,
         Message   => "Expected Action Point=5 after Perform_Demolition, but found " & A_Server_Piece.all.Action_Points'Img);

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected Ok got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_1 - exit");
      end if;

   end Test_Perform_Demolition_1;

   procedure Test_Perform_Demolition_Occupied_Patch
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Occupied_Patch - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Occupied_Patch_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 5);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Occupied_Patch_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Target_Patch_Occupied,
         Message   => "Expected Target_Patch_Occupied got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Occupied_Patch - exit");
      end if;

   end Test_Perform_Demolition_Occupied_Patch;

   procedure Test_Perform_Demolition_Outside_Capability
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Outside_Capability - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Outside_Capability_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 50, 50);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Outside_Capability_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Reachable,
         Message   => "Expected Not_Reachable got status " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Outside_Capability - exit");
      end if;

   end Test_Perform_Demolition_Outside_Capability;

   procedure Test_Perform_Demolition_Not_Players_Turn
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Not_Players_Turn - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Not_Players_Turn_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_2, 5, 6);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         2,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Not_Players_Turn_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Not_Players_Turn,
         Message   => "Expected Nt_Players_Turn, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Not_Players_Turn - exit");
      end if;

   end Test_Perform_Demolition_Not_Players_Turn;

   procedure Test_Perform_Demolition_Construct_Two_Things
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Two_Things - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_02.html"));

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall2,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Two_Things_03.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Ok,
         Message   => "Expected Ok, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Two_Things - exit");
      end if;

   end Test_Perform_Demolition_Construct_Two_Things;

   procedure Test_Perform_Demolition_Construct_Twice
     (CWTC : in out AUnit.Test_Cases.Test_Case'Class)
   is
      A_Piece                       : Piece.Type_Piece;
      A_Patch, A_Construction_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Ret_Status                    : Status.Type_Status;

      Current_Player_Id : Player.Type_Player_Id;
      Countdown         : Positive;
      System_Messages   : Observation.Activity.Activity_Report.Vector;
      Game_Status       : Status.Type_Game_Status;
      Ret               : Boolean;

      use Ada.Containers;
      use Status;
      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Twice - enter");
      end if;

      Ret := Test_ServerRCI.End_Turn(1);
      Ret := Test_ServerRCI.End_Turn(2);

      A_Piece.Id := 1;
      Hexagon.Client_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Twice_01.html"),
         Map_Player_1);

      Test_ServerRCI.Get_Updates_Summary
        (Player_Id_1,
         Current_Player_Id,
         Countdown,
         Game_Status,
         System_Messages);

      A_Patch              := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 4, 4);
      A_Construction_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (Map_Player_1, 5, 6);

      Test_ServerRCI.Perform_Demolition
        (Action.Type_Action_Type(1),
         A_Piece.Id,
         A_Patch.all.Pos,
         A_Construction_Patch.all.Pos,
         Test_Piece.Construction_Wall1,
         1,
         Ret_Status);
      Hexagon.Server_Map.Save_Scenario
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Test_Piece.HTML_Path &
            "tc_construction-Test_Perform_Demolition_Construct_Twice_02.html"));

      AUnit.Assertions.Assert
        (Condition => Ret_Status = Status.Construction_Doesnt_Exist,
         Message   => "Expected Construction_Exists, got " & Ret_Status'Img);

      if Verbose then
         Text_IO.Put_Line ("tc_construction.Test_Perform_Demolition_Construct_Twice - exit");
      end if;

   end Test_Perform_Demolition_Construct_Twice;

   procedure Test_Game_Stop (CWTC : in out AUnit.Test_Cases.Test_Case'Class) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("tc_attempt_move.Test_Game_Stop - enter");
      end if;

      Test_ServerRCI.Stop;

      AUnit.Assertions.Assert (Condition => True, Message => "...");

      if Verbose then
         Text_IO.Put_Line ("tc_attempt_move.Test_Game_Stop - exit");
      end if;
   end Test_Game_Stop;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      --
      -- Test Attempt_Move
      --
      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Start'Access,
         Name    => "Test Start Game");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_AP_1'Access,
         Name    => "Test Perform Construction too few AP");


      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Allowed_Patch_1'Access,
         Name    => "Test Perform Construction on allowed patch 1");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Occupied_Patch'Access,
         Name    => "Test Perform Construction on occupied patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Outside_Capability'Access,
         Name    => "Test Perform Construction Outside capability");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Not_Players_Turn'Access,
         Name    => "Test Perform Construction Not players turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Construct_Two_Things'Access,
         Name    => "Test Perform Construction on patch  where there already is a construction");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Construction_Construct_Twice'Access,
         Name    =>
           "Test Perform Construction on patch  where there already is an exisiting construction of the same type");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_AP_1'Access,
         Name    => "Test Perform Demolition too few AP");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_1'Access,
         Name    => "Test Perform Demolition that succeeds");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Occupied_Patch'Access,
         Name    => "Test Perform Demolition on occupied patch");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Outside_Capability'Access,
         Name    => "Test Perform Demolition ouside capability");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Not_Players_Turn'Access,
         Name    => "Test Perform Demolition Not players turn");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Perform_Demolition_Construct_Twice'Access,
         Name    => "Test Perform Demolition of same construction twice");

      AUnit.Test_Cases.Registration.Register_Routine
        (Test    => T,
         Routine => Test_Game_Stop'Access,
         Name    => "Test Stop Game");
   end Register_Tests;

end Tc_Construction;
