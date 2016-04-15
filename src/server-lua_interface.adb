with Lua;
with Text_IO;
with Player;
with Hexagon;
with Piece;
with Construction;
with Effect;
with Status;
with Server.ServerAPI;
with Hexagon.Area;
with Hexagon.Area.Server_Area;
with Interfaces;
with Interfaces.C;
with Utilities;
with Piece.Server;
with Landscape;
with Action;
--
package body Server.Lua_Interface is

   procedure Print_Stack (P_Lua_State : in Lua.Lua_State) is
      Max_Index, N : Lua.Lua_Index;
   begin
      Text_IO.Put_Line ("Stack:");
      Max_Index := Lua.Get_Top (P_Lua_State);
      N         := 1;
      while N <= Max_Index loop

         Text_IO.Put_Line
           ("Index " & N'Img & " " & Lua.Get_Type (P_Lua_State, N)'Img);
         N := N + 1;
      end loop;
   end Print_Stack;

   procedure Init (P_Lua_State : in Lua.Lua_State) is
      Stack_Top : Lua.Lua_Index;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Init - enter");
      end if;

      --
      Stack_Top := Lua.Get_Top (P_Lua_State);

      -- Status constants
      Lua.Create_Table (P_Lua_State);

      for Trav_Status in Status.Type_Status'Range loop
         Lua.Push (P_Lua_State, Trav_Status'Img);
         Lua.Push
           (P_Lua_State,
            Lua.Lua_Integer (Status.Type_Status'Pos (Trav_Status)));

         Lua.Set_Table (P_Lua_State, 1);
      end loop;

      Lua.Register_Object (P_Lua_State, "Sisyfos.Status");

      -- Piece Category constants
      Lua.Create_Table (P_Lua_State);

      for Trav_Category in Piece.Type_Category'Range loop
         Lua.Push (P_Lua_State, Trav_Category'Img);
         Lua.Push
           (P_Lua_State,
            Lua.Lua_Integer (Piece.Type_Category'Pos (Trav_Category)));

         Lua.Set_Table (P_Lua_State, 1);
      end loop;

      Lua.Register_Object (P_Lua_State, "Sisyfos.Category");

      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Observe_Game_Minimum_Details",
         Observe_Game_Minimum_Details'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Observe_Game",
         Observe_Game'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Create_Piece",
         Create_Piece'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Put_Piece",
         Put_Piece'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Remove_Piece",
         Remove_Piece'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Attack_Pos",
         Perform_Attack_Pos'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Attack_Path",
         Perform_Attack_Path'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Move_Pos",
         Perform_Move_Pos'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Move_Path",
         Perform_Move_Path'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Ranged_Attack",
         Perform_Ranged_Attack'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Construction",
         Perform_Construction'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Demolition",
         Perform_Demolition'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Grant_Piece_Effect",
         Grant_Piece_Effect'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Revoke_Piece_Effect",
         Revoke_Piece_Effect'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Grant_Patch_Effect",
         Grant_Patch_Effect'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Revoke_Patch_Effect",
         Revoke_Patch_Effect'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Patch_Effect",
         Perform_Patch_Effect'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Perform_Piece_Effect",
         Perform_Piece_Effect'Access);
      Lua.Register_Function (P_Lua_State, "Sisyfos.End_Turn", End_Turn'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Observation_Area",
         Observation_Area'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Movement_Capability",
         Movement_Capability'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Attack_Capability",
         Attack_Capability'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Find_Piece_In_List",
         Find_Piece_In_List'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Opponents_Activity_Report_Append",
         Opponents_Activity_Report_Append'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Player_Activity_Report_Append",
         Player_Activity_Report_Append'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Opponents_System_Report_Append",
         Opponents_System_Report_Append'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Player_System_Report_Append",
         Player_System_Report_Append'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Get_Player_Name",
         Get_Player_Name'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Get_Map_Terrain",
         Get_Map_Terrain'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Get_Map_Construction_List",
         Get_Map_Construction_List'Access);
      Lua.Register_Function
        (P_Lua_State,
         "Sisyfos.Get_Map_Pieces_List",
         Get_Map_Pieces_List'Access);

      Text_IO.Put_Line ("Lua API Initialised");

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Init - exit");
      end if;
   end Init;

   function Parameter_Hexagon_Area_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     : in Lua.Lua_Index)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A
   is
      Trav_Parameters, Max_Parameters : Integer;

      Stack_Top_On_Start : Lua.Lua_Index;

      Area : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
      Lua_Integer_A, Lua_Integer_B : Lua.Lua_Integer;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Area_Input - enter P__Index=" &
            P_Index'Img &
            " top " &
            Lua.Get_Top (P_Lua_State)'Img);
         Print_Stack (P_Lua_State);
      end if;

      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Max_Parameters := Integer (Lua.Raw_Len (P_Lua_State, P_Index));

      Area :=
        new Hexagon.Area.Type_Action_Capabilities_A (1 .. Max_Parameters);

      Trav_Parameters := 1;
      while Trav_Parameters <= Max_Parameters loop
         Lua.Push (P_Lua_State, Lua.Lua_Integer (Trav_Parameters));

         Lua.Get_Table (P_Lua_State, P_Index);

         Lua.Get_Field (P_Lua_State, Stack_Top_On_Start + 1, "a");

         Lua_Integer_A := Lua.To_Ada (P_Lua_State, Stack_Top_On_Start + 2);
         Lua.Pop (P_Lua_State, 1);

         Lua.Get_Field (P_Lua_State, Stack_Top_On_Start + 1, "b");

         Lua_Integer_B := Lua.To_Ada (P_Lua_State, Stack_Top_On_Start + 2);

         Lua.Pop (P_Lua_State, 2);

         Area.all (Trav_Parameters) :=
           Hexagon.Type_Hexagon_Position'
             (True,
              Hexagon.Type_Hexagon_Numbers (Lua_Integer_A),
              Hexagon.Type_Hexagon_Numbers (Lua_Integer_B));
         Trav_Parameters := Trav_Parameters + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Area_Input - exit " &
            Lua.Get_Top (P_Lua_State)'Img);
         Print_Stack (P_Lua_State);
      end if;

      return Area;
   end Parameter_Hexagon_Area_Input;

   function Parameter_Boolean_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Boolean
   is
      A_Lua_Boolean : Boolean;
   begin
      A_Lua_Boolean := Lua.To_Ada (P_Lua_State, P_Index);

      return A_Lua_Boolean;
   end Parameter_Boolean_Input;

   function Parameter_Action_Type_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Action.Type_Action_Type
   is
      A_Lua_Integer : Lua.Lua_Integer;
      Action_Type   : Action.Type_Action_Type;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);
      Action_Type   := Action.Type_Action_Type (A_Lua_Integer);

      return Action_Type;
   end Parameter_Action_Type_Input;

   function Parameter_Player_Id_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Player.Type_Player_Id
   is
      A_Lua_Integer : Lua.Lua_Integer;
      Player_Id     : Player.Type_Player_Id;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);
      Player_Id     := Player.Type_Player_Id (A_Lua_Integer);

      return Player_Id;
   end Parameter_Player_Id_Input;

   function Parameter_Hexagon_Position_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Hexagon.Type_Hexagon_Position
   is
      A_Lua_Integer : Lua.Lua_Integer;
      Pos           : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);
      Pos.A         := Hexagon.Type_Hexagon_Numbers (A_Lua_Integer);
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index + 1);
      Pos.B         := Hexagon.Type_Hexagon_Numbers (A_Lua_Integer);

      return Pos;
   end Parameter_Hexagon_Position_Input;

   function Parameter_Positive_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Positive
   is
      A_Lua_Integer : Lua.Lua_Integer;
      A_Number      : Positive;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);
      A_Number      := Positive (A_Lua_Integer);

      return A_Number;
   end Parameter_Positive_Input;

   function Parameter_Piece_Id_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Piece.Type_Piece_Id
   is
      A_Lua_Integer : Lua.Lua_Integer;
      Piece_Id      : Piece.Type_Piece_Id;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);
      Piece_Id      := Piece.Type_Piece_Id (A_Lua_Integer);

      return Piece_Id;
   end Parameter_Piece_Id_Input;

   function Parameter_Construction_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Construction.Type_Construction
   is
      A_Lua_Integer  : Lua.Lua_Integer;
      A_Construction : Construction.Type_Construction;
   begin
      A_Lua_Integer  := Lua.To_Ada (P_Lua_State, P_Index);
      A_Construction := Construction.Type_Construction (A_Lua_Integer);

      return A_Construction;
   end Parameter_Construction_Input;

   function Parameter_Type_Of_Piece_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Piece.Type_Piece_Type
   is
      A_Lua_Integer : Lua.Lua_Integer;

      A_Type_Of_Piece : Piece.Type_Piece_Type;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);

      A_Type_Of_Piece := Piece.Type_Piece_Type (A_Lua_Integer);

      return A_Type_Of_Piece;
   end Parameter_Type_Of_Piece_Input;

   function Parameter_Category_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Piece.Type_Category
   is
      A_Lua_Integer : Lua.Lua_Integer;

      A_Category : Piece.Type_Category;

      use Interfaces.C;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);

      if A_Lua_Integer = Piece.Type_Category'Pos (Piece.Fighting_Piece) then
         A_Category := Piece.Fighting_Piece;
      elsif A_Lua_Integer = Piece.Type_Category'Pos (Piece.House_Piece) then
         A_Category := Piece.House_Piece;
      end if;

      return A_Category;
   end Parameter_Category_Input;

   function Parameter_String_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Utilities.RemoteString.Type_String
   is
      A_String : Utilities.RemoteString.Type_String;

      use Interfaces.C;
   begin
      A_String :=
        Utilities.RemoteString.To_Unbounded_String
          (Lua.To_Ada (P_Lua_State, P_Index));

      return A_String;
   end Parameter_String_Input;

   function Parameter_Piece_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Piece.Type_Piece
   is
      A_Piece : Piece.Type_Piece;
   begin
      A_Piece.Id            := Parameter_Piece_Id_Input (P_Lua_State, P_Index);
      A_Piece.Type_Of_Piece :=
        Parameter_Type_Of_Piece_Input (P_Lua_State, P_Index + 1);
      A_Piece.Category  := Parameter_Category_Input (P_Lua_State, P_Index + 2);
      A_Piece.Name      := Parameter_String_Input (P_Lua_State, P_Index + 3);
      A_Piece.Player_Id :=
        Parameter_Player_Id_Input (P_Lua_State, P_Index + 4);

      return A_Piece;
   end Parameter_Piece_Input;

   function Parameter_Effect_Name_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Effect.Type_Effect_Name
   is
      A_Lua_Integer : Lua.Lua_Integer;

      An_Effect_Name : Effect.Type_Effect_Name;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);

      An_Effect_Name := Effect.Type_Effect_Name (A_Lua_Integer);

      return An_Effect_Name;
   end Parameter_Effect_Name_Input;

   function Parameter_Effect_Aux_Input
     (P_Lua_State : in Lua.Lua_State;
      P_Index     :    Lua.Lua_Index) return Natural
   is
      A_Lua_Integer : Lua.Lua_Integer;

      An_Effect_Aux : Natural;
   begin
      A_Lua_Integer := Lua.To_Ada (P_Lua_State, P_Index);

      An_Effect_Aux := Natural (A_Lua_Integer);

      return An_Effect_Aux;
   end Parameter_Effect_Aux_Input;

   procedure Parameter_Hexagon_Delta_Position_Output
     (P_Lua_State      : in Lua.Lua_State;
      P_Delta_Position : in Hexagon.Area.Type_Action_Capabilities)
   is
      Stack_Top_On_Start           : Lua.Lua_Index;
      Lua_Integer_A, Lua_Integer_B : Lua.Lua_Integer;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Delta_Position_Output - enter top " &
            Lua.Get_Top (P_Lua_State)'Img);
         Print_Stack (P_Lua_State);
      end if;

      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Lua.Create_Table (P_Lua_State);
      for Trav_Parameters in P_Delta_Position'First .. P_Delta_Position'Last
      loop

         Lua.Push (P_Lua_State, Lua.Lua_Integer (Trav_Parameters));

         Lua.Create_Table (P_Lua_State);
         Lua_Integer_A :=
           Lua.Lua_Integer (P_Delta_Position (Trav_Parameters).A);
         Lua.Push (P_Lua_State, Lua_Integer_A);
         Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 3, "a");

         Lua_Integer_B :=
           Lua.Lua_Integer (P_Delta_Position (Trav_Parameters).B);
         Lua.Push (P_Lua_State, Lua_Integer_B);
         Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 3, "b");

         Lua.Set_Table (P_Lua_State, Stack_Top_On_Start + 1);

      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Delta_Position_Output - exit " &
            Lua.Get_Top (P_Lua_State)'Img);
         Print_Stack (P_Lua_State);
      end if;

   end Parameter_Hexagon_Delta_Position_Output;

   procedure Parameter_Hexagon_Position_Output
     (P_Lua_State : in Lua.Lua_State;
      P_Pos       : in Hexagon.Type_Hexagon_Position)
   is
      Stack_Top_On_Start           : Lua.Lua_Index;
      Lua_Integer_A, Lua_Integer_B : Lua.Lua_Integer;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Position_Output - enter ");
      end if;

      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Lua.Create_Table (P_Lua_State);

      Lua_Integer_A := Lua.Lua_Integer (P_Pos.A);
      Lua.Push (P_Lua_State, Lua_Integer_A);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "a");

      Lua_Integer_B := Lua.Lua_Integer (P_Pos.B);
      Lua.Push (P_Lua_State, Lua_Integer_B);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "b");

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Hexagon_Position_Output - exit ");
      end if;
   end Parameter_Hexagon_Position_Output;

   procedure Parameter_Piece_Output
     (P_Lua_State : in Lua.Lua_State;
      P_Piece     : in Piece.Type_Piece)
   is
      Stack_Top_On_Start : Lua.Lua_Index;
      Lua_Integer        : Lua.Lua_Integer;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Piece_Output - enter ");
      end if;

      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Lua.Create_Table (P_Lua_State);

      Lua_Integer := Lua.Lua_Integer (P_Piece.Id);
      Lua.Push (P_Lua_State, Lua_Integer);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "id");

      Lua_Integer := Lua.Lua_Integer (P_Piece.Type_Of_Piece);
      Lua.Push (P_Lua_State, Lua_Integer);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "type_of_piece");

      Lua_Integer :=
        Lua.Lua_Integer (Piece.Type_Category'Pos (P_Piece.Category));
      Lua.Push (P_Lua_State, Lua_Integer);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "category");

      Lua.Push (P_Lua_State, Utilities.RemoteString.To_String (P_Piece.Name));
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "name");

      Lua_Integer := Lua.Lua_Integer (P_Piece.Player_Id);
      Lua.Push (P_Lua_State, Lua_Integer);
      Lua.Set_Field (P_Lua_State, Stack_Top_On_Start + 1, "player_id");

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Piece_Output - exit ");
      end if;
   end Parameter_Piece_Output;

   procedure Parameter_String_Output
     (P_Lua_State : in Lua.Lua_State;
      P_String    : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_String_Output - enter ");
      end if;

      Lua.Push (P_Lua_State, Utilities.RemoteString.To_String (P_String));

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_String_Output - exit ");
      end if;
   end Parameter_String_Output;

   procedure Parameter_Landscape_Output
     (P_Lua_State : in Lua.Lua_State;
      P_Landscape : in Landscape.Type_Landscape)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Landscape_Output - enter ");
      end if;

      Lua.Push (P_Lua_State, Lua.Lua_Integer (P_Landscape));

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Landscape_Output - exit ");
      end if;
   end Parameter_Landscape_Output;

   procedure Parameter_Construction_List_Output
     (P_Lua_State         : in Lua.Lua_State;
      P_Construction_List : in Construction.Construction_List.Set)
   is
      Stack_Top_On_Start : Lua.Lua_Index;
      Lua_Integer        : Lua.Lua_Integer;
      Table_Index        : Positive;

      Trav_Constructions : Construction.Construction_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Construction_List_Output - enter ");
      end if;
      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Lua.Create_Table (P_Lua_State);
      Trav_Constructions :=
        Construction.Construction_List.First (P_Construction_List);
      Table_Index := 1;
      while Construction.Construction_List.Has_Element (Trav_Constructions)
      loop

         Lua.Push (P_Lua_State, Lua.Lua_Integer (Table_Index));
         Lua_Integer :=
           Lua.Lua_Integer
             (Construction.Construction_List.Element (Trav_Constructions));
         Lua.Push (P_Lua_State, Lua_Integer);

         Lua.Set_Table (P_Lua_State, Stack_Top_On_Start + 1);

         Table_Index        := Table_Index + 1;
         Trav_Constructions :=
           Construction.Construction_List.Next (Trav_Constructions);
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Construction_List_Output - exit ");
      end if;
   end Parameter_Construction_List_Output;

   procedure Parameter_Pieces_List_Output
     (P_Lua_State   : in Lua.Lua_State;
      P_Pieces_List : in Landscape.Pieces_Here_List.Vector)
   is
      Stack_Top_On_Start : Lua.Lua_Index;
      Lua_Integer        : Lua.Lua_Integer;
      Table_Index        : Positive;

      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Pieces_List_Output - enter ");
      end if;
      Stack_Top_On_Start := Lua.Get_Top (P_Lua_State);

      Lua.Create_Table (P_Lua_State);
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Pieces_List);
      Table_Index := 1;
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         Lua.Push (P_Lua_State, Lua.Lua_Integer (Table_Index));
         Lua_Integer :=
           Lua.Lua_Integer (Landscape.Pieces_Here_List.Element (Trav_Pieces));
         Lua.Push (P_Lua_State, Lua_Integer);

         Lua.Set_Table (P_Lua_State, Stack_Top_On_Start + 1);

         Table_Index := Table_Index + 1;
         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Parameter_Pieces_List_Output - exit ");
      end if;
   end Parameter_Pieces_List_Output;

   function Observe_Game_Minimum_Details
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Minimum_Details : Positive;
   begin
      Minimum_Details := Parameter_Positive_Input (P_Lua_State, 1);

      ServerAPI.Observe_Game_Minimum_Details (Minimum_Details);

      return 0;
   end Observe_Game_Minimum_Details;

   function Observe_Game (P_Lua_State : in Lua.Lua_State) return Integer is
      Detail : Positive;
   begin
      Detail := Parameter_Positive_Input (P_Lua_State, 1);

      ServerAPI.Observe_Game (Detail);

      return 0;
   end Observe_Game;

   function Create_Piece (P_Lua_State : in Lua.Lua_State) return Integer is

      Action_Type : Action.Type_Action_Type;

      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Pos                          : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      A_Piece  : Piece.Type_Piece;
      Force    : Boolean;

      Ret_Status : Status.Type_Status := Status.Ok;

   begin

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 2);

      A_Piece := Parameter_Piece_Input (P_Lua_State, 4);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 9);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 10);

      Force := Parameter_Boolean_Input (P_Lua_State, 11);

      ServerAPI.Create_Piece
        (Action_Type,
         Pos,
         A_Piece,
         Piece_Id,
         Current_Player_Id,
         Player_Id,
         Ret_Status,
         Force);

      Lua.Push (P_Lua_State, Lua.Lua_Integer (Piece_Id));
      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      return 2;
   end Create_Piece;

   function Put_Piece (P_Lua_State : in Lua.Lua_State) return Integer is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Pos                          : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Put_Piece - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);
      Pos         := Parameter_Hexagon_Position_Input (P_Lua_State, 2);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 4);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 5);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 6);

      ServerAPI.Put_Piece
        (Action_Type,
         Pos,
         Piece_Id,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Put_Piece - exit");
      end if;

      return 1;
   end Put_Piece;

   function Remove_Piece (P_Lua_State : in Lua.Lua_State) return Integer is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Pos                          : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Remove_Piece - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 2);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 4);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 5);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 6);

      ServerAPI.Remove_Piece
        (Action_Type,
         Pos,
         Piece_Id,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Remove_Piece - exit");
      end if;

      return 1;
   end Remove_Piece;

   function Perform_Attack_Pos
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Attacking_Pos,
      Attacked_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Attacking_Piece_Id,
      Attacked_Piece_Id : Piece.Type_Piece_Id :=
        Piece.Undefined_Piece_Id;

      Ret_Winner : Player.Type_Player_Id := Player.Type_Player_Id (0);
      Ret_Status : Status.Type_Status    := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Attack_Pos - enter");
      end if;
      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Attacking_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Attacked_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 3);

      Attacking_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 4);
      Attacked_Pos  := Parameter_Hexagon_Position_Input (P_Lua_State, 6);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Perform_Attack
        (Action_Type,
         Attacking_Piece_Id,
         Attacked_Piece_Id,
         Attacking_Pos,
         Attacked_Pos,
         Current_Player_Id,
         Player_Id,
         Ret_Winner,
         Ret_Status);

      Lua.Push (P_Lua_State, Lua.Lua_Integer (Ret_Winner));
      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Attack_Pos - exit");
      end if;
      return 2;
   end Perform_Attack_Pos;

   function Perform_Attack_Path
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type : Action.Type_Action_Type;

      Current_Player_Id, Player_Id : Player.Type_Player_Id;

      Path_Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
      Attacking_Piece_Id,
      Attacked_Piece_Id : Piece.Type_Piece_Id :=
        Piece.Undefined_Piece_Id;
      Attack_Path : Hexagon.Path.Vector;

      Ret_Winner : Player.Type_Player_Id := Player.Type_Player_Id (0);
      Ret_Status : Status.Type_Status    := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Attack_Path - enter");
      end if;
      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Attacking_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Attacked_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 3);

      Path_Tmp := Parameter_Hexagon_Area_Input (P_Lua_State, 4);
      for Trav_Path in Path_Tmp'First .. Path_Tmp'Last loop
         Hexagon.Path.Append
           (Attack_Path,
            Hexagon.Type_Hexagon_Position'
              (True, Path_Tmp (Trav_Path).A, Path_Tmp (Trav_Path).B));
      end loop;

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 5);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 6);

      ServerAPI.Perform_Attack
        (Action_Type,
         Attacking_Piece_Id,
         Attacked_Piece_Id,
         Attack_Path,
         Current_Player_Id,
         Player_Id,
         Ret_Winner,
         Ret_Status);

      Lua.Push (P_Lua_State, Lua.Lua_Integer (Ret_Winner));
      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Attack_Path - exit");
      end if;
      return 2;
   end Perform_Attack_Path;

   function Perform_Move_Pos (P_Lua_State : in Lua.Lua_State) return Integer is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      From_Pos,
      To_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Moving_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Move_Pos - enter");
      end if;

      Action_Type     := Parameter_Action_Type_Input (P_Lua_State, 1);
      Moving_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      From_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      To_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 5);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 7);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 8);

      ServerAPI.Perform_Move
        (Action_Type,
         Moving_Piece_Id,
         From_Pos,
         To_Pos,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Move_Pos - exit");
      end if;

      return 1;
   end Perform_Move_Pos;

   function Perform_Move_Path
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Path_Tmp : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;
      Moving_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      Move_Path                    : Hexagon.Path.Vector;
      Ret_Status                   : Status.Type_Status  := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Move_Path - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Moving_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Path_Tmp := Parameter_Hexagon_Area_Input (P_Lua_State, 3);
      for Trav_Path in Path_Tmp'First .. Path_Tmp'Last loop
         Hexagon.Path.Append
           (Move_Path,
            Hexagon.Type_Hexagon_Position'
              (True, Path_Tmp (Trav_Path).A, Path_Tmp (Trav_Path).B));
      end loop;

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 4);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 5);

      ServerAPI.Perform_Move
        (Action_Type,
         Moving_Piece_Id,
         Move_Path,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Move_Path - exit");
      end if;

      return 1;
   end Perform_Move_Path;

   function Perform_Ranged_Attack
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Attacking_Pos,
      Attacked_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Attacking_Piece_Id,
      Attacked_Piece_Id : Piece.Type_Piece_Id :=
        Piece.Undefined_Piece_Id;

      Ret_Winner : Player.Type_Player_Id := Player.Type_Player_Id (0);
      Ret_Status : Status.Type_Status    := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Perform_Ranged_Attack - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Attacking_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Attacked_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 3);

      Attacking_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 4);

      Attacked_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 6);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Perform_Ranged_Attack
        (Action_Type,
         Attacking_Piece_Id,
         Attacked_Piece_Id,
         Attacking_Pos,
         Attacked_Pos,
         Current_Player_Id,
         Player_Id,
         Ret_Winner,
         Ret_Status);

      Lua.Push (P_Lua_State, Lua.Lua_Integer (Ret_Winner));
      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Perform_Ranged_Attack - exit");
      end if;

      return 2;
   end Perform_Ranged_Attack;

   function Perform_Construction
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Construction_Pos,
      Piece_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Constructing_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      A_Construction        : Construction.Type_Construction;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Perform_Construction - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Constructing_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Piece_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      Construction_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 5);

      A_Construction := Parameter_Construction_Input (P_Lua_State, 7);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Perform_Construction
        (Action_Type,
         Constructing_Piece_Id,
         Piece_Pos,
         Construction_Pos,
         A_Construction,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Construction - exit");
      end if;

      return 1;
   end Perform_Construction;

   function Perform_Demolition
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Demolition_Pos,
      Piece_Pos : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Demolition_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      A_Construction      : Construction.Type_Construction;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Demolition - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Demolition_Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      Piece_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      Demolition_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 5);

      A_Construction := Parameter_Construction_Input (P_Lua_State, 7);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Perform_Demolition
        (Action_Type,
         Demolition_Piece_Id,
         Piece_Pos,
         Demolition_Pos,
         A_Construction,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Demolition - exit");
      end if;

      return 1;
   end Perform_Demolition;

   function Grant_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      An_Effect                    : Effect.Type_Effect;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Grant_Piece_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);
      Piece_Id    := Parameter_Piece_Id_Input (P_Lua_State, 2);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 3);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 4);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 5);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 6);

      ServerAPI.Grant_Piece_Effect
        (Action_Type,
         Piece_Id,
         An_Effect,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Grant_Piece_Effect - exit");
      end if;

      return 1;
   end Grant_Piece_Effect;

   function Revoke_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      An_Effect                    : Effect.Type_Effect;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Revoke_Piece_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 3);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 4);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 5);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 6);

      ServerAPI.Revoke_Piece_Effect
        (Action_Type,
         Piece_Id,
         An_Effect,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Revoke_Piece_Effect - exit");
      end if;

      return 1;
   end Revoke_Piece_Effect;

   function Grant_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type                  : Action.Type_Action_Type;
      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id           := Piece.Undefined_Piece_Id;
      A_Pos                        : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      An_Effect : Effect.Type_Effect;
      Area      : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Grant_Patch_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 5);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 6);

      Area := Parameter_Hexagon_Area_Input (P_Lua_State, 7);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Grant_Patch_Effect
        (Action_Type,
         Piece_Id,
         A_Pos,
         An_Effect,
         Area.all,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Grant_Patch_Effect - exit");
      end if;

      return 1;
   end Grant_Patch_Effect;

   function Revoke_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type : Action.Type_Action_Type;

      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id           := Piece.Undefined_Piece_Id;
      A_Pos                        : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      An_Effect : Effect.Type_Effect;
      Area      : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Revoke_Patch_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 5);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 6);

      Area := Parameter_Hexagon_Area_Input (P_Lua_State, 7);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Revoke_Patch_Effect
        (Action_Type,
         Piece_Id,
         A_Pos,
         An_Effect,
         Area.all,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Revoke_Patch_Effect - exit");
      end if;

      return 1;
   end Revoke_Patch_Effect;

   function Perform_Patch_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type : Action.Type_Action_Type;

      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id           := Piece.Undefined_Piece_Id;
      A_Pos                        : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      An_Effect : Effect.Type_Effect;
      An_Area   : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Perform_Patch_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 5);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 6);

      An_Area := Parameter_Hexagon_Area_Input (P_Lua_State, 7);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 8);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 9);

      ServerAPI.Perform_Patch_Effect
        (Action_Type,
         Piece_Id,
         A_Pos,
         An_Effect,
         An_Area.all,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Patch_Effect - exit");
      end if;

      return 1;
   end Perform_Patch_Effect;

   function Perform_Piece_Effect
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Action_Type : Action.Type_Action_Type;

      Current_Player_Id, Player_Id : Player.Type_Player_Id;
      Piece_Id : Piece.Type_Piece_Id           := Piece.Undefined_Piece_Id;
      A_Pos                        : Hexagon.Type_Hexagon_Position :=
        Hexagon.Type_Hexagon_Position'(True, 1, 1);
      An_Effect : Effect.Type_Effect;

      Ret_Status : Status.Type_Status := Status.Ok;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Perform_Piece_Effect - enter");
      end if;

      Action_Type := Parameter_Action_Type_Input (P_Lua_State, 1);

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 2);

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 3);

      An_Effect.Effect_Name := Parameter_Effect_Name_Input (P_Lua_State, 5);

      An_Effect.Aux := Parameter_Effect_Aux_Input (P_Lua_State, 6);

      Current_Player_Id := Parameter_Player_Id_Input (P_Lua_State, 7);
      Player_Id         := Parameter_Player_Id_Input (P_Lua_State, 8);

      ServerAPI.Perform_Piece_Effect
        (Action_Type,
         Piece_Id,
         A_Pos,
         An_Effect,
         Current_Player_Id,
         Player_Id,
         Ret_Status);

      Lua.Push
        (P_Lua_State,
         Lua.Lua_Integer (Status.Type_Status'Pos (Ret_Status)));

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Perform_Piece_Effect - exit");
      end if;

      return 1;
   end Perform_Piece_Effect;

   function End_Turn (P_Lua_State : in Lua.Lua_State) return Integer is
      Player_Id : Player.Type_Player_Id;
      Ret       : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.End_Turn - enter");
      end if;

      Player_Id := Parameter_Player_Id_Input (P_Lua_State, 1);
      Ret       := ServerAPI.End_Turn (Player_Id);

      Lua.Push (P_Lua_State, Ret);

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.End_Turn - exit");
      end if;
      return 1;
   end End_Turn;

   function Observation_Area (P_Lua_State : in Lua.Lua_State) return Integer is
      Piece_Id : Piece.Type_Piece_Id;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Observation_Area - enter");
      end if;

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 1);

      declare
         Area : Hexagon.Area.Type_Action_Capabilities :=
           ServerAPI.Observation_Area (Piece_Id);
      begin
         Parameter_Hexagon_Delta_Position_Output (P_Lua_State, Area);
      end;

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Observation_Area - exit");
      end if;

      return 1;
   end Observation_Area;

   function Movement_Capability
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Piece_Id : Piece.Type_Piece_Id;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Movement_Capability - enter");
      end if;

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 1);

      declare
         Area : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access :=
           ServerAPI.Movement_Capability (Piece_Id);
      begin
         Parameter_Hexagon_Delta_Position_Output (P_Lua_State, Area.all);

         Hexagon.Area.Server_Area.Free_Action_Capabilities (Area);
      end;

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Movement_Capability - exit");
      end if;
      return 1;
   end Movement_Capability;

   function Attack_Capability
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Piece_Id : Piece.Type_Piece_Id;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Attack_Capability - enter");
      end if;

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 1);

      declare
         Area : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access :=
           ServerAPI.Attack_Capability (Piece_Id);
      begin
         Parameter_Hexagon_Delta_Position_Output (P_Lua_State, Area.all);

         Hexagon.Area.Server_Area.Free_Action_Capabilities (Area);
      end;

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Attack_Capability - exit");
      end if;
      return 1;
   end Attack_Capability;

   function Find_Piece_In_List
     (P_Lua_State : in Lua.Lua_State) return Integer
   is
      Piece_Id       : Piece.Type_Piece_Id;
      Piece_Position : ServerAPI.Type_Piece_Position;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Find_Piece_In_List - enter");
      end if;

      Piece_Id := Parameter_Piece_Id_Input (P_Lua_State, 1);

      Piece_Position := ServerAPI.Find_Piece_In_List (Piece_Id);

      Parameter_Piece_Output (P_Lua_State, Piece_Position.Actual_Piece);

      Parameter_Hexagon_Position_Output
        (P_Lua_State,
         Piece_Position.Actual_Pos);

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Find_Piece_In_List - exit");
      end if;
      return 2;
   end Find_Piece_In_List;

   function Opponents_Activity_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer
   is
      Detail               : Positive;
      Player_Id            : Player.Type_Player_Id;
      Activity_Description : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Opponents_Activity_Report_Append - enter");
      end if;

      Detail               := Parameter_Positive_Input (P_Lua_State, 1);
      Player_Id            := Parameter_Player_Id_Input (P_Lua_State, 2);
      Activity_Description := Parameter_String_Input (P_Lua_State, 3);

      Server.ServerAPI.Opponents_Activity_Report_Append
        (Detail,
         Player_Id,
         Activity_Description);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Opponents_Activity_Report_Append - exit");
      end if;
      return 1;
   end Opponents_Activity_Report_Append;

   function Player_Activity_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer
   is
      Detail               : Positive;
      Player_Id            : Player.Type_Player_Id;
      Activity_Description : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Player_Activity_Report_Append - enter");
      end if;

      Detail               := Parameter_Positive_Input (P_Lua_State, 1);
      Player_Id            := Parameter_Player_Id_Input (P_Lua_State, 2);
      Activity_Description := Parameter_String_Input (P_Lua_State, 3);

      Server.ServerAPI.Player_Activity_Report_Append
        (Detail,
         Player_Id,
         Activity_Description);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Player_Activity_Report_Append - exit");
      end if;
      return 1;
   end Player_Activity_Report_Append;

   function Opponents_System_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer
   is
      Detail               : Positive;
      Player_Id            : Player.Type_Player_Id;
      Activity_Description : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Opponents_System_Report_Append - enter");
      end if;

      Detail               := Parameter_Positive_Input (P_Lua_State, 1);
      Player_Id            := Parameter_Player_Id_Input (P_Lua_State, 2);
      Activity_Description := Parameter_String_Input (P_Lua_State, 3);

      Server.ServerAPI.Opponents_System_Report_Append
        (Detail,
         Player_Id,
         Activity_Description);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Opponents_System_Report_Append - exit");
      end if;
      return 1;
   end Opponents_System_Report_Append;

   function Player_System_Report_Append
     (P_Lua_State : Lua.Lua_State) return Integer
   is
      Detail               : Positive;
      Player_Id            : Player.Type_Player_Id;
      Activity_Description : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Player_System_Report_Append - exit");
      end if;

      Detail               := Parameter_Positive_Input (P_Lua_State, 1);
      Player_Id            := Parameter_Player_Id_Input (P_Lua_State, 2);
      Activity_Description := Parameter_String_Input (P_Lua_State, 3);

      Server.ServerAPI.Player_System_Report_Append
        (Detail,
         Player_Id,
         Activity_Description);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Player_System_Report_Append - exit");
      end if;
      return 1;
   end Player_System_Report_Append;

   function Get_Player_Name (P_Lua_State : Lua.Lua_State) return Integer is
      Player_Id   : Player.Type_Player_Id;
      Player_Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Player_Name - enter");
      end if;

      Player_Id := Parameter_Player_Id_Input (P_Lua_State, 1);

      Player_Name := Server.ServerAPI.Get_Player_Name (Player_Id);

      Parameter_String_Output (P_Lua_State, Player_Name);

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Player_Name - exit");
      end if;
      return 1;
   end Get_Player_Name;

   function Get_Map_Terrain (P_Lua_State : Lua.Lua_State) return Integer is
      A_Pos : Hexagon.Type_Hexagon_Position;

      A_Landscape : Landscape.Type_Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Map_Terrain - enter");
      end if;

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 1);

      A_Landscape := ServerAPI.Get_Map_Terrain (A_Pos);

      Parameter_Landscape_Output (P_Lua_State, A_Landscape);

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Map_Terrain - enter");
      end if;
      return 1;

   end Get_Map_Terrain;

   function Get_Map_Construction_List
     (P_Lua_State : Lua.Lua_State) return Integer
   is
      A_Pos : Hexagon.Type_Hexagon_Position;

      A_Construction_List : Construction.Construction_List.Set;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Get_Map_Construction_List - enter");
      end if;

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 1);

      A_Construction_List := ServerAPI.Get_Map_Construction_List (A_Pos);

      Parameter_Construction_List_Output (P_Lua_State, A_Construction_List);

      if Verbose then
         Text_IO.Put_Line
           ("Server.Lua_Interface.Get_Map_Construction_List - enter");
      end if;
      return 1;
   end Get_Map_Construction_List;

   function Get_Map_Pieces_List (P_Lua_State : Lua.Lua_State) return Integer is
      A_Pos : Hexagon.Type_Hexagon_Position;

      A_Pieces_List : Landscape.Pieces_Here_List.Vector;
   begin
      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Map_Pieces_List - enter");
      end if;

      A_Pos := Parameter_Hexagon_Position_Input (P_Lua_State, 1);

      A_Pieces_List := ServerAPI.Get_Map_Pieces_List (A_Pos);

      Parameter_Pieces_List_Output (P_Lua_State, A_Pieces_List);

      if Verbose then
         Text_IO.Put_Line ("Server.Lua_Interface.Get_Map_Pieces_List - enter");
      end if;
      return 1;
   end Get_Map_Pieces_List;

end Server.Lua_Interface;
