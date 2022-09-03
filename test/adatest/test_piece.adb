--
--
--      Sisyfos Client/Server logic. This is test logic to test both server and client of Sisyfos.
--      Copyright (C) 2013-2021  Frank J Jorgensen
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

with Piece;
with Piece.Server;
with Piece.Server.House_Piece;
with Piece.Server.Fighting_Piece;
with Text_IO;
with Hexagon;
with Hexagon.Area;
with Hexagon.Area.Server_Area;
with Ada.Numerics.Discrete_Random;
with Server;
with Server.ServerAPI;
with Status;
with Effect.Server;
with Server.ServerAPI;
with Attempt;

package body Test_Piece is
   package Random is new Ada.Numerics.Discrete_Random (Positive);
   RandomGen : Random.Generator;

   Verbose : constant Boolean := False;

   Current_Scenario : Utilities.RemoteString.Type_String;

   procedure Wait_For_Server (P_Action_Type : in Action.Type_Action_Type) is
   begin
      while not Test_List (Integer (P_Action_Type)).Done loop
         Text_IO.Put_Line ("Test number:" & P_Action_Type'Img & " not finished");
      end loop;
   end Wait_For_Server;

   --
   -- Create Piece
   --
   function Validate_Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_Piece) return Boolean
   is
      use Player;
      use Hexagon;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Create_Piece (Piece)- enter - exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (1,
         P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("You entered a Create Piece command (Piece)"));

      return True;
   end Validate_Create_Piece;

   function Validate_Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_House) return Boolean
   is
      use Player;
      use Hexagon;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Create_Piece (House)- enter - exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (1,
         P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("You entered a Create Piece command (House)"));

      return True;
   end Validate_Create_Piece;

   procedure Before_Create_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Pos          : in     Hexagon.Type_Hexagon_Position;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Before_Create_Piece (Piece)- enter - exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      if P_Action_Type = 1027 then
         P_Result := Status.Fail;
      else
         P_Result := Status.Proceed;
      end if;

   end Before_Create_Piece;

   procedure Before_Create_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Pos          : in     Hexagon.Type_Hexagon_Position;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Before_Create_Piece (House)- enter - exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      P_Result := Status.Proceed;
   end Before_Create_Piece;

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
      use Status;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.End_Create_Piece (Piece)- enter - exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Create_Piece;

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.End_Create_Piece (House)- enter " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.End_Create_Piece (House)- exit " &
            P_Piece.Type_Of_Piece'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;
   end End_Create_Piece;

   function Validate_Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_Piece) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Validate_Put_Piece - enter - exit " & P_Piece.Type_Of_Piece'Img);
      end if;

      return True;
   end Validate_Put_Piece;

   function Validate_Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Test_Piece.Type_My_Test_House) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Put_Piece - enter - exit");
      end if;

      return True;
   end Validate_Put_Piece;

   procedure Before_Put_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Pos          : in     Hexagon.Type_Hexagon_Position;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is

      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Put_Piece (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Put_Piece;

   procedure Before_Put_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Pos          : in     Hexagon.Type_Hexagon_Position;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Put_Piece (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Put_Piece;

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Put_Piece (Piece) - enter - exit");
      end if;
   end End_Put_Piece;

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Put_Piece (House) - enter - exit");
      end if;

   end End_Put_Piece;

   function Validate_Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Remove_Piece - enter - exit");
      end if;

      return True;
   end Validate_Remove_Piece;

   function Validate_Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Remove_Piece - enter - exit");
      end if;

      return True;
   end Validate_Remove_Piece;

   procedure Before_Remove_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Remove_Piece (Piece) - enter - exit");
      end if;

      if P_Action_Type = 1029 then
         P_Result := Status.Fail;
      else
         P_Result := Status.Proceed;
      end if;
   end Before_Remove_Piece;

   procedure Before_Remove_Piece
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Remove_Piece (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Remove_Piece;

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Remove_Piece (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Remove_Piece;

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Remove_Piece (House) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Remove_Piece;

   procedure Perform_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Cursor_Effect : Effect.Effect_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Perform_Patch_Effect (Piece) - enter - exit");
      end if;

      A_Patch       := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Area (1).A, P_Area (1).B);
      Cursor_Effect := Effect.Effect_List.Find (A_Patch.all.Effects_Here, P_Effect_Name);

      if Effect.Effect_List.Has_Element (Cursor_Effect) then
         Effect.Effect_List.Delete (A_Patch.all.Effects_Here, Cursor_Effect);
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Perform_Patch_Effect (House) - enter - exit");
      end if;
   end Perform_Patch_Effect;

   function Validate_Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Patch_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Patch_Effect;

   function Validate_Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Patch_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Patch_Effect;

   procedure Before_Perform_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Patch_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Patch_Effect;

   procedure Before_Perform_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Patch_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Patch_Effect;

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Patch_Effect (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Perform_Patch_Effect;

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Patch_Effect (House) - enter - exit");
      end if;
   end End_Perform_Patch_Effect;

   function Validate_Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Piece_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Piece_Effect;

   function Validate_Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Piece_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Piece_Effect;

   procedure Before_Perform_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Piece_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Piece_Effect;

   procedure Before_Perform_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Piece_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Piece_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      Cursor_Effect : Effect.Effect_List.Cursor;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Perform_Piece_Effect (Piece) - enter - exit");
      end if;

      Cursor_Effect := Effect.Effect_List.Find (P_Piece.Effects_On_Piece, P_Effect_Name);

      if Effect.Effect_List.Has_Element (Cursor_Effect) then
         Effect.Effect_List.Delete (P_Piece.Effects_On_Piece, Cursor_Effect);
      end if;
   end Perform_Piece_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Perform_Piece_Effect (House) - enter - exit");
      end if;
   end Perform_Piece_Effect;

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Piece_Effect (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Perform_Piece_Effect;

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Piece_Effect (House) - enter - exit");
      end if;
   end End_Perform_Piece_Effect;

   function Validate_Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect      : in Effect.Type_Effect) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Grant_Piece_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Grant_Piece_Effect;

   function Validate_Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect      : in Effect.Type_Effect) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Grant_Piece_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Grant_Piece_Effect;

   procedure Before_Grant_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Effect       : in     Effect.Type_Effect;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Grant_Piece_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Piece_Effect;

   procedure Before_Grant_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Effect       : in     Effect.Type_Effect;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Grant_Piece_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Piece_Effect;

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Grant_Piece_Effect (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Grant_Piece_Effect;

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Grant_Piece_Effect (House) - enter - exit");
      end if;

   end End_Grant_Piece_Effect;

   function Validate_Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Revoke_Piece_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Piece_Effect;

   function Validate_Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Revoke_Piece_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Piece_Effect;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Revoke_Piece_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Piece_Effect;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Revoke_Piece_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Piece_Effect;

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Revoke_Piece_Effect (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Revoke_Piece_Effect;

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Revoke_Piece_Effect (House) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Revoke_Piece_Effect;

   function Validate_Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Grant_Patch_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Grant_Patch_Effect;

   function Validate_Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Grant_Patch_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Grant_Patch_Effect;

   procedure Before_Grant_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect       : in     Effect.Type_Effect;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Grant_Patch_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Patch_Effect;

   procedure Before_Grant_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect       : in     Effect.Type_Effect;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Grant_Patch_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Patch_Effect;

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Grant_Patch_Effect (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Grant_Patch_Effect;

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Grant_Patch_Effect (House) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Grant_Patch_Effect;

   function Validate_Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Revoke_Patch_Effect (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Patch_Effect;

   function Validate_Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Revoke_Patch_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Patch_Effect;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_Piece;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Revoke_Patch_Effect (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Patch_Effect;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Piece        : in out Test_Piece.Type_My_Test_House;
      P_Area         : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name  : in     Effect.Type_Effect_Name;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Revoke_Patch_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Patch_Effect;

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Revoke_Patch_Effect (Piece) - enter - exit");
      end if;
      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Revoke_Patch_Effect;

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Test_Piece.Type_My_Test_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Revoke_Patch_Effect (House) - enter - exit");
      end if;
      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Revoke_Patch_Effect;

   --
   -- Perform Attack
   --
   function Validate_Perform_Attack
     (P_Player_Id                         : in Player.Type_Player_Id;
      P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Test_Piece.Type_My_Test_Piece) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Attack (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Attack;

   procedure Before_Perform_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Result                            :    out Status.Type_Result_Status;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Attack (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Attack;

   procedure Calculate_Attack_Result
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Winner                            :    out Player.Type_Player_Id;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Attack_Result (Piece) - enter - exit");
      end if;

      if P_Attacking_Piece.Id = 1 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 2 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 3 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 4 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 5 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 6 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 7 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 8 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 9 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 10 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 11 and P_Attacked_Piece.Id = 12 then
         P_Winner := Player.Undefined_Player_Id;
      elsif P_Attacking_Piece.Id = 12 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 13 then
         P_Winner := P_Attacking_Piece.Player_Id;

      elsif P_Attacking_Piece.Id = 24 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 39 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 40 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 41 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 42 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 44 and P_Attacked_Piece.Id = 45 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 44 and P_Attacked_Piece.Id = 46 then
         P_Winner := P_Attacked_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 47 and P_Attacked_Piece.Id = 46 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 56 and P_Attacked_Piece.Id = 57 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 58 and P_Attacked_Piece.Id = 59 then
         P_Winner := Player.Undefined_Player_Id;
      elsif P_Attacking_Piece.Id = 60 and P_Attacked_Piece.Id = 61 then
         P_Winner := P_Attacking_Piece.Player_Id;
      else
         Text_IO.Put_Line ("Battle not defined P_Attacking_Piece.Id=" & P_Attacking_Piece.Id'Img);
      end if;

   end Calculate_Attack_Result;

   procedure End_Perform_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Winner                            : in     Player.Type_Player_Id;
      P_End_Status                        : in     Status.Type_Status;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.End_Perform_Attack (Piece) - enter - exit P_End_Status=" &
            P_End_Status'Img);
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Perform_Attack;

   --
   -- Perform Ranged Attack
   --
   function Validate_Perform_Ranged_Attack
     (P_Player_Id                         : in Player.Type_Player_Id;
      P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Test_Piece.Type_My_Test_Piece) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Ranged_Attack (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Ranged_Attack;

   procedure Before_Perform_Ranged_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Result                            :    out Status.Type_Result_Status;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Ranged_Attack (Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Ranged_Attack;

   procedure Calculate_Ranged_Attack_Result
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Winner                            :    out Player.Type_Player_Id;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Calculate_Ranged_Attack_Result (Piece) - enter - exit");
      end if;

      if P_Attacking_Piece.Id = 27 and P_Attacked_Piece.Id = 28 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Id = 29 and P_Attacked_Piece.Id = 30 then
         P_Winner := Player.Undefined_Player_Id;
      end if;

   end Calculate_Ranged_Attack_Result;

   procedure End_Perform_Ranged_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Winner                            : in     Player.Type_Player_Id;
      P_End_Status                        : in     Status.Type_Status;
      P_Attempt_Info                      : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Ranged_Attack (Piece) - enter - exit");
      end if;

      Test_List.all (Integer (P_Action_Type)).Done   := True;
      Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

      Attempt.Set_Done_Attempt(P_Attempt_Info);
   end End_Perform_Ranged_Attack;

   --
   -- Perform_Move
   --
   function Validate_Perform_Move
     (P_Player_Id    : in Player.Type_Player_Id;
      P_Action_Type  : in Action.Type_Action_Type;
      P_Moving_Piece : in Test_Piece.Type_My_Test_Piece;
      P_To_Pos       : in Hexagon.Type_Hexagon_Position) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Validate_Perform_Move (Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Move;

   procedure Before_Perform_Move
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Move (Piece) - enter - exit");
      end if;

      if P_Action_Type = 1031 then
         P_Result := Status.Fail;
      else
         P_Result := Status.Proceed;
      end if;
   end Before_Perform_Move;

   procedure Before_Perform_Move_Step
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Before_Perform_Move_Step (Piece) - enter - exit");
      end if;

      if P_Action_Type = 1031 then
         P_Result := Status.Fail;
      else
         P_Result := Status.Proceed;
      end if;
   end Before_Perform_Move_Step;


   procedure End_Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece       : in out Test_Piece.Type_My_Test_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_End_Pos            : in     Hexagon.Type_Hexagon_Position;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info)
   is
      use Status;
      use Action;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.End_Perform_Move (Piece) - enter - exit");
      end if;

      if P_End_Status = Status.Completed_Ok then
         Test_List.all (Integer (P_Action_Type)).Done   := True;
         Test_List.all (Integer (P_Action_Type)).Result := Status.Ok;

         Attempt.Set_Done_Attempt(P_Attempt_Info);
      elsif P_End_Status = Status.No_Path_Found then
         Test_List.all (Integer (P_Action_Type)).Done   := True;
         Test_List.all (Integer (P_Action_Type)).Result := Status.No_Path_Found;

         Attempt.Set_Done_Attempt(P_Attempt_Info);
      elsif P_End_Status = Status.Not_Before_Perform_Move and P_Action_Type = 1031 then
         Test_List.all (Integer (P_Action_Type)).Done   := True;
         Test_List.all (Integer (P_Action_Type)).Result := P_End_Status;

         Attempt.Set_Done_Attempt(P_Attempt_Info);
      else
--         P_Attempt_Info := P_Attempt_Info - 1;
         Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt(P_Attempt_Info) + 1);
      end if;

   end End_Perform_Move;

   function Observation_Area
     (P_Piece : in Type_My_Test_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - enter " & P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Sentry_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
         --
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Bowman_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
         --
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2));

      elsif P_Piece.Type_Of_Piece = Test_Piece.Knight_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Ship_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   procedure After_Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Winner                            : in     Player.Type_Player_Id;
      P_Player_Id                         : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Ranged_Attack;

   procedure After_Perform_Move
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id)
   is
   begin
      null;
   end After_Perform_Move;

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end After_Put_Piece;

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Test_Piece.Type_My_Test_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id)
   is
   begin
      null;
   end After_Put_Piece;

   function Movement_Cost
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Test_Piece.Type_My_Test_Piece;
      P_From_Patch  : in out Landscape.Type_Patch;
      P_To_Patch    : in out Landscape.Type_Patch) return Integer
   is
   begin
      return 1;
   end Movement_Cost;

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece : in out Type_My_Test_Piece)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - Piece - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - Piece - exit");
      end if;
   end Upkeep;

   function Observation_Area
     (P_Piece : in Type_My_Test_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - enter " & P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Test_Piece.Farm_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Test_Piece.Lumberjack_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Test_Piece.Tower_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 1),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -3),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 3));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House : in out Type_My_Test_House)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - House - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Upkeep - House - exit");
      end if;

   end Upkeep;

   procedure Test_Joining_Game is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Joining_Game -enter - exit");
      end if;

   end Test_Joining_Game;

   procedure Test_Leaving_Game is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Leaving_Game -enter - exit");
      end if;

   end Test_Leaving_Game;

   procedure Test_Creating_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Creating_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Creating_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Creating_Game;

   procedure Test_Saving_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Saving_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Saving_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Saving_Game;

   procedure Test_Loading_Game
     (P_File_Name     : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Loading_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Test_Piece.Test_Loading_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Test_Loading_Game;

   procedure Test_Start_Game is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;

      A_Pos1     : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 4, 4);
      A_Pos2     : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 8, 9);
      A_To_Pos   : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 5, 5);
      A_From_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 6, 5);
      A_Patch    : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Game - enter");
      end if;
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (5, 6);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (4, 6);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (44, 50);
      Effect.Effect_List.Include
        (A_Patch.all.Effects_Here,
         Test_Piece.Effect_Treasure,
         Effect.Type_Effect'(Test_Piece.Effect_Treasure, 0));

      A_Piece.Type_Of_Piece := Test_Piece.Tower_House;
      A_Piece.Category      := Piece.House_Piece;
      A_Piece.Player_Id     := 1;

      Server.ServerAPI.Create_Piece
        (Player.Type_Player_Id (1),
         Action.Type_Action_Type (1),
         A_Pos1,
         A_Piece,
         A_Piece.Id,
         Ret_Status);

      A_Piece.Type_Of_Piece := Test_Piece.Tower_House;
      A_Piece.Category      := Piece.House_Piece;
      A_Piece.Player_Id     := 2;

      Server.ServerAPI.Create_Piece
        (Player.Type_Player_Id (2),
         Action.Type_Action_Type (1),
         A_Pos2,
         A_Piece,
         A_Piece.Id,
         Ret_Status);

      A_Piece.Type_Of_Piece := Test_Piece.Sentry_Piece;
      A_Piece.Category      := Piece.Fighting_Piece;
      A_Piece.Player_Id     := 2;

      Server.ServerAPI.Create_Piece
        (Player.Type_Player_Id (2),
         Action.Type_Action_Type (1),
         A_From_Pos,
         A_Piece,
         A_Piece.Id,
         Ret_Status);

      Server.ServerAPI.Observe_Game (5);

      Server.ServerAPI.Perform_Move
        (Player.Type_Player_Id (2),
         Action.Type_Action_Type (1),
         A_Piece.Id,
         A_To_Pos,
         Ret_Status);

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Start_Game - exit");
      end if;
   end Test_Start_Game;

   procedure Test_Upkeep_Game is
      Trav_All_Pieces                       : Piece.Server.Pieces_Server_List.Cursor;
      Trav_Pieces                           : Landscape.Pieces_Here_List.Cursor;
      A_Piece_To_Visit, A_Piece_Encountered : Piece.Server.Type_Piece_Access              := null;
      A_Patch                               : Hexagon.Server_Map.Type_Server_Patch_Adress := null;
      A_Pos                                 : Hexagon.Type_Hexagon_Position;
      Axis_Patch                            : Hexagon.Server_Map.Type_Server_Patch_Adress := null;

      use Piece;
      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Upkeep_Game - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_Upkeep_Game - exit");
      end if;

   end Test_Upkeep_Game;

   procedure Test_End_Game (P_Game_Status : out Status.Type_Game_Status) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Game - enter");
      end if;
      P_Game_Status := Status.Playing;
      if Verbose then
         Text_IO.Put_Line ("Test_Piece.Test_End_Game - exit");
      end if;
   end Test_End_Game;

begin
   Random.Reset (RandomGen, 1);
end Test_Piece;
