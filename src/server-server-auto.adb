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

with Server.Server;

package body Server.Server.Auto is

   procedure Check_Opponent_Communications
     (P_Player_List_Internal : in out Server.Type_Player_List)
   is
      Now_Time : Ada.Real_Time.Time;

      use Ada.Real_Time;
      use Utilities.RemoteString;
   begin
      Now_Time := Ada.Real_Time.Clock;

      for Trav_Player in P_Player_List_Internal'First .. P_Player_List_Internal'Last loop
         if P_Player_List_Internal (Trav_Player).Active then
            if Now_Time >
              P_Player_List_Internal (Trav_Player).Last_Update_Summary +
                Ada.Real_Time.Milliseconds (Time_Interval * 4) and
              Now_Time >
                P_Player_List_Internal (Trav_Player).Last_System_Warning +
              Ada.Real_Time.Milliseconds (60000)
            then
               Opponents_System_Report_Append
                 (Observation.Activity.Internal_Details,
                  Trav_Player,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Opponent '" & Utilities.RemoteString.To_String(Get_Player_Name(Trav_Player)) & "' seems to get late updates"));
               P_Player_List_Internal (Trav_Player).Last_System_Warning := Ada.Real_Time.Clock;

            end if;
         end if;
      end loop;

   end Check_Opponent_Communications;

   procedure Update_Game
     (P_Last_Update_Time     : in out Ada.Real_Time.Time;
      P_Countdown            : in out Positive;
      P_Player_List_Internal : in out Server.Type_Player_List)
   is
      Now_Time : Ada.Real_Time.Time;

      use Ada.Real_Time;
   begin
      Now_Time := Ada.Real_Time.Clock;

      if Now_Time > P_Last_Update_Time + Ada.Real_Time.Milliseconds (Time_Interval) then
         -- should be done for all players.
         -- idea: penalty for the player that use long time to do his moves,
         -- and reward to te player being fast...
         --
         for Trav_Player in P_Player_List_Internal'First .. P_Player_List_Internal'Last loop
            if P_Player_List_Internal (Trav_Player).Active then
               Piece.Server.Upkeep_All (Trav_Player);

               Observe_Game (1);
            end if;
         end loop;
         P_Countdown        := ((P_Countdown + 1) mod 359) + 1;
         P_Last_Update_Time := Now_Time;

      end if;
   end Update_Game;

end Server.Server.Auto;
