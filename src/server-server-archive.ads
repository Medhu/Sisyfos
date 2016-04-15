--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015  Frank J Jorgensen
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

with Utilities;

package Server.Server.Archive is

     procedure Creating_Game
       (P_Directory            : in     Utilities.RemoteString.Type_String;
        P_Create_File_Name     : in     Utilities.RemoteString.Type_String;
        P_Scenario_Name        :    out Utilities.RemoteString.Type_String;
        P_Player_List          :    out Server.Type_Player_List;
        P_Countdown            :    out Positive;
        P_Current_Player_Id    :    out Player.Type_Player_Id);

   procedure Saving_Game
     (P_Directory            : in     Utilities.RemoteString.Type_String;
      P_Save_File_Name, P_Scenario_Name : in Utilities.RemoteString.Type_String;
      P_Player_List              : in Server.Type_Player_List;
      P_Countdown                       : in Positive;
      P_Current_Player_Id               : in Player.Type_Player_Id);

   procedure Loading_Game
     (P_Directory          : in     Utilities.RemoteString.Type_String;
      P_Load_File_Name     : in     Utilities.RemoteString.Type_String;
      P_Scenario_Name      :    out Utilities.RemoteString.Type_String;
      P_Player_List        :    out Server.Type_Player_List;
      P_Countdown          :    out Positive;
      P_Current_Player_Id  :    out Player.Type_Player_Id);

end Server.Server.Archive;
