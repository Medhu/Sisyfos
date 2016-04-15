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

package Status is
   pragma Pure;

   type Type_Status is
     (Ok,
      Inconsistent_Parameters,
      Not_Players_Turn,
      Not_Players_Piece,
      No_Executing_Piece,
      Players_Attacks_Himself,
      No_Target_Piece,
      Target_Patch_Occupied,
      Not_Reachable,
      Patch_Effect_Not_Here,
      Piece_Effect_Not_Here,
      No_Movement_Capability,
      No_Attack_Capability,
      No_Path_Found,
      Patch_Occupied,
      Patch_Empty,
      Patch_Bad_Terrain,
      Expected_Fighting_Piece,
      Expected_House_Piece,
      Piece_Cant_Be_Placed,
      Not_Allowed_To_Create_Piece,
      Will_Try,
      Not_Possible,
      Out_Of_Moves,
      Patches_Not_Connected,
      Construction_Exists,
      Construction_Doesnt_Exist);

   type Type_Adm_Status is
     (Adm_Ok,
      Cant_Create_Now,
      Cant_Join_Now,
      Already_Joined,
      Cant_Save_Now,
      Cant_Load_Now,
      Not_Ready,
      Not_Waiting,
      Not_Playing);

   type Type_Engine_State is
     (Starting,
      Creating_Game,
      Joining_Game,
      Leaving_Game,
      Saving_Game,
      Loading_Game,
      Ongoing,
      Switching_Turn,
      Last_Report,
      Stopping,
      Client_Stopped,
      Stopped);

   type Type_Game_Status is (Playing, End_Of_Game);

end Status;
