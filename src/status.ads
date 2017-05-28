--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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
   --Perform_Move
      Not_Players_Piece,
      Patch_Bad_Terrain,
      Patch_Occupied,
      Target_Patch_Occupied,
      Not_Before_Perform_Move,
      No_Path_Found,
   --
      Completed_Ok,

   -- Create_Piece
      Not_Before_Create_Piece,

   -- Put_Piece
      Not_Before_Put_Piece,

   -- Remove_Piece
      Not_Before_Remove_Piece,

   --Perform_Attack
      Not_Before_Perform_Attack,

   --Perform_Ranged_Attack
      Not_Before_Perform_Ranged_Attack,

   -- Perform_Patch_Effect
      Not_Before_Perform_Patch_Effect,

   --Perform Piece Effect
      Not_Before_Perform_Piece_Effect,

   --Perform_Construction
      Not_Before_Perform_Construction,

   --Perform_Demolition
      Not_Before_Perform_Demolition,

   --Grant_Piece_Effect
      Not_Before_Grant_Piece_Effect,

   -- Revoke_Piece_Effect
      Not_Before_Revoke_Piece_Effect,

   -- Grant_Patch_Effect
      Not_Before_Grant_Patch_Effect,

   -- Revoke_Patch_Effect
      Not_Before_Revoke_Patch_Effect,

      Patch_Effect_Not_Here,

      Piece_Effect_Not_Here,

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
      Last_Report,
      Stopping,
      Client_Stopped,
      Stopped);

   type Type_Game_Status is (Playing, End_Of_Game);

   type Type_Result_Status is (Proceed, Retry, Fail);

end Status;
