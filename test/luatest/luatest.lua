
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

print("Concept test of Sisyfos - Lua interface")

print("TEST / DEMONSTRATION Global constants")
print("Sisyfos.Status.OK=" .. Sisyfos.Status.OK)
print("Sisyfos.Status.PATCH_BAD_TERRAIN=" .. Sisyfos.Status.PATCH_BAD_TERRAIN)
print("Sisyfos.Category.FIGHTING_PIECE=" .. Sisyfos.Category.FIGHTING_PIECE)
print("Sisyfos.Category.HOUSE_PIECE=" .. Sisyfos.Category.HOUSE_PIECE)

print("")
print("")

print("TEST Ada Procedures and Functions")

local ret_status = 0

piece_id, ret_status = Sisyfos.Create_Piece(2,
                          900,
                          53,54, -- position
                          1005,1006,Sisyfos.Category.FIGHTING_PIECE,"TestName",2,
                          true)
print("piece_id=" .. piece_id .. " ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Put_Piece(2,
                      901,
                       33,44, -- position
                       55)  -- piece_id
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Remove_Piece(2,
                          902,
                          34,45,
                          56)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Attack(11, 
                                    903,
                                    81,72)


ret_status = Sisyfos.Perform_Move(7, 905,121,22,33,44,54)
print ("ret_status=" .. ret_status)
print("")


ret_status = Sisyfos.Perform_Ranged_Attack(81,907,11,22)
print("")

ret_status = Sisyfos.Perform_Construction(8,908,1,2,3,4,5,6)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Demolition(88,909,11,22,33,44,55,66)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Grant_Piece_Effect(2,910,3,4,5)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Revoke_Piece_Effect(3,911,4,5,6)
print ("ret_status=" .. ret_status)
print("")

local area1 = {{a=2, b=4},{a=32, b=14},{a=21, b=17}, {a=21, b=18}}
local area2 = {{a=21, b=4},{a=26, b=7},{a=2, b=1}, {a=21, b=18}, {a=98, b=17}}

ret_status = Sisyfos.Grant_Patch_Effect(12,912,13,14,15,16,17, area1)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Revoke_Patch_Effect(22,913,23,24,25,26,27, area2)
print ("ret_status=" .. ret_status)
print("")

local area3 = {{a=1, b=2},{a=3, b=4},{a=5, b=6}}

ret_status = Sisyfos.Perform_Patch_Effect(78,920,33,4,5,area3)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Piece_Effect(87,921,34,89,98,5,4)
print ("ret_status=" .. ret_status)
print("")


print("Find_Piece_In_List - TEST")
local piece, position = Sisyfos.Find_Piece_In_List(1)
if piece.id == 76
and piece.type_of_piece == 1 
and piece.category == Sisyfos.Category.FIGHTING_PIECE
and piece.name == "Dummy Name" 
and piece.player_id == 1 
and position.a == 5
and position.b == 7 then
  print("Find_Piece_In_List -                                                OK")
else
  print("Find_Piece_In_List -                                                FAILED")
end
print("")

print("Opponents_Activity_Report_Append - TEST")
Sisyfos.Opponents_Activity_Report_Append(10, 40, "Opponents_Activity_Report_Append")
print("");

print("Player_Activity_Report_Append - TEST")
Sisyfos.Player_Activity_Report_Append(11, 41, "Player_Activity_Report_Append")
print("");

print("Opponents_System_Report_Append - TEST")
Sisyfos.Opponents_System_Report_Append(12, 42, "Opponents_System_Report_Append")
print("");

print("Player_System_Report_Append - TEST")
Sisyfos.Player_System_Report_Append(13, 43, "Player_System_Report_Append")
print("");


print("Get_Player_Name - TEST")
local player_name = Sisyfos.Get_Player_Name(5)

if player_name == "The Player Name" then
  print("Get_Player_Name -                                                   OK")
else
  print("Get_Player_Name -                                                   FAILED")
end

print("")
print("Get_Map_Terrain - TEST")
local a_terrain = Sisyfos.Get_Map_Terrain(6, 8)

if a_terrain == 3 then
  print("Get_Map_Terrain -                                                   OK")
else
  print("Get_Map_Terrain -                                                   FAILED")
end

print("")
print("Get_Map_Construction_List - TEST")
local a_construction_list = Sisyfos.Get_Map_Construction_List(6, 8)

print("1: " .. a_construction_list[1] .. " 2: " .. a_construction_list[2] .. " 3: " .. a_construction_list[3])
if a_construction_list[1] == 201
and a_construction_list[2] == 202 
and a_construction_list[3] == 203 then
  print("Get_Map_Construction_List -                                         OK")
else
  print("Get_Map_Construction_List -                                         FAILED")
end

print("")
local a_pieces_list = Sisyfos.Get_Map_Pieces_List(4,4)
if a_pieces_list[1] == 92
and a_pieces_list[2] == 23 
and a_pieces_list[3] == 42 then
  print("Get_Map_Pieces_List -                                               OK")
else
  print("Get_Map_Pieces_List -                                               FAILED")
end


print("")
print("Observe_Game_Minimum_Details - TEST")
Sisyfos.Observe_Game_Minimum_Details(697)


print("")
print("Observe_Game - TEST")
Sisyfos.Observe_Game(879)

print("")
print("")
print("")
print("Lua script completed")





