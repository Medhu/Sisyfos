
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

print("Concept test of Sisyfos - Lua interface")

print("TEST / DEMONSTRATION Global constants")
print("Sisyfos.Status.OUT_OF_MOVES=" .. Sisyfos.Status.OUT_OF_MOVES)
print("Sisyfos.Status.OK=" .. Sisyfos.Status.OK)
print("Sisyfos.Status.PATCH_BAD_TERRAIN=" .. Sisyfos.Status.PATCH_BAD_TERRAIN)
print("Sisyfos.Category.FIGHTING_PIECE=" .. Sisyfos.Category.FIGHTING_PIECE)
print("Sisyfos.Category.HOUSE_PIECE=" .. Sisyfos.Category.HOUSE_PIECE)

print("")
print("")

print("TEST Ada Procedures and Functions")

local ret_status = 0

piece_id, ret_status = Sisyfos.Create_Piece(900,
                          53,54, -- position
                          1005,1006,Sisyfos.Category.FIGHTING_PIECE,"TestName",2,
                          1,2, -- current player id player id
                          true)
print("piece_id=" .. piece_id .. " ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Put_Piece(901,
                       33,44, -- position
                       55,
                       1,2)  -- piece_id
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Remove_Piece(902,
                          34,45,
                          56,
                          1,2)
print ("ret_status=" .. ret_status)
print("")

winner, ret_status = Sisyfos.Perform_Attack_Pos(903,
                                    81,72,
                                    61,52,
                                    41,32,
                                    22,11)
print("winner=" .. winner .. " ret_status=" .. ret_status)
print("")

local path1 = {{a=2, b=4},{a=32, b=14},{a=21, b=17}, {a=21, b=18}, {a=21, b=11}, {a=21, b=19}, }

winner, ret_status = Sisyfos.Perform_Attack_Path(904,
                                    81,72,
                                    path1,
                                    22,11)
print("winner=" .. winner .. " ret_status=" .. ret_status)
print("")


ret_status = Sisyfos.Perform_Move_Pos(905,121,22,33,44,54,6,7)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Move_Path(906,121,path1,6,7)
print ("ret_status=" .. ret_status)
print("")


winner, ret_status = Sisyfos.Perform_Ranged_Attack(907,11,22,41,42,53,54,71,81)
print("winner=" .. winner .. " ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Construction(908,1,2,3,4,5,6,7,8)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Demolition(909,11,22,33,44,55,66,77,88)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Grant_Piece_Effect(910,3,4,5,1,2)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Revoke_Piece_Effect(911,4,5,6,2,3)
print ("ret_status=" .. ret_status)
print("")

local area1 = {{a=2, b=4},{a=32, b=14},{a=21, b=17}, {a=21, b=18}}
local area2 = {{a=21, b=4},{a=26, b=7},{a=2, b=1}, {a=21, b=18}, {a=98, b=17}}

ret_status = Sisyfos.Grant_Patch_Effect(912,13,14,15,16,17, area1, 11,12)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Revoke_Patch_Effect(913,23,24,25,26,27, area2,21,22)
print ("ret_status=" .. ret_status)
print("")

local area3 = {{a=1, b=2},{a=3, b=4},{a=5, b=6}}

ret_status = Sisyfos.Perform_Patch_Effect(920,33,88,99,4,5,area3,67,78)
print ("ret_status=" .. ret_status)
print("")

ret_status = Sisyfos.Perform_Piece_Effect(921,34,89,98,5,4,76,87)
print ("ret_status=" .. ret_status)
print("")

local ret_End_Turn = False

ret_End_Turn = Sisyfos.End_Turn(98)
if not ret_End_Turn then
  print("End_Turn - OK")
else
  print("End_Turn - FAILED")
end
print("")

print("Observation_Area - TEST")

local area3 = Sisyfos.Observation_Area(65)
if area3[1].a == 6 and area3[1].b == 7 
and area3[2].a == 8 and area3[2].b == 9 
and area3[3].a == 5 and area3[3].b == 4  then
  print("Observation_Area - OK")
else
  print("Observation_Area - FAILED")
end 
print("")


print("Movement_Capability - TEST")

local area4 = Sisyfos.Movement_Capability(67)
if area4[1].a == 1 and area4[1].b == 2 
and area4[2].a == 3 and area4[2].b == 4 
and area4[3].a == 5 and area4[3].b == 6  then
  print("Movement_Capability - OK")
else
  print("Movement_Capability - FAILED")
end 
print("")


print("Attack_Capability - TEST")

local area5 = Sisyfos.Attack_Capability(68)
if area5[1].a == 3 and area5[1].b == 4 
and area5[2].a == 5 and area5[2].b == 6 
and area5[3].a == 7 and area5[3].b == 8  then
  print("Attack_Capability - OK")
else
  print("Attack_Capability - FAILED")
end 
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
  print("Find_Piece_In_List - OK")
else
  print("Find_Piece_In_List - FAILED")
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
  print("Get_Player_Name - OK")
else
  print("Get_Player_Name - FAILED")
end

print("")
print("Get_Map_Terrain - TEST")
local a_terrain = Sisyfos.Get_Map_Terrain(6, 8)

if a_terrain == 3 then
  print("Get_Map_Terrain - OK")
else
  print("Get_Map_Terrain - FAILED")
end

print("")
print("Get_Map_Construction_List - TEST")
local a_construction_list = Sisyfos.Get_Map_Construction_List(6, 8)

print("1: " .. a_construction_list[1] .. " 2: " .. a_construction_list[2] .. " 3: " .. a_construction_list[3])
if a_construction_list[1] == 201
and a_construction_list[2] == 202 
and a_construction_list[3] == 203 then
  print("Get_Map_Construction_List - OK")
else
  print("Get_Map_Construction_List - FAILED")
end

print("")
local a_pieces_list = Sisyfos.Get_Map_Pieces_List(4,4)
if a_pieces_list[1] == 92
and a_pieces_list[2] == 23 
and a_pieces_list[3] == 42 then
  print("Get_Map_Pieces_List - OK")
else
  print("Get_Map_Pieces_List - FAILED")
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





