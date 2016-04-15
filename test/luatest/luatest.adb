with Lua;
with Server.Lua_Interface;
with Text_IO;
with Ada.Exceptions;
procedure LuaTest is
   The_Lua_State : Lua.Lua_State;
   Status        : Lua.Lua_Return_Code;

   use Lua;
begin
   The_Lua_State := Lua.New_State;
   Lua.Open_Libs (The_Lua_State);
   --  Load the lua "standard" libraries

   Server.Lua_Interface.Init(The_Lua_State);

   --
   Text_IO.Put_Line ("Load script");
   Lua.Load_File (The_Lua_State, "../luatest.lua");
   --  Load a script. Note that loading a script does not execute it. This
   --  includes toplevel code.

   Text_IO.Put_Line ("Execute script");
   Status := Lua.PCall (The_Lua_State);

   if Status /= Lua.LUA_OK then
      --  An error occurs during the execution
      Text_IO.Put_Line (Status'Img);
      Text_IO.Put_Line (To_Ada (The_Lua_State, -1));
   end if;

exception
   when E : Lua_Error =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));

end LuaTest;
