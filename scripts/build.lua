local glue = require("glue")
glue.luapath(glue.bin .. "/lualib")

require("os_arch")
require("util")
require("lfs")

-- get version of gamegio from CMakeLists file
local function versionGameGio()
	cmakeFile = glue.bin .. "/../gamegio-library/src/CMakeLists.txt"
	io.input(cmakeFile)

	local major = ""
	local minor = ""
	local patch = ""

	while true do
		local line = io.read()
		if line == nil then break end
		local r = string.match(line, "VERSION_MAJOR (.*)%)")
		if r then
			major = r
		end
		local r = string.match(line, "VERSION_MINOR (.*)%)")
		if r then
			minor = r
		end
		local r = string.match(line, "VERSION_PATCH (.*)%)")
		if r then
			patch = r
		end
	end

	return major .. "." .. minor .. "." .. patch
end

local function aio()
	return ".." .. pathSep() .. "scripts" .. pathSep() .."aio"
end

local function buildGameGio()
	-- change into build dir
	local currDir = lfs.currentdir()
	local version = versionGameGio()
	o, a = getOS()
	lfs.chdir(glue.bin .. "/..")
	lfs.mkdir("gamegio-build") 
	lfs.chdir("gamegio-build")
	-- build dll
	os.execute(aio() .. " http://www.hgamer3d.org/tools/Urho3D-1.6 cmd /C cmake ../gamegio-library/src -G \"Visual Studio 15 2017 Win64\"")
	os.execute("cmake --build . --config Release")
	-- package component
	lfs.mkdir("package")
	local plat = getPlatString("gamegio", version)
	lfs.mkdir("package/" .. plat)
	if o == "windows" then
		local platdir = "package\\" .. plat 
		os.execute("copy ..\\gamegio-library\\arriccio.toml package\\arriccio.toml")
		os.execute("copy Release\\game_gio_lib.dll " .. platdir .. "\\game_engine.gio")
	else
		local platdir = "package/" .. plat
		os.execute("cp ../gamegio-library/arriccio.toml arriccio.toml")
		os.execute("cp Release/game_gio_lib.dll " .. platdir .. "/game_engine.gio")
	end
	-- change dir back
	lfs.chdir(currDir)
end


if #arg > 0 then

	-- prepare host for building
	if arg[1] == "gamegio" then
		buildGameGio()
		os.exit(0)

	-- register component for local use
	elseif arg[1] == "register-gamegio" then
		lfs.chdir("gamegio-build")
		os.execute(aio() .. " local http://www.hgamer3d.org/component/HG3DEngineGio.0517 package")
		os.exit(0)

	-- unregister component for local use
	elseif arg[1] == "unregister-gamegio" then
		lfs.chdir("gamegio-build")
		os.execute(aio() .. " remove-local http://www.hgamer3d.org/component/HG3DEngineGio.0517")
		os.exit(0)
	end

-- give hints about commands
else
	print([[

HGamer3D build script, commands:

  build-gamegio
  register-gamegio
  unregister-gamegio

	]])
	os.exit(0)
end




