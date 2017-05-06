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

local function buildGameGio()
	-- change into build dir
	local currDir = lfs.currentdir()
	local version = versionGameGio()
	lfs.chdir(glue.bin .. "/..")
	lfs.mkdir("gamegio-build") 
	lfs.chdir("gamegio-build")
	-- build dll
	os.execute(".." .. pathSep() .. "scripts" .. pathSep() .."aio http://www.hgamer3d.org/tools/Urho3D-1.6 cmd /C cmake ../gamegio-library/src -G \"Visual Studio 15 2017 Win64\"")
	os.execute("cmake --build . --config Release")
	-- package component
	lfs.mkdir("package")
	local platdir = "package/" .. getPlatString("gamegio", version)
	lfs.mkdir(platdir)


	-- change dir back
	lfs.chdir(currDir)
end


local function runPackage()
	local packageDir = absPath(relToScriptPath("../gamegio/package"))
	-- recreate empty package dir
	os.execute("rm -rf " .. packageDir)
	os.execute("mkdir " .. packageDir)
	local platDir = packageDir .. "/" .. getPlatString("gamegio", version)
	os.execute("mkdir " .. platDir)

	-- copy toml file
	os.execute("cp gamegio/arriccio.toml " .. packageDir .. "/arriccio.toml")
	
	-- copy executable
	local fs = assert(io.popen("ls gamegio/build"), "ls not working on your system")
	local s = nil
	while true do
		s = fs:read()
		if s then
			local m = s:match("libgame_gio")
			if m then
				os.execute("cp gamegio/build/" .. s .. " " .. platDir .. "/game_engine.gio")
				break
			end
		else
			break
		end
	end
	fs:close()
end

if #arg > 0 then

	-- prepare host for building
	if arg[1] == "gamegio" then
		buildGameGio()
		os.exit(0)


--[[
	-- build dependencies
	elseif arg[1] == "deps" then
		runDeps()
		os.exit(0)

	-- download dependencies (not needed, included in git, just first time or to check)
	elseif arg[1] == "download-deps" then
		runDownload()
		os.exit(0)

	-- prepare: setup missing libraries, build Urho3D
	elseif arg[1] == "build" then
		runBuild()
		os.exit(0)

	-- package build library for distribution
	elseif arg[1] == "package" then
		runPackage()
		os.exit(0)

	-- update build and package
	elseif arg[1] == "update" then
		runBuild()
		runPackage()
		os.exit(0)

	-- output version information
	elseif arg[1] == "version" then
		io.write(version)
		os.exit(0)

	-- register component for local use
	elseif arg[1] == "register" then
		os.execute("aio local http://www.hgamer3d.org/component/GameEngineGio gamegio/package")
		os.exit(0)

	-- unregister component for local use
	elseif arg[1] == "unregister" then
		os.execute("aio remove-local http://www.hgamer3d.org/component/GameEngineGio")
		os.exit(0)

	end

	print("wrong argument to build script:", arg[1])
	os.exit(-1)  -- wrong argument given
--]]
	end

-- give hints about commands
else
	print([[

HGamer3D build script, commands:

  build-gamegio

	]])
	os.exit(0)
end




