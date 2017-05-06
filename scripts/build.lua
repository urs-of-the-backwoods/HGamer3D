local glue = require("glue")
glue.luapath(glue.bin .. "/../lualib")

require("os_arch")
require("util")
require("lfs")


-- get version of gamegio from CMakeLists file
local function getVersion()
	cmakeFile = relToScriptPath("../gamegio/src/CMakeLists.txt")
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

-- start script
local var version = getVersion()

local function runPrepare()
	local scriptPath = absPath(relToScriptPath("."))
	local cmd = scriptPath .. "/prepareHostLinux.sh"
	os.execute(cmd)
end

local function runDownload()
	-- download urho3d
	print("downloading Urho3d to downloads directory")
	local urhoDownloadPath = absPath(relToScriptPath("../urho3d"))
	local cmd = "wget -P " .. urhoDownloadPath .. " -nv https://github.com/Urho3D/urho3d/archive/7a16f9ca.zip"
	os.execute(cmd)
	-- download tinycbor
	local cborDownloadPath = absPath(relToScriptPath("../tinycbor"))
	print("downloading tinycbor to downloads directory")
	local cmd = "wget -P " .. cborDownloadPath .. " -nv https://github.com/01org/tinycbor/archive/a088996.zip"
	os.execute(cmd)
end

local function runCBOR()
	-- build tinycbor
	local cborZipFile = relToScriptPath("../tinycbor/a088996.zip")
	local buildPath = relToScriptPath("../tinycbor/build")
	-- create build directory
	lfs.mkdir(buildPath)
	-- extract zipfile
	os.execute("unzip -d " .. buildPath .. " " .. cborZipFile)
	os.rename(buildPath .. "/tinycbor-a088996aa5f59b4f27f20fadad053d88bee357d4", buildPath .. "/tinycbor")
	-- apply makefile patch
	os.execute("cd " .. buildPath .. " && patch tinycbor/Makefile ../patch-makefile")
	-- build tinycbor
	os.execute("cd " .. buildPath .. "/tinycbor && make")
end

local function runURHO()
	-- build urho3d
	local urho3dZipFile = absPath(relToScriptPath("../urho3d/7a16f9ca.zip"))
	local buildPath = absPath(relToScriptPath("../urho3d/build"))
	-- create build directory
	os.execute("mkdir " .. buildPath)
	-- extract zipfile
	os.execute("cd " .. buildPath .. " && unzip " .. urho3dZipFile .. " && mv Urho3D-* Urho3D")
	-- build Urho3d
	local urhoSrcPath = absPath(relToScriptPath("../urho3d/build/Urho3D"))
	local urhoBuildPath = absPath(relToScriptPath("../urho3d/build/Urho3D-Build"))
	os.execute("mkdir " .. urhoBuildPath)
	os.execute("cd " .. urhoBuildPath .. " && cmake -DURHO3D_LIB_TYPE=SHARED -DURHO3D_SAMPLES=0 -DURHO3D_64BIT=1 -DURHO3D_DEPLOYMENT_TARGET=core2 ../Urho3D")
	os.execute("cd " .. urhoBuildPath .. " && cmake --build . --config Release")
end

local function runDeps()
	runCBOR()
--	runURHO()
end

local function runBuild()
	local buildPath = absPath(relToScriptPath("../gamegio/build"))
	local urhoSrcPath = absPath(relToScriptPath("../urho3d/build/Urho3D"))
	local urhoBuildPath = absPath(relToScriptPath("../urho3d/build/Urho3D-Build"))
	-- create build directory
	os.execute("mkdir " .. buildPath)
	-- build gamegio
	os.execute("cd " .. buildPath .. " && cmake -DURHO3D_SRC=" .. urhoSrcPath .. " -DURHO3D_HOME=" .. urhoBuildPath .. " -DURHO3D_DEPLOYMENT_TARGET=core2 ../src")
	os.execute("cd " .. buildPath .. " && cmake --build . --config Release")
--	local cmd = "sudo docker run -v " .. gamegioPath .. ":/gamegio-library -v " .. buildPath .. ":/build debian:wheezy bash -c \"GAMEGIO_BUILD_DIR=/build GAMEGIO_SRC_DIR=/gamegio-library IS64BIT=1 ARCH_TARGET=core2 /gamegio-library/scripts/buildLinux.sh\""
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
	if arg[1] == "prepare" then
		runPrepare()
		os.exit(0)

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

-- give hints about commands
else
	print([[

gamegio-library build script, commands:

  prepare - prepare host for build (once)
  deps - build deps
  build - build library
  package - create package after build
  update - rebuild and repackage
  version - give version of library
  register - register local version in aio
  unregister - remove registration in aio
  download-deps - not needed, zips are included in git repo

	]])
	os.exit(0)
end




