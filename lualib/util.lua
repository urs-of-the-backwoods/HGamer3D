-- HGamer3D
-- 
-- lua library
-- utility functions

function relToScriptPath(localPath)
	local p = debug.getinfo(2).source:gsub('\\', '/'):match("@?(.*/)")
	if p then
--		print(p)
		return(p..localPath)
	else
		return localPath
	end
end

function absPath(localPath)
	local currentPath = assert(io.popen("pwd"), "pwd not working on your system")
	return currentPath:read() .. "/" .. localPath
end

