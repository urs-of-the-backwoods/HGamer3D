-- HGamer3D
-- 
-- lua library
-- functions to get os and architecture informaiton

-- function to get os and arch
function getOS()
	local raw_os_name, raw_arch_name = '', ''

	local isWin = string.sub(os.tmpname(), 1, 1) == '\\'
	if isWin then
		-- Windows
		local env_OS = os.getenv('OS')
		local env_ARCH = os.getenv('PROCESSOR_ARCHITECTURE')
		if env_OS and env_ARCH then
			raw_os_name, raw_arch_name = env_OS, env_ARCH
		end
	else
		-- Unix-based OS
		raw_os_name = io.popen('uname -s','r'):read('*l')
		raw_arch_name = io.popen('uname -m','r'):read('*l')
	end

	raw_os_name = (raw_os_name):lower()
	raw_arch_name = (raw_arch_name):lower()

	local os_patterns = {
		['windows'] = 'windows',
		['linux'] = 'linux',
		['mac'] = 'darwin',
		['darwin'] = 'darwin',
		['^mingw'] = 'windows',
		['^cygwin'] = 'windows',
		['bsd$'] = 'BSD',
		['SunOS'] = 'Solaris',
	}
	
	local arch_patterns = {
		['^x86$'] = 'x86',
		['i[%d]86'] = 'x86',
		['amd64'] = 'amd64',
		['x86_64'] = 'amd64',
		['Power Macintosh'] = 'powerpc',
		['^arm'] = 'arm',
		['^mips'] = 'mips',
	}

	local os_name, arch_name = 'unknown', 'unknown'

	for pattern, name in pairs(os_patterns) do
		if raw_os_name:match(pattern) then
			os_name = name
			break
		end
	end
	for pattern, name in pairs(arch_patterns) do
		if raw_arch_name:match(pattern) then
			arch_name = name
			break
		end
	end
	return os_name, arch_name
end

-- function to get platform string
function getPlatString(name, version)
	o, a = getOS()
	return (name .. '-' .. a .. '-' .. o .. '-' .. version)
end

function pathSep()
	o, a = getOS()
	if o == "windows" then
		return '\\'
	else 
		return '/'
	end
end


