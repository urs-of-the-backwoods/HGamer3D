# dodo file for building HGamer3D components

#	HGamer3D hgamer3d (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
# 
#	file: dodo.py

import os, os.path, platform, string, sys
from doit.tools import config_changed, run_once


#
# CONFIGURATION
#

# version information for all subcomponents
version_gamegio = "0.7.0"
version_media = "1.5.0"
version_engine = "1.5.0"
version_media_pack1 = "1.0.0"
version_hgamer3d = "0.7.1"
version_intonaco = "0.1.0"
version_luascripts = "0.1.1"
version_fresco = "0.1.1"

# where is Urho3D located
urho3d_home = os.path.abspath("../Urho3D-1.5").replace(os.sep, "/")
urho3d_build = os.path.abspath("../Urho3D-Build").replace(os.sep, "/")


#
# SUBROUTINES
#

def make_dir(d, targets):
	d = d.replace("/", os.sep)
	if not os.path.exists(d):
		os.makedirs(d)

def get_os():
	os_name = platform.system()
	if os_name == "Linux":
		return "linux"
	if os_name == "Windows":
		return "windows"
	print "don't know, which platform this is: ", os_name
	sys.exit()

def get_arch():
	ar = platform.machine()
	if ar == "AMD64" or ar == "x86_64":
		return "amd64"
	if ar == "i386":
		return "386"
	print "don't know, which arch this is: ", ar
	sys.exit()

arch_os = get_arch() + "-" + get_os()

def short_version(version):
	return string.join(string.split(version, '.')[0:2], '.')

def copy_file_replace(file_in, replace_dict, targets):
	with open(targets[0], "wt") as fout:
	    with open(file_in, "rt") as fin:
	        for line in fin:
	        	line_out = line
	        	for k in replace_dict.keys():
	        		line_out = line_out.replace(k, replace_dict[k])
		        fout.write(line_out)


#
# COMPONENTS
#

def task_gamegio():
	comp_name = 'gamegio-' + arch_os + '-' + version_gamegio
	targetdir = 'build-gamegio'
	component = 'GameEngineGio'

	if platform.system() == "Windows":
		cmake_cmd = 'cd build-gamegio && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../gamegio -G "Visual Studio 14 2015 Win64"'
		copy_cmd = 'cp build-gamegio/Release/game_gio_lib.dll build-gamegio/' + comp_name + '/game_engine.gio'
	else:
#		cmake_cmd = 'cd build-gamegio && cmake ../gamegio'
		cmake_cmd = 'cd build-gamegio && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../gamegio'
		copy_cmd = 'cp build-gamegio/libgame_gio_lib.so build-gamegio/' + comp_name + '/game_engine.gio'

	yield {
		'name' : 'init',
		'actions' : [
	    			(make_dir, [targetdir]),
					cmake_cmd
		],
	    'file_dep': [
			'gamegio/CMakeLists.txt',
			],
		'targets' : [targetdir]
	}

	yield {
		'name' : 'build',
	    'actions': [
	    			(make_dir, ['build-gamegio/' + comp_name]),
	    			'cd build-gamegio && cmake --build . --config Release',
	    			copy_cmd
	    ],
	    'file_dep': [
			'gamegio/gui.hpp',
			'gamegio/audio.hpp',
			'gamegio/input.cpp',
			'gamegio/errors.hpp',
			'gamegio/gui.cpp',
			'gamegio/graphics3d.cpp',
			'gamegio/CMakeLists.txt',
			'gamegio/graphics3d.hpp',
			'gamegio/input.hpp',
			'gamegio/audio.cpp',
			'gamegio/interface.cpp',
			'gamegio/interface.h',
			'gamegio/Slider2.hpp',
			'gamegio/Slider2.cpp',
			'gamegio/LineEdit2.hpp',
			'gamegio/LineEdit2.cpp',
		],
	    'targets': ['build-gamegio/' + comp_name + '/game_engine.gio',
	    ],
	} 

	yield {
		'name' : 'local',
	    'actions': [
	    	(copy_file_replace, ['component/' + component, {'{version}' : version_gamegio, '{version_engine}' : short_version(version_engine)} ]),
			'aio local http://www.hgamer3d.org/component/' +  component + ' ' + targetdir + ' || true'
	    ],
	    'targets': [targetdir + '/arriccio.toml'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'component/' + component,
		]
	} 

	yield {
		'name' : 'package',
		'targets' : 
			[
			'build-components/' + comp_name + '.tar.gz',
			'build-components/' + comp_name + '.tar.gz.sig'
			],
		'task_dep' : ['component_dir', 
		],
		'file_dep' : [
					targetdir + '/' + comp_name + '/game_engine.gio',
					targetdir + '/arriccio.toml'
					],
		'actions' : [
			'cd ' + targetdir + '/' + comp_name + ' && tar czf ../../build-components/' + comp_name + '.tar.gz *',
			'cd build-components && aio sign ' + comp_name + '.tar.gz ~/.ssh/id_rsa',
			'cp ' + targetdir + '/arriccio.toml build-components/' +  component,
			'cd build-components && aio sign ' + component + ' ~/.ssh/id_rsa'
		]
	}


def task_engine_cmp():
	comp_name = 'engine-' + arch_os + '-' + version_engine
	targetdir = 'build-engine'
	component = 'Engine'

	if platform.system() == "Windows":
		yield {
			'name' : 'init',
			'actions' : [
		    			(make_dir, [targetdir + '/' + comp_name]),
		    			'cp -r ../Urho3D-Build/bin/Urho3D.dll ' + targetdir + '/' + comp_name,
		    			'cp -r ../Urho3D-Build/bin/d3dcompiler_47.dll ' + targetdir + '/' + comp_name,
			],
			'uptodate' : [run_once],
			'targets' : ['build-engine']
		}
	else:
		yield {
			'name' : 'init',
			'actions' : [
		    			(make_dir, [targetdir + '/' + comp_name]),
		    			'cp -r ../Urho3D-Build/lib/libUrho3D.so* ' + targetdir + '/' + comp_name,
			],
			'uptodate' : [run_once],
			'targets' : ['build-engine']
		}


	yield {
		'name' : 'local',
	    'actions': [
	    	(copy_file_replace, ['engine/Engine', {'{version}' : version_engine, '{version_media}' : short_version(version_media)} ]),
				'aio local http://www.hgamer3d.org/component/Engine build-engine || true'
	    ],
	    'targets': ['build-engine/arriccio.toml'],
	    'uptodate': [config_changed(version_engine)],
	    'file_dep': [
			'engine/Engine',
		]
	} 

def task_media_cmp():

	for (tdir, mdir, mversion, msource, mcomponent) in [
		('build-media', 'build-media/media-', version_media, '../HGamer3D-Media/Plain-1.5', 'MediaPlain'),
		('build-media-pack1', 'build-media-pack1/media-pack1-', version_media_pack1, '../HGamer3D-Media/MediaPack1', 'MediaPack1')
	]:

		yield {
			'name' : 'init-' + mcomponent,
			'actions' : [
		    			 (make_dir, [mdir + mversion]),
						 'cp -r ' + msource + '/* ' + mdir + mversion,
			],
			'uptodate' : [run_once],
			'targets' : [mdir + mversion]
		}

		yield {
			'name' : 'local-' + mcomponent,
		    'actions': [
		    	(copy_file_replace, ['media/' + mcomponent, {'{version}' : mversion} ]),
					'aio local http://www.hgamer3d.org/component/' + mcomponent + ' ' + tdir + ' || true'
		    ],
		    'targets': [tdir + '/arriccio.toml'],
		    'file_dep': [
				'media/' + mcomponent,
			]
		} 

def task_hgamer3d():

	yield {
		'name' : 'init',
	    'actions': [
	    	(copy_file_replace, ['hgamer3d/HGamer3D.cabal.tmpl', {'{version}' : version_hgamer3d} ]),
	    ],
	    'targets': ['hgamer3d/HGamer3D.cabal'],
	    'uptodate': [config_changed(version_hgamer3d)],
	    'file_dep': [
			'hgamer3d/HGamer3D.cabal.tmpl',
		]
	} 

	yield {
		'name' : 'build',
		'actions' : [
					 'cd hgamer3d && stack build',
					 'cd hgamer3d && stack sdist',
	    			 (make_dir, ['build-hgamer3d']),
					 'cd hgamer3d && bash -c "cp `find .stack-work | grep .tar.gz` ../build-hgamer3d"'
					],
		'targets' : ['build-hgamer3d/HGamer3D-' + version_hgamer3d + '.tar.gz'],
	    'file_dep': [
	    	'hgamer3d/HGamer3D.cabal',
	    	'hgamer3d/LICENSE',
	    	'hgamer3d/stack.yaml',
			'hgamer3d/HGamer3D/Data/Angle.hs',
			'hgamer3d/HGamer3D/Data/Colour.hs',
			'hgamer3d/HGamer3D/Data/LMH.hs',
			'hgamer3d/HGamer3D/Data/GameTime.hs',
			'hgamer3d/HGamer3D/Data/Geometry2D.hs',
			'hgamer3d/HGamer3D/Data/Transform3D.hs',
			'hgamer3d/HGamer3D/Data/TypeSynonyms.hs',
			'hgamer3d/HGamer3D/Data/Vector.hs',
			'hgamer3d/HGamer3D/Data/Window.hs',
			'hgamer3d/HGamer3D/Data/PlayCmd.hs',
			'hgamer3d/HGamer3D/Data.hs',
			'hgamer3d/HGamer3D/Util/FileLocation.hs',
			'hgamer3d/HGamer3D/Util/UniqueName.hs',
			'hgamer3d/HGamer3D/Util/Variable.hs',
			'hgamer3d/HGamer3D/Util.hs',
			'hgamer3d/HGamer3D/Graphics3D.hs',
			'hgamer3d/HGamer3D/Graphics3D/Camera.hs',
			'hgamer3d/HGamer3D/Graphics3D/Geometry.hs',
			'hgamer3d/HGamer3D/Graphics3D/Light.hs',
			'hgamer3d/HGamer3D/Graphics3D/Material.hs',
			'hgamer3d/HGamer3D/Graphics3D/Window.hs',
			'hgamer3d/HGamer3D/Graphics3D/Graphics3DCommand.hs',
			'hgamer3d/HGamer3D/Graphics3D/Graphics3DConfig.hs',
			'hgamer3d/HGamer3D/Input/Mouse.hs',
			'hgamer3d/HGamer3D/Input/Keyboard.hs',
			'hgamer3d/HGamer3D/Input/Joystick.hs',
			'hgamer3d/HGamer3D/Input/InputEventHandler.hs',
			'hgamer3d/HGamer3D/Input.hs',
		    'hgamer3d/HGamer3D/GUI/Button.hs',
		    'hgamer3d/HGamer3D/GUI/EditText.hs',
		    'hgamer3d/HGamer3D/GUI/Text.hs',
		    'hgamer3d/HGamer3D/GUI/Slider.hs',
		    'hgamer3d/HGamer3D/GUI/CheckBox.hs',
		    'hgamer3d/HGamer3D/GUI/DropDownList.hs',
		    'hgamer3d/HGamer3D/GUI.hs',
		    'hgamer3d/HGamer3D/Audio/SoundSource.hs',
		    'hgamer3d/HGamer3D/Audio/SoundListener.hs',
		    'hgamer3d/HGamer3D/Audio/Volume.hs',
		    'hgamer3d/HGamer3D/Audio.hs',
			'hgamer3d/HGamer3D.hs',
		],
	}

def task_examples():

	yield {
		'name' : 'init',
		'actions' : [
	    			(make_dir, ['build-examples']),
	    			'cp examples/* build-examples'
					],
		'targets' : [
						'build-examples/RotatingCube.hs',
						'build-examples/Gui1.hs',
						'build-examples/SoundEffects.hs',
						'build-examples/Cuboid2.hs',
						'build-examples/Materials.hs',
						'build-examples/Billion.hs',
						'build-examples/SpaceInvaders.hs',
						'build-examples/stack.yaml'
					],
	    'file_dep': [
						'examples/RotatingCube.hs',
						'examples/Gui1.hs',
						'examples/SoundEffects.hs',
						'examples/Cuboid2.hs',
						'examples/Materials.hs',
						'examples/Billion.hs',
						'examples/SpaceInvaders.hs',
					],
		}

	yield {
		'name' : "build",
		'actions' : [
						'cd build-examples && aio Stack build HGamer3D', 
						'cd build-examples && aio Stack exec ghc -- -threaded RotatingCube.hs',
						'cd build-examples && aio Stack exec ghc -- -threaded Gui1.hs',
						'cd build-examples && aio Stack ghc -- -threaded SoundEffects.hs',
						'cd build-examples && aio Stack exec ghc -- -threaded Cuboid2.hs',
						'cd build-examples && aio Stack exec ghc -- -threaded Materials.hs',
						'cd build-examples && aio Stack exec ghc -- -threaded Billion.hs',
						'cd build-examples && aio Stack exec ghc -- -threaded SpaceInvaders.hs',
					],
		'targets' : [
						'build-examples/RotatingCube',
						'build-examples/Gui1',
						'build-examples/SoundEffects',
						'build-examples/Cuboid2',
						'build-examples/Materials',
						'build-examples/SpaceInvaders',
						'build-examples/Billion'
					],
	    'file_dep': [
						'examples/RotatingCube.hs',
						'examples/Gui1.hs',
						'examples/SoundEffects.hs',
						'examples/Cuboid2.hs',
						'examples/Materials.hs',
						'examples/Billion.hs',
						'examples/SpaceInvaders.hs',
					],
	}

def task_run_cmp():

	yield {
		'name' : 'init',
		'actions' : [
	    			(make_dir, ['build-run']),
					],
		'targets' : ['build-run'],
		'uptodate' : [run_once],
		}

	yield {
		'name' : 'local',
	    'actions': [
	    	(copy_file_replace, ['run/Run', {'{version}' : version_hgamer3d, '{version_media_examples}' : short_version(version_media_pack1), '{version_gamegio}' : short_version(version_gamegio), '{version_intonaco}' : short_version(version_intonaco)}]),
			'aio local http://www.hgamer3d.org/run/Run build-run || true',
			'aio alias Run http://www.hgamer3d.org/run/Run || true'
	    ],
	    'targets': ['build-run/arriccio.toml'],
	    'file_dep': [
			'run/Run',
		],
	    'uptodate': [
	    	config_changed(version_hgamer3d),
	    	config_changed(version_media_pack1),
	    	config_changed(version_gamegio),
	    	config_changed(version_intonaco)
	    	],
	}

def task_component_dir():
	return {
		'actions' : [
	    			(make_dir, ['build-components']),
					],
		'targets' : ['build-components'],
		'uptodate' : [run_once],
		}


def task_scripts_cmp():
	
	comp_name = 'scripts-' + version_luascripts
	targetdir = 'build-scripts'
	component = 'CreateProject'

	yield {
		'name' : 'init',
		'actions' : [
	    			(make_dir, [targetdir + '/' + comp_name]),
					],
		'targets' : [targetdir + '/' + comp_name],
		'uptodate' : [run_once],
		}

	for targetfile in [
					'create_project.lua',
					'os_name.lua'
					]:
		yield {
			'name' : 'build-' + targetfile,
			'actions' : [
		    	(copy_file_replace, ['scripts/' + targetfile, {
		    		'{version}' : version_luascripts,
		    		'{version_hgamer3d}' : version_hgamer3d,
		    		'{version_fresco}' : version_fresco
		    		}])
						],

			'targets' : [targetdir + '/' + comp_name + "/" + targetfile],
			'file_dep' : ['scripts/' + targetfile],
		    'uptodate': [config_changed(version_luascripts)],
		}


	yield {
		'name' : 'local',
	    'actions': [
	    	(copy_file_replace, ['scripts/' + component, {'{version}' : version_luascripts} ]),
			'aio local http://www.hgamer3d.org/component/' +  component + ' ' + targetdir + ' || true'
	    ],
	    'targets': [targetdir + '/arriccio.toml'],
	    'uptodate': [config_changed(version_luascripts)],
	    'file_dep': [
			'scripts/' + component,
		]
	} 

	yield {
		'name' : 'packet',
		'targets' : 
			[
			'build-components/' + comp_name + '.tar.gz',
			'build-components/' + comp_name + '.tar.gz.sig',
			'build-components/' + component,
			'build-components/' + component + '.sig',
			],
		'task_dep' : ['component_dir', 
		],
		'file_dep' : [
					targetdir + '/' + comp_name + '/create_project.lua',
					targetdir + '/' + comp_name + '/os_name.lua',
					targetdir + '/arriccio.toml'
					],
		'actions' : [
			'cd ' + targetdir + '/' + comp_name + ' && tar czf ../../build-components/' + comp_name + '.tar.gz *',
			'cd build-components && aio sign ' + comp_name + '.tar.gz ~/.ssh/id_rsa',
			'cp ' + targetdir + '/arriccio.toml build-components/' +  component,
			'cd build-components && aio sign ' + component + ' ~/.ssh/id_rsa'
		]
	}




