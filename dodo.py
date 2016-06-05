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

# include versions and some utilities
sys.path.append("tools")
from versions import *
from lib import *

# where build will be located
build_dir = "build"
component_dir = "build/components"

# where is Urho3D located
urho3d_home = os.path.abspath("../Urho3D").replace(os.sep, "/")
urho3d_build = os.path.abspath("../Urho3D-Build").replace(os.sep, "/")



#
# COMPONENTS
#

def task_gamegio():
	comp_name = 'gamegio-' + arch_os + '-' + version_gamegio
	targetdir = build_dir + '/gamegio'
	component = 'GameEngineGio'

	if get_os() == "windows":
		cmake_cmd = 'cd ' + targetdir + ' && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../../gamegio -G "Visual Studio 14 2015 Win64"'
		copy_cmd = 'cp ' + targetdir + '/Release/game_gio_lib.dll ' + targetdir + '/' + comp_name + '/game_engine.gio'
	elif get_os() == "linux":
		cmake_cmd = 'cd ' + targetdir + ' && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../../gamegio'
		copy_cmd = 'cp ' + targetdir + '/libgame_gio_lib.so ' + targetdir + '/' + comp_name + '/game_engine.gio'
	else:
		cmake_cmd = 'cd ' + targetdir + ' && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../../gamegio'
		copy_cmd = 'cp ' + targetdir + '/libgame_gio_lib.dylib ' + targetdir + '/' + comp_name + '/game_engine.gio'


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
	    			(make_dir, [targetdir + '/' + comp_name]),
	    			'cd ' + targetdir + ' && cmake --build . --config Release',
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
	    'targets': [targetdir + '/' + comp_name + '/game_engine.gio',
	    ],
	} 

	yield {
		'name' : 'local',
	    'actions': [
	    	(copy_file_replace, ['gamegio/' + component, {'{version}' : version_gamegio, '{version_engine}' : short_version(version_engine)} ]),
			'aio local http://www.hgamer3d.org/component/' +  component + ' ' + targetdir + ' || true'
	    ],
	    'targets': [targetdir + '/arriccio.toml'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'gamegio/' + component,
		]
	} 

	yield {
		'name' : 'package',
		'targets' : 
			[
			component_dir + '/' + comp_name + '.tar.gz',
			component_dir + '/' + comp_name + '.tar.gz.sig'
			],
		'task_dep' : ['component_dir', 
		],
		'file_dep' : [
					targetdir + '/' + comp_name + '/game_engine.gio',
					targetdir + '/arriccio.toml'
					],
		'actions' : [
			'cd ' + targetdir + '/' + comp_name + ' && tar czf ../../../' + component_dir + '/' + comp_name + '.tar.gz *',
			'cd ' + component_dir + ' && aio sign ' + comp_name + '.tar.gz ~/.ssh/id_rsa',
			'cp ' + targetdir + '/arriccio.toml ' + component_dir + '/' +  component,
			'cd ' + component_dir + ' && aio sign ' + component + ' ~/.ssh/id_rsa'
		]
	}


def task_hgamer3d():
	targetdir = build_dir + '/hgamer3d'

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
					 'cd hgamer3d && aio Stack build',
					 'cd hgamer3d && aio Stack sdist',
	    			 (make_dir, [targetdir]),
					 'cd hgamer3d && bash -c "cp `find .stack-work | grep .tar.gz` ../' + targetdir + '"'
					],
		'targets' : [targetdir + '/HGamer3D-' + version_hgamer3d + '.tar.gz'],
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
	targetdir = build_dir + "/examples"

	yield {
		'name' : 'init',
		'actions' : [
	    			(make_dir, [targetdir]),
	    			'cp examples/* ' + targetdir
					],
		'targets' : [
						targetdir + '/RotatingCube.hs',
						targetdir + '/Gui1.hs',
						targetdir + '/SoundEffects.hs',
						targetdir + '/Cuboid2.hs',
						targetdir + '/Materials.hs',
						targetdir + '/Billion.hs',
						targetdir + '/SpaceInvaders.hs',
						targetdir + '/stack.yaml'
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
						'cd ' + targetdir + ' && aio Stack build HGamer3D', 
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded RotatingCube.hs',
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded Gui1.hs',
						'cd ' + targetdir + ' && aio Stack ghc -- -threaded SoundEffects.hs',
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded Cuboid2.hs',
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded Materials.hs',
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded Billion.hs',
						'cd ' + targetdir + ' && aio Stack exec ghc -- -threaded SpaceInvaders.hs',
					],
		'targets' : [
						targetdir + '/RotatingCube',
						targetdir + '/Gui1',
						targetdir + '/SoundEffects',
						targetdir + '/Cuboid2',
						targetdir + '/Materials',
						targetdir + '/SpaceInvaders',
						targetdir + '/Billion'
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

def task_component_dir():
	return {
		'actions' : [
	    			(make_dir, [component_dir]),
					],
		'targets' : [component_dir],
		'uptodate' : [run_once],
		}


