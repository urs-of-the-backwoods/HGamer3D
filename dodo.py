# dodo file for building HGamer3D components

#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
# 
#	file: dodo.py

import os, os.path, platform, string, sys
from doit.tools import config_changed, run_once

# version information for all subcomponents
version_gamegio = "0.7.0"
version_media = "1.5.0"
version_engine = "1.5.0"
version_media_pack1 = "1.0.0"
version_hgamer3d = "0.7.1"
version_intonaco = "0.1.0"

# where is Urho3D located
urho3d_home = os.path.abspath("../Urho3D-1.5").replace(os.sep, "/")
urho3d_build = os.path.abspath("../Urho3D-Build").replace(os.sep, "/")

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

def task_gamegio():
	if platform.system() == "Windows":
		cmake_cmd = 'cd build-gamegio && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../fresco/game-giornata -G "Visual Studio 14 2015 Win64"'
		copy_cmd = 'cp build-gamegio/Release/game_gio_lib.dll build-gamegio/gamegio-' + arch_os + '-' + version_gamegio + '/game_engine.gio'
	else:
#		cmake_cmd = 'cd build-gamegio && cmake ../fresco/game-giornata'
		cmake_cmd = 'cd build-gamegio && cmake -D URHO3D_64BIT=1 -D URHO3D_LIB_TYPE=SHARED -D URHO3D_SRC=' + urho3d_home + ' -D URHO3D_HOME=' + urho3d_build + ' ../fresco/game-giornata'
		copy_cmd = 'cp build-gamegio/libgame_gio_lib.so build-gamegio/gamegio-' + arch_os + '-' + version_gamegio + '/game_engine.gio'

	yield {
		'name' : 'build-dir',
		'actions' : [
	    			(make_dir, ['build-gamegio']),
					cmake_cmd
		],
	    'file_dep': [
			'fresco/game-giornata/CMakeLists.txt',
			],
		'targets' : ['build-gamegio']
	}

	yield {
		'name' : 'library',
	    'actions': [
	    			(make_dir, ['build-gamegio/gamegio-' + arch_os + '-' + version_gamegio]),
	    			'cd build-gamegio && cmake --build . --config Release',
	    			copy_cmd
	    ],
	    'file_dep': [
			'fresco/game-giornata/gui.hpp',
			'fresco/game-giornata/audio.hpp',
			'fresco/game-giornata/input.cpp',
			'fresco/game-giornata/errors.hpp',
			'fresco/game-giornata/gui.cpp',
			'fresco/game-giornata/graphics3d.cpp',
			'fresco/game-giornata/CMakeLists.txt',
			'fresco/game-giornata/graphics3d.hpp',
			'fresco/game-giornata/input.hpp',
			'fresco/game-giornata/audio.cpp',
			'fresco/game-giornata/interface.cpp',
			'fresco/game-giornata/interface.h',
			'fresco/game-giornata/Slider2.hpp',
			'fresco/game-giornata/Slider2.cpp',
			'fresco/game-giornata/LineEdit2.hpp',
			'fresco/game-giornata/LineEdit2.cpp',
		],
	    'targets': ['build-gamegio/gamegio-' + arch_os + '-' + version_gamegio + '/game_engine.gio',
	    ],
	} 

	yield {
		'name' : 'arriccio',
	    'actions': [
	    	(copy_file_replace, ['component/GameEngineGio', {'{version}' : version_gamegio, '{version_engine}' : short_version(version_engine)} ]),
				'aio local http://www.hgamer3d.org/component/GameEngineGio build-gamegio || true'
	    ],
	    'targets': ['build-gamegio/arriccio.toml'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'component/GameEngineGio',
		]
	} 


def task_engine():
	targetdir = 'build-engine/engine-' + arch_os + '-' + version_engine
	if platform.system() == "Windows":
		yield {
			'name' : 'build-dir',
			'actions' : [
		    			(make_dir, [targetdir]),
		    			'cp -r ../Urho3D-Build/bin/Urho3D.dll ' + targetdir,
		    			'cp -r ../Urho3D-Build/bin/d3dcompiler_47.dll ' + targetdir,
			],
			'uptodate' : [run_once],
			'targets' : ['build-engine']
		}
	else:
		yield {
			'name' : 'build-dir',
			'actions' : [
		    			(make_dir, [targetdir]),
		    			'cp -r ../Urho3D-Build/lib/libUrho3D.so* ' + targetdir,
			],
			'uptodate' : [run_once],
			'targets' : ['build-engine']
		}


	yield {
		'name' : 'arriccio',
	    'actions': [
	    	(copy_file_replace, ['component/Engine', {'{version}' : version_engine, '{version_media}' : short_version(version_media)} ]),
				'aio local http://www.hgamer3d.org/component/Engine build-engine || true'
	    ],
	    'targets': ['build-engine/arriccio.toml'],
	    'uptodate': [config_changed(version_engine)],
	    'file_dep': [
			'component/Engine',
		]
	} 


def task_media():

	for (tdir, mdir, mversion, msource, mcomponent) in [
		('build-media', 'build-media/media-', version_media, '../HGamer3D-Media/Plain-1.5', 'MediaPlain'),
		('build-media-pack1', 'build-media-pack1/media-pack1-', version_media_pack1, '../HGamer3D-Media/MediaPack1', 'MediaPack1')
	]:

		yield {
			'name' : 'build-dir-' + mcomponent,
			'actions' : [
		    			 (make_dir, [mdir + mversion]),
						 'cp -r ' + msource + '/* ' + mdir + mversion,
			],
			'uptodate' : [run_once],
			'targets' : [mdir + mversion]
		}

		yield {
			'name' : 'arriccio-plain-' + mcomponent,
		    'actions': [
		    	(copy_file_replace, ['component/' + mcomponent, {'{version}' : mversion} ]),
					'aio local http://www.hgamer3d.org/component/' + mcomponent + ' ' + tdir + ' || true'
		    ],
		    'targets': [tdir + '/arriccio.toml'],
		    'file_dep': [
				'component/' + mcomponent,
			]
		} 

def task_hgamer3d():

	yield {
		'name' : 'cabal',
	    'actions': [
	    	(copy_file_replace, ['library/HGamer3D.cabal.tmpl', {'{version}' : version_hgamer3d} ]),
	    ],
	    'targets': ['library/HGamer3D.cabal'],
	    'uptodate': [config_changed(version_hgamer3d)],
	    'file_dep': [
			'library/HGamer3D.cabal.tmpl',
		]
	} 

	yield {
		'name' : 'library',
		'actions' : [
					 'cd library && stack build',
					 'cd library && stack sdist',
	    			 (make_dir, ['build-hgamer3d']),
					 'cd library && bash -c "cp `find .stack-work | grep .tar.gz` ../build-hgamer3d"'
					],
		'targets' : ['build-hgamer3d/HGamer3D-' + version_hgamer3d + '.tar.gz'],
	    'file_dep': [
	    	'library/HGamer3D.cabal',
	    	'library/LICENSE',
	    	'library/stack.yaml',
			'library/HGamer3D/Data/Angle.hs',
			'library/HGamer3D/Data/Colour.hs',
			'library/HGamer3D/Data/LMH.hs',
			'library/HGamer3D/Data/GameTime.hs',
			'library/HGamer3D/Data/Geometry2D.hs',
			'library/HGamer3D/Data/Transform3D.hs',
			'library/HGamer3D/Data/TypeSynonyms.hs',
			'library/HGamer3D/Data/Vector.hs',
			'library/HGamer3D/Data/Window.hs',
			'library/HGamer3D/Data/PlayCmd.hs',
			'library/HGamer3D/Data.hs',
			'library/HGamer3D/Util/FileLocation.hs',
			'library/HGamer3D/Util/UniqueName.hs',
			'library/HGamer3D/Util/Variable.hs',
			'library/HGamer3D/Util.hs',
			'library/HGamer3D/Graphics3D.hs',
			'library/HGamer3D/Graphics3D/Camera.hs',
			'library/HGamer3D/Graphics3D/Geometry.hs',
			'library/HGamer3D/Graphics3D/Light.hs',
			'library/HGamer3D/Graphics3D/Material.hs',
			'library/HGamer3D/Graphics3D/Window.hs',
			'library/HGamer3D/Graphics3D/Graphics3DCommand.hs',
			'library/HGamer3D/Graphics3D/Graphics3DConfig.hs',
			'library/HGamer3D/Input/Mouse.hs',
			'library/HGamer3D/Input/Keyboard.hs',
			'library/HGamer3D/Input/Joystick.hs',
			'library/HGamer3D/Input/InputEventHandler.hs',
			'library/HGamer3D/Input.hs',
		    'library/HGamer3D/GUI/Button.hs',
		    'library/HGamer3D/GUI/EditText.hs',
		    'library/HGamer3D/GUI/Text.hs',
		    'library/HGamer3D/GUI/Slider.hs',
		    'library/HGamer3D/GUI/CheckBox.hs',
		    'library/HGamer3D/GUI/DropDownList.hs',
		    'library/HGamer3D/GUI.hs',
		    'library/HGamer3D/Audio/SoundSource.hs',
		    'library/HGamer3D/Audio/SoundListener.hs',
		    'library/HGamer3D/Audio/Volume.hs',
		    'library/HGamer3D/Audio.hs',
			'library/HGamer3D.hs',
		],
	}

def task_examples():

	yield {
		'name' : 'build-dir',
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
						'build-examples/stack.yaml'
					],
	    'file_dep': [
						'examples/RotatingCube.hs',
						'examples/Gui1.hs',
						'examples/SoundEffects.hs',
						'examples/Cuboid2.hs',
						'examples/Materials.hs',
					],
		}

	yield {
		'name' : "compile",
		'actions' : [
						'cd build-examples && stack build HGamer3D', 
						'cd build-examples && stack exec ghc -- -threaded RotatingCube.hs',
						'cd build-examples && stack exec ghc -- -threaded Gui1.hs',
						'cd build-examples && stack exec ghc -- -threaded SoundEffects.hs',
						'cd build-examples && stack exec ghc -- -threaded Cuboid2.hs',
						'cd build-examples && stack exec ghc -- -threaded Materials.hs',
					],
		'targets' : [
						'build-examples/RotatingCube',
						'build-examples/Gui1',
						'build-examples/SoundEffects',
						'build-examples/Cuboid2',
						'build-examples/Materials',
					],
	    'file_dep': [
						'examples/RotatingCube.hs',
						'examples/Gui1.hs',
						'examples/SoundEffects.hs',
						'examples/Cuboid2.hs',
						'examples/Materials.hs',
					],
	}

def task_runner():

	yield {
		'name' : 'build-dir',
		'actions' : [
	    			(make_dir, ['build-runner']),
					],
		'targets' : ['build-runner'],
		'uptodate' : [run_once],
		}

	yield {
		'name' : 'runner',
	    'actions': [
	    	(copy_file_replace, ['component/Run', {'{version}' : version_hgamer3d, '{version_media_examples}' : short_version(version_media_pack1), '{version_gamegio}' : short_version(version_gamegio), '{version_intonaco}' : short_version(version_intonaco)}]),
			'aio local http://www.hgamer3d.org/component/Run build-runner || true',
			'aio alias Run http://www.hgamer3d.org/component/Run || true'
	    ],
	    'targets': ['build-runner/arriccio.toml'],
	    'file_dep': [
			'component/Run',
		]
	}
