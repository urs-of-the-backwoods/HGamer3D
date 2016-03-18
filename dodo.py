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
version_media = "1.4.0"
version_media_examples = "0.1.0"
version_hgamer3d = "0.7.0"
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
	if ar == "AMD64":
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
		cmake_cmd = 'cd build-gamegio && cmake ../fresco/game-giornata'
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
	    	(copy_file_replace, ['component/GameEngineGio', {'{version}' : version_gamegio} ]),
				'aio local http://www.hgamer3d.org/component/GameEngineGio build-gamegio || true'
	    ],
	    'targets': ['build-gamegio/arriccio.toml'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'component/GameEngineGio',
		]
	} 


def task_media():

	yield {
		'name' : 'build-dir-plain',
		'actions' : [
	    			 (make_dir, ['build-media/media-' + version_media]),
					 'cp -r ../HGamer3D-Runtimes/Plain-1.4/* build-media/media-' + version_media,
		],
	    'file_dep': [
			'../HGamer3D-Runtimes/Plain-1.4/LICENSE.txt',
			'../HGamer3D-Runtimes/Plain-1.4/README_RUNTIME.txt',
			],
		'targets' : ['build-media/media-' + version_media]
	}

	yield {
		'name' : 'arriccio-plain',
	    'actions': [
	    	(copy_file_replace, ['component/MediaPlain', {'{version}' : version_media} ]),
				'aio local http://www.hgamer3d.org/component/MediaPlain build-media || true'
	    ],
	    'targets': ['build-media/arriccio.toml'],
	    'uptodate': [config_changed(version_media)],
	    'file_dep': [
			'component/MediaPlain',
		]
	} 

def task_media_ex():

	yield {
		'name' : 'build-dir',
		'actions' : [
	    			 (make_dir, ['build-media-ex/media-examples-' + version_media_examples]),
					 'cp -r ../HGamer3D-Runtimes/HGamer3D-1/* build-media-ex/media-examples-' + version_media_examples,
		],
	    'file_dep': [
			'../HGamer3D-Runtimes/HGamer3D-1/LICENSE.txt',
			'../HGamer3D-Runtimes/HGamer3D-1/README_RUNTIME.txt',
			],
		'targets' : ['build-media-ex/media-examples-' + version_media_examples]
	}

	yield {
		'name' : 'arriccio',
	    'actions': [
	    	(copy_file_replace, ['component/MediaExamples', {'{version}' : version_media_examples} ]),
				'aio local http://www.hgamer3d.org/component/MediaExamples build-media-ex || true'
	    ],
	    'targets': ['build-media-ex/arriccio.toml'],
	    'uptodate': [config_changed(version_media_examples)],
	    'file_dep': [
			'component/MediaExamples',
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
					 'mkdir -p build-hgamer3d',
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
					],
		'targets' : ['build-examples'],
		'uptodate' : [run_once],
		}

	yield {
		'name' : "compile",
		'actions' : [ 
						'cd examples && stack build',
					    'cd examples && bash -c "cp `find .stack-work | grep bin/RotatingCube` ../build-examples"',
					    'cd examples && bash -c "cp `find .stack-work | grep bin/Gui1` ../build-examples"',
					    'cd examples && bash -c "cp `find .stack-work | grep bin/SoundEffects` ../build-examples"',
					    'cd examples && bash -c "cp `find .stack-work | grep bin/Cuboid2` ../build-examples"',
					],
		'targets' : [
						'build-examples/RotatingCube',
						'build-examples/Gui1',
						'build-examples/SoundEffects',
						'build-examples/Cuboid1',
					],
	    'file_dep': [
						'examples/RotatingCube.hs',
						'examples/Gui1.hs',
						'examples/SoundEffects.hs',
						'examples/Cuboid2.hs',
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
	    	(copy_file_replace, ['component/Run', {'{version}' : version_hgamer3d, '{version_media}' : short_version(version_media), '{version_media_examples}' : short_version(version_media_examples), '{version_gamegio}' : short_version(version_gamegio), '{version_intonaco}' : short_version(version_intonaco)}]),
			'aio local http://www.hgamer3d.org/component/Run build-runner || true',
			'aio alias Run http://www.hgamer3d.org/component/Run || true'
	    ],
	    'targets': ['build-runner/arriccio.toml'],
	    'file_dep': [
			'component/Run',
		]
	}
