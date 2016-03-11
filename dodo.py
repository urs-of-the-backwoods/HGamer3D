# dodo file for building HGamer3D components

#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
# 
#	file: dodo.py

import os, os.path, platform
from doit.tools import config_changed

def get_os():
	os = platform.system()
	if os == "Linux":
		return "linux"
	if os == "Windows":
		return "windows"
	return "???"

def get_arch():
	ar = platform.machine()
	if ar == "x86_64":
		return "amd64"
	if ar == "i386":
		return "386"


arch_os = get_arch() + "-" + get_os()

# version information for all subcomponents
version_gamegio = "0.7.0"
version_media = "1.4.0"
version_media_examples = "0.1.0"
version_hgamer3d = "0.7.0"
version_intonaco = "0.1.0"

def copy_file_replace(file_in, replace_dict, targets):
	with open(targets[0], "wt") as fout:
	    with open(file_in, "rt") as fin:
	        for line in fin:
	        	line_out = line
	        	for k in replace_dict.keys():
	        		line_out = line_out.replace(k, replace_dict[k])
		        fout.write(line_out)


def task_gamegio():
	yield {
		'name' : 'build-dir',
		'actions' : ['mkdir -p build-gamegio',
					 'cd build-gamegio && cmake ../fresco/game-giornata'
		],
	    'file_dep': [
			'fresco/game-giornata/CMakeLists.txt',
			],
		'targets' : ['build-gamegio']
	}

	yield {
		'name' : 'library',
	    'actions': [
		 			'mkdir -p build-gamegio/gamegio-' + arch_os + '-' + version_gamegio,
	    			'cd build-gamegio && cmake --build . --config Release',
	    			'cp build-gamegio/libgame_gio_lib.so build-gamegio/gamegio-' + arch_os + '-' + version_gamegio + '/game_engine.gio',
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
	    	(copy_file_replace, ['interface/GameEngineGio', {'{version}' : version_gamegio} ]),
				'aio local http://www.hgamer3d.org/interface/GameEngineGio build-gamegio || true'
	    ],
	    'targets': ['build-gamegio/arriccio.toml'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'interface/GameEngineGio',
		]
	} 


def task_media():

	yield {
		'name' : 'build-dir-plain',
		'actions' : ['mkdir -p build-media/media-' + version_media,
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
	    	(copy_file_replace, ['interface/MediaPlain', {'{version}' : version_media} ]),
				'aio local http://www.hgamer3d.org/interface/MediaPlain build-media || true'
	    ],
	    'targets': ['build-media/arriccio.toml'],
	    'uptodate': [config_changed(version_media)],
	    'file_dep': [
			'interface/MediaPlain',
		]
	} 

def task_media_ex():

	yield {
		'name' : 'build-dir',
		'actions' : ['mkdir -p build-media-ex/media-examples-' + version_media_examples,
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
	    	(copy_file_replace, ['interface/MediaExamples', {'{version}' : version_media_examples} ]),
				'aio local http://www.hgamer3d.org/interface/MediaExamples build-media-ex || true'
	    ],
	    'targets': ['build-media-ex/arriccio.toml'],
	    'uptodate': [config_changed(version_media_examples)],
	    'file_dep': [
			'interface/MediaExamples',
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

	for example in [
		"RotatingCube",
		"Gui1",
		"SoundEffects",
		"Cuboid2",
	]:

		yield {
			'name' : "build-" + example,
			'actions' : [
						 'cd examples && stack build',
						 'mkdir -p build-examples/' + example + '/'  + example + 'Example-' + arch_os + '-' + version_hgamer3d,
						 'cd examples && bash -c "cp `find .stack-work | grep bin/' + example +'` ../build-examples/' + example + '/'  + example + 'Example-' + arch_os + '-' + version_hgamer3d + '"',
						],
			'targets' : ['build-examples/' + example + '/'  + example + 'Example-' + arch_os + '-' + version_hgamer3d + "/" + example,
			],
		    'file_dep': [
				'examples/' + example + '.hs',
			],
			}

		yield {
			'name' : 'arriccio-' + example,
		    'actions': [
		    	(copy_file_replace, ['interface/Example', {'{example}' : example, '{version}' : version_hgamer3d, '{version_media}' : version_media, '{version_media_examples}' : version_media_examples, '{version_gamegio}' : version_gamegio, '{version_intonaco}' : version_intonaco}]),
				'aio local http://www.hgamer3d.org/interface/' + example + 'Example build-examples/' + example + ' || true',
				'aio alias ' + example + ' http://www.hgamer3d.org/interface/' + example + 'Example || true'
		    ],
		    'targets': ['build-examples/' + example + '/arriccio.toml'],
		    'file_dep': [
				'interface/Example',
			]
		} 
