# dodo file for building HGamer3D components

#	HGamer3D Library (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
# 
#	file: dodo.py

import os, os.path
from doit.tools import config_changed

# version information for all subcomponents
version_gamegio = "0.7.0"
version_media = "1.4.0"
version_hgamer3d = "0.7.0"

def copy_file_replace(file_in, replace_in, replace_out, targets):
	with open(targets[0], "wt") as fout:
	    with open(file_in, "rt") as fin:
	        for line in fin:
	            fout.write(line.replace(replace_in, replace_out))


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
	    			'cd build-gamegio && cmake --build . --config Release',
	    			'cp build-gamegio/libgame_gio_lib.so build-gamegio/game_engine.gio',
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
			'fresco/game-giornata/interface.h'    
		],
	    'targets': ['build-gamegio/game_engine.gio',
	    ],
	} 

	yield {
		'name' : 'feed',
	    'actions': [
	    	(copy_file_replace, ['interface/GameEngineGio', '{version}', version_gamegio]),
				'0install add-feed http://www.hgamer3d.org/interface/GameEngineGio build-gamegio/GameEngineGio || true'
	    ],
	    'targets': ['build-gamegio/GameEngineGio'],
	    'uptodate': [config_changed(version_gamegio)],
	    'file_dep': [
			'interface/GameEngineGio',
		]
	} 


def task_media():

	yield {
		'name' : 'build-dir',
		'actions' : ['mkdir -p build-media',
					 'cp -r ../HGamer3D-Runtimes/Plain-1.4/* build-media'
		],
	    'file_dep': [
			'../HGamer3D-Runtimes/Plain-1.4/LICENSE.txt',
			'../HGamer3D-Runtimes/Plain-1.4/README_RUNTIME.txt',
			],
		'targets' : ['build-media']
	}

	yield {
		'name' : 'feed',
	    'actions': [
	    	(copy_file_replace, ['interface/MediaPlain', '{version}', version_media]),
				'0install add-feed http://www.hgamer3d.org/interface/MediaPlain build-media/MediaPlain || true'
	    ],
	    'targets': ['build-media/MediaPlain'],
	    'uptodate': [config_changed(version_media)],
	    'file_dep': [
			'interface/MediaPlain',
		]
	} 


def task_hgamer3d():

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
			'library/HGamer3D/Graphics3D/SystemGraphics3D.hs',
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

	return {
		'actions' : [
					 'cd examples && stack build',
					 'mkdir -p build-examples',
					 'cd examples && bash -c "cp `find .stack-work | grep bin/RotatingCube` ../build-examples"',
					 'cd examples && bash -c "cp `find .stack-work | grep bin/Gui1` ../build-examples"',
					 'cd examples && bash -c "cp `find .stack-work | grep bin/SoundEffects` ../build-examples"',
					],
		'targets' : ['build-examples/Gui1',
					 'build-examples/RotatingCube',
					 'build-examples/SoundEffects',
		],
	    'file_dep': [
			'examples/Gui1.hs',
			'examples/RotatingCube.hs',
			'examples/SoundEffects.hs',
		],
		}

