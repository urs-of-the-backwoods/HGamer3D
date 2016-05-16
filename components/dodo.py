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

# the buildinstructions for the components are not complete, some manual steps are needed

#
# CONFIGURATION
#

# include versions and some utilities
sys.path.append("..")
sys.path.append("../tools")
from versions import *
from lib import *

# where build will be located
build_dir = "build"
component_dir = "build/components"

# where is Urho3D located
urho3d_dir = os.path.abspath("../../Urho3D-1.5").replace(os.sep, "/")
urho3d_build_dir = os.path.abspath("../../Urho3D-Build").replace(os.sep, "/")

# where component data is placed
component_data_dir = '../../HGamer3D-Components/downloads'

#
# helper
#

def make_dirs_subtask(name, list_of_dirs):
	la = []
	for d in list_of_dirs:
		la.append( (make_dir, [d]) )
	return {
		'name' : name,
		'actions' : la,
		'uptodate' : [run_once],
		'targets' : list_of_dirs,
	}

def copy_files_subtask(name, files, dest):
	fa = []
	ta = []
	for f in files:
		fa.append(
				'cp -rL ' + f + ' ' + dest
			)
		ta.append(
				dest + '/' + os.path.basename(f)
			)
	return {
		'name' : name,
		'actions' : fa,
		'targets' : ta,
		'file_dep' : files,
	}

def copy_files_replace_subtask(name, files, dest, replace_dict):
	fa = []
	ta = []
	for f in files:
		fa.append(
		    	(copy_file_replace, [f, replace_dict])
			)
		ta.append(
				dest + '/' + os.path.basename(f)
			)
	return {
		'name' : name,
		'actions' : fa,
		'targets' : ta,
		'file_dep' : files,
	}


#		yield {
#			'name' : 'build-' + targetfile,
#			'actions' : [
#		    	(copy_file_replace, ['scripts/' + targetfile, {
#		    		'{version}' : version_luascripts,
#		    		'{version_hgamer3d}' : version_hgamer3d,
#		    		'{version_fresco}' : version_fresco
#		    		}])
#						],
#
#			'targets' : [targetdir + '/' + tarfolder + "/" + targetfile],
#			'file_dep' : ['scripts/' + targetfile],
#		    'uptodate': [config_changed(version_luascripts)],


def arriccio_subtask(name, template, component, version_dict, dest):
	return {
		'name' : name,
	    'actions': [
	    	(copy_file_replace, [template, version_dict ]),
			'aio local http://www.hgamer3d.org/component/' + component + ' ' + dest + ' || true'
	    ],
	    'targets': [dest + '/arriccio.toml'], 
	    'file_dep': [template],
	} 

def untar_data_subtask(name, tarfolder, dest, exception_list):
	tarfile = component_data_dir + '/' + tarfolder + '.tar.gz'
	files = os.popen('tar tzf ' + tarfile).read()[:-1].split('\n')
	tf = []
	for f in files:
		if f not in exception_list:
			tf.append(dest + '/' + tarfolder + '/' + f)
	return {
		'name' : name,
		'actions' : ['tar xzf ' + tarfile + ' -C ' + dest + '/' + tarfolder],
		'targets' : tf,
		'file_dep' : [component_data_dir + '/' + tarfolder + '.tar.gz'],
	}

#
# COMPONENTS
#

def task_engine():
	targetdir = build_dir + '/engine'
	tarfolder = 'engine-' + arch_os + '-' + version_engine
	component = 'Engine'

	yield make_dirs_subtask("dirs", [targetdir + '/' + tarfolder])

	if platform.system() == "Windows":
		yield untar_data_subtask("data", tarfolder, targetdir, ["Urho3D.dll"])
		yield copy_files_subtask("files", [
			urho3d_build_dir + '/bin/Urho3D.dll',
			], targetdir + '/' + tarfolder)
	else:
		libgcc = os.popen("find /lib -name libgcc_s.so.?").read()[:-1]
		libstdcpp = os.popen("find /usr/lib/x86_64-linux-gnu -name libstdc++.so.?").read()[:-1]

		yield copy_files_subtask("files", [
			libgcc,
			libstdcpp,
			urho3d_build_dir + '/lib/libUrho3D.so.0',
			], targetdir + '/' + tarfolder)

	yield arriccio_subtask("arriccio", "engine/Engine", "Engine", 
		{'{version}' : version_engine, '{version_media}' : short_version(version_media)},
		targetdir)

def task_media():
	targetdir = build_dir + '/media'
	tarfolder = 'media-' + version_media
	component = 'MediaPlain'

	yield make_dirs_subtask("dirs", [targetdir + '/' + tarfolder])
	yield untar_data_subtask('data', tarfolder, targetdir, [])
	yield arriccio_subtask("arriccio", "media/MediaPlain", component, 
		{'{version}' : version_media},
		targetdir)

def task_mediapack1():
	targetdir = build_dir + '/media-pack1'
	tarfolder = 'media-pack1-' + version_media_pack1
	component = 'MediaPack1'

	yield make_dirs_subtask("dirs", [targetdir + '/' + tarfolder])
	yield untar_data_subtask('data', tarfolder, targetdir, [])
	yield arriccio_subtask("arriccio", "media/MediaPack1", component, 
		{'{version}' : version_media_pack1},
		targetdir)

def task_runner():
	targetdir = build_dir + '/run'
	component = 'Run'

	yield make_dirs_subtask("dirs", [targetdir])
	yield arriccio_subtask("arriccio", "run/Run", component, 
		{
			'{version}' : version_hgamer3d, 
			'{version_media_examples}' : short_version(version_media_pack1), 
			'{version_gamegio}' : short_version(version_gamegio), 
			'{version_intonaco}' : short_version(version_intonaco)
		},
		targetdir)


def task_scripts_cmp():
	
	tarfolder = 'scripts-' + version_luascripts
	targetdir = 'build-scripts'
	component = 'CreateProject'

	yield make_dirs_subtask("dirs", [targetdir + '/' + tarfolder])


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

			'targets' : [targetdir + '/' + tarfolder + "/" + targetfile],
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
			'build-components/' + tarfolder + '.tar.gz',
			'build-components/' + tarfolder + '.tar.gz.sig',
			'build-components/' + component,
			'build-components/' + component + '.sig',
			],
		'file_dep' : [
					targetdir + '/' + tarfolder + '/create_project.lua',
					targetdir + '/' + tarfolder + '/os_name.lua',
					targetdir + '/arriccio.toml'
					],
		'actions' : [
			'cd ' + targetdir + '/' + tarfolder + ' && tar czf ../../build-components/' + tarfolder + '.tar.gz *',
			'cd build-components && aio sign ' + tarfolder + '.tar.gz ~/.ssh/id_rsa',
			'cp ' + targetdir + '/arriccio.toml build-components/' +  component,
			'cd build-components && aio sign ' + component + ' ~/.ssh/id_rsa'
		]
	}




