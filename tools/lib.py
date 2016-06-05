# dodo file for building HGamer3D components

#	HGamer3D hgamer3d (A project to enable 3D game development in Haskell)
#	Copyright 2015 Peter Althainz
#	
#	Distributed under the Apache License, Version 2.0
#	(See attached file LICENSE or copy at 
#	http://www.apache.org/licenses/LICENSE-2.0)
# 
#	file: lib.py

import os, os.path, platform, string, sys
from doit.tools import config_changed, run_once


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
	if os_name == "Darwin":
		return "darwin"
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


