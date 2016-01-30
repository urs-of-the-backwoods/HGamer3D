#!/usr/bin/python

import sys, os, os.path
from pynt import task

mydir = os.path.realpath(os.path.dirname(__file__))

@task()
def urho3d():
    """builds and installs Urho3D"""    
    res = os.system("bash " + mydir + "/building/buildUrho3d.sh")
    if res != 0:
        sys.exit(res)

@task()
def binding():
    """builds and installs the binding library to Urho3D"""
    res = os.system("bash " + mydir + "/building/buildBinding.sh " + mydir + "/..")
    if res != 0:
        sys.exit(res)

@task()
def haskell():
    """install Haskell"""
    res = os.system("bash " + mydir + "/building/installHaskell.sh")
    if res != 0:
        sys.exit(res)

@task()
def samples():
    """builds HGamer3D samples"""
    res = os.system("bash " + mydir + "/building/buildHGamer3D.sh " + mydir + "/..")
    if res != 0:
        sys.exit(res)

@task(urho3d, binding, haskell)
def linux():
    """Installs all pre-requisites on Linux"""
    pass

@task(urho3d, binding, haskell, samples)
def travis():
    """Builds all steps for travis CI integration test"""
    pass

