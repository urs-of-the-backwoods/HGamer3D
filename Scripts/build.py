#!/usr/bin/python

import sys, os, os.path
from pynt import task

mydir = os.path.realpath(os.path.dirname(__file__))

@task()
def urho3d():
    """builds and installs Urho3D"""    
    os.system("bash " + mydir + "/building/buildUrho3d.sh")

@task()
def binding():
    """builds and installs the binding library to Urho3D"""
    os.system("bash " + mydir + "/building/buildBinding.sh " + mydir + "/..")

@task()
def haskell():
    """install Haskell"""
    os.system("bash " + mydir + "/building/installHaskell.sh")

@task()
def hgamer3d():
    """builds HGamer3D"""
    os.system("bash " + mydir + "/building/buildHGamer3D.sh " + mydir + "/..")

@task(urho3d, haskell, hgamer3d)
def all():
    """builds all pre-requisites and HGamer3D"""
    pass

