DOIT_CONFIG = {'default_tasks' : []}

def task_urho3d():
    """builds and installs Urho3D"""	
    return {
        'actions': ["bash Scripts/building/buildUrho3d.sh"],
        'targets': ["/usr/local/lib/Urho3D"],
        }

def task_binding():
    """builds and installs the binding library to Urho3D"""
    return {
        'actions': ["bash Scripts/building/buildBinding.sh"],
        'targets': ["/usr/local/lib/Urho3D/libhgamer3d062.so"],
        }
