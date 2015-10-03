//	C++ part of bindings, outer interface functions
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/interface.h

#include "urho3dbinding_clib_export.h"

typedef int (*msgFP)(void *p, char* data, int len);
typedef int (*initFP)(void *p);

int URHO3DBINDING_CLIB_EXPORT hg3durho3d0_create_item(uint64_t idItemType, char* c_initData, int len_id, void** p);
int URHO3DBINDING_CLIB_EXPORT hg3durho3d0_destroy_item(uint64_t idItemType, void* p);
int URHO3DBINDING_CLIB_EXPORT hg3durho3d0_get_msg_sender(uint64_t idItemType, uint64_t idPropType, msgFP *f);
int URHO3DBINDING_CLIB_EXPORT hg3durho3d0_register_msg_receiver(uint64_t idItemType, uint64_t idEvtType, void *p, msgFP f);
char URHO3DBINDING_CLIB_EXPORT *hg3durho3d0_error_message(int error_id);

