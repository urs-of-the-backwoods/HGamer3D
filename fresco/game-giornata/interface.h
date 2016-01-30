//	C++ part of bindings, outer interface functions
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/interface.h

#include "game_gio_lib_export.h" 

typedef int (*msgFP)(void *p, char* data, int len);
typedef int (*msgFP2)(void *p1, void *p2, char* data, int len);
typedef int (*initFP)(void *p);

int GAME_GIO_LIB_EXPORT create_item(uint64_t idItemType, char* c_initData, int len_id, void** p);
int GAME_GIO_LIB_EXPORT destroy_item(uint64_t idItemType, void* p);
int GAME_GIO_LIB_EXPORT get_msg_sender(uint64_t idItemType, uint64_t idPropType, msgFP *f);
int GAME_GIO_LIB_EXPORT register_msg_receiver(uint64_t idItemType, uint64_t idEvtType, void *p1, void *p2, msgFP2 f);
char GAME_GIO_LIB_EXPORT *error_message(int error_id);

