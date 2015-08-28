// This source file is part of HGamer3D
// (A project to enable 3D game development in Haskell)
// For the latest info, see http://www.hgamer3d.org
//
// (c) 2015 Peter Althainz
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// should match Urho3D-Binding/interface.h !!!

#include <stdint.h>

typedef int (*msgFP)(void *p, char* data, int len);
typedef int (*initFP)(void *p);

int hg3durho3d0_create_item(uint64_t idItemType, char* c_initData, int len_id, void** p);
int hg3durho3d0_destroy_item(uint64_t idItemType, void* p);
int hg3durho3d0_get_msg_sender(uint64_t idItemType, uint64_t idPropType, msgFP *f);
int hg3durho3d0_register_msg_receiver(uint64_t idItemType, uint64_t idEvtType, void *p, msgFP f);
char *hg3durho3d0_error_message(int error_id);
