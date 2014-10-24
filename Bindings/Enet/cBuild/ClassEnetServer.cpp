// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
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
// 

// ClassEnetServer.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "EnetDllDefines.h"
	#include "ClassPtr.h"
	#include "./hg3denet.hpp"




// 
extern "C" Enet_LIB_EXPORT void enet_srv_destruct(struct hg3dclass_struct * thisclass_c)
{
  EnetServer * thisclass_cpp = static_cast<EnetServer*> (getHG3DClassPtr(*thisclass_c, "EnetServer"));
  (delete thisclass_cpp);
};

// 
extern "C" Enet_LIB_EXPORT void enet_srv_serve(struct hg3dclass_struct * thisclass_c, int msWait_c, int * result_c)
{
  EnetServer * thisclass_cpp = static_cast<EnetServer*> (getHG3DClassPtr(*thisclass_c, "EnetServer"));
  int msWait_cpp = (int)msWait_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->serve(msWait_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Enet_LIB_EXPORT void enet_srv_getPacket(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  EnetServer * thisclass_cpp = static_cast<EnetServer*> (getHG3DClassPtr(*thisclass_c, "EnetServer"));
  EnetPacket * result_cpp;
  result_cpp = (thisclass_cpp->getPacket());
  *result_c = getHG3DClass_EnetPacket((void *) result_cpp);
;
};

// 
extern "C" Enet_LIB_EXPORT void enet_srv_send(struct hg3dclass_struct * thisclass_c, char * peer_c, char * message_c, int channel_c)
{
  EnetServer * thisclass_cpp = static_cast<EnetServer*> (getHG3DClassPtr(*thisclass_c, "EnetServer"));
  const char * peer_cpp = (char*) peer_c;
  const char * message_cpp = (char*) message_c;
  int channel_cpp = (int)channel_c;
  (thisclass_cpp->send(peer_cpp, message_cpp, channel_cpp));
};

