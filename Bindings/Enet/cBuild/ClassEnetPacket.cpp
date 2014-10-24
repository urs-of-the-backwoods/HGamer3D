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

// ClassEnetPacket.cpp

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
extern "C" Enet_LIB_EXPORT void enet_pck_getData(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  EnetPacket * thisclass_cpp = static_cast<EnetPacket*> (getHG3DClassPtr(*thisclass_c, "EnetPacket"));
  char * result_cpp;
  result_cpp = (thisclass_cpp->getData());
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// 
extern "C" Enet_LIB_EXPORT void enet_pck_getPeer(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  EnetPacket * thisclass_cpp = static_cast<EnetPacket*> (getHG3DClassPtr(*thisclass_c, "EnetPacket"));
  char * result_cpp;
  result_cpp = (thisclass_cpp->getPeer());
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

// 
extern "C" Enet_LIB_EXPORT void enet_pck_getChannel(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  EnetPacket * thisclass_cpp = static_cast<EnetPacket*> (getHG3DClassPtr(*thisclass_c, "EnetPacket"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getChannel());
  *result_c = (int)result_cpp;
};

// 
extern "C" Enet_LIB_EXPORT void enet_pck_destruct(struct hg3dclass_struct * thisclass_c)
{
  EnetPacket * thisclass_cpp = static_cast<EnetPacket*> (getHG3DClassPtr(*thisclass_c, "EnetPacket"));
  (delete thisclass_cpp);
};

