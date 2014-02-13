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

// ClassEnet.cpp

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
extern "C" Enet_LIB_EXPORT void enet_enet_construct(struct hg3dclass_struct * result_c)
{
  HG3DEnet * result_cpp;
  result_cpp = (new HG3DEnet());
  *result_c = getHG3DClass_HG3DEnet((void *) result_cpp);
;
};

// 
extern "C" Enet_LIB_EXPORT void enet_enet_destruct(struct hg3dclass_struct * thisclass_c)
{
  HG3DEnet * thisclass_cpp = static_cast<HG3DEnet*> (getHG3DClassPtr(*thisclass_c, "HG3DEnet"));
  (delete thisclass_cpp);
};

// 
extern "C" Enet_LIB_EXPORT void enet_enet_createServer(int port_c, struct hg3dclass_struct * result_c)
{
  int port_cpp = (int)port_c;
  EnetServer * result_cpp;
  result_cpp = (HG3DEnet::createServer(port_cpp));
  *result_c = getHG3DClass_EnetServer((void *) result_cpp);
;
};

// 
extern "C" Enet_LIB_EXPORT void enet_enet_createClient(struct hg3dclass_struct * result_c)
{
  EnetClient * result_cpp;
  result_cpp = (HG3DEnet::createClient());
  *result_c = getHG3DClass_EnetClient((void *) result_cpp);
;
};

