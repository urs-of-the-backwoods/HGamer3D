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

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdio.h>


#include "graphics3d.hpp"
#include "input.hpp"

uint64_t id_g3dsystem =       0x0884eb62b6674bff;
uint64_t id_cmdg3d =          0xea06bc20a9334af3;

uint64_t id_camera = 0xd3b0d455ab1f4716;
uint64_t id_frustum = 0xf3ce3235d4f8e73d;

uint64_t id_light =  0x981e80e50d994ea9;
uint64_t id_geo =    0xee433d1a4b964591;

uint64_t id_pos =    0x29aacbbb10c84016;
uint64_t id_scale =  0x2f9c124bc8fd41c4;
uint64_t id_ori =    0x815eb4d9c7bfaa74;

uint64_t id_mat = 0xb4bae8b0d0d8c162;
uint64_t id_col = 0xe202add0521cde41;

uint64_t id_input =  0xa532f43b1c1c6bc7;
uint64_t id_mouseevent = 0x27eaf3fd46595d08;

using namespace std;

//
// -----------------------
// Input
// -----------------------
//


//
// -----------------------
// Graphics3D
// -----------------------
//

// messages for g3dsystem
extern "C" int msg_g3dsystem_cmd(void* p, char* data, int len)
{
  return ((Graphics3DSystem *)p)->msgCmdGraphics3DSystem(data, len);
}

// messages for camera
extern "C" int msg_camera_frustum(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgFrustum(data, len);
}

extern "C" int msg_camera_ori(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgOri(data, len);
}

extern "C" int msg_camera_pos(void* p, char* data, int len)
{
  return ((CameraItem *)p)->msgPos(data, len);
}

// messages for light
extern "C" int msg_light_light(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgLight(data, len);
}

extern "C" int msg_light_ori(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgOri(data, len);
}

extern "C" int msg_light_pos(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgPos(data, len);
}

extern "C" int msg_light_col(void* p, char* data, int len)
{
  return ((LightItem *)p)->msgColour(data, len);
}

// messages for geometry
extern "C" int msg_geo_geo(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgGeometry(data, len);
}

extern "C" int msg_geo_ori(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgOri(data, len);
}

extern "C" int msg_geo_pos(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgPos(data, len);
}

extern "C" int msg_geo_scale(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgScale(data, len);
}

extern "C" int msg_geo_mat(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgMaterial(data, len);
}

extern "C" int msg_geo_col(void* p, char* data, int len)
{
  return ((GeometryItem *)p)->msgColour(data, len);
}



// System interface

static Graphics3DSystem* g3ds = NULL;

int hg3durho3d0_create_item(uint64_t idItemType, char* c_initData, int len_id, void** p)
{

  //
  // input
  //

  if (idItemType == id_input) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    InputEventHub *ieh = new InputEventHub(g3ds);
    int rv = ieh->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)ieh;
      return OK;
    } else {
      delete ieh;
      return rv; 
    }
  }   
 
  //
  // graphics3d
  // 

  if (idItemType == id_g3dsystem) { 
    if (g3ds) return ERROR_SYSTEM_ALREADY_INITIALIZED;
    g3ds = new Graphics3DSystem();
    int rv = g3ds->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)g3ds;
      return OK;
    } else {
      delete g3ds;
      g3ds = NULL;
      return rv; 
    }
  }   
  if (idItemType == id_camera) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    CameraItem *pc = new CameraItem(g3ds);
    int rv = pc->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pc;
      return OK;
    } else {
      delete pc;
      return rv; 
    }
  }   
  if (idItemType == id_light) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    LightItem *pl = new LightItem(g3ds);
    int rv = pl->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pl;
      return OK;
    } else {
      delete pl;
      return rv; 
    }
  }   
  if (idItemType == id_geo) {
    if (!g3ds) return ERROR_SYSTEM_NOT_INITIALIZED;
    GeometryItem *pg = new GeometryItem(g3ds);
    int rv = pg->create(c_initData, len_id);
    if ( rv == 0) {
      *p = (void *)pg;
      return OK;
    } else {
      delete pg;
      return rv; 
    }
  }   
  return ERROR_TYPE_NOT_KNOWN;
}

// an item can be destroyed by calling this function

int hg3durho3d0_destroy_item(uint64_t idItemType, void* p)
{
  // input

  if (idItemType == id_input) {
    delete ((InputEventHub *) p);
    return OK;
  }   

  // graphics3d

  if (idItemType == id_g3dsystem) {
    delete ((Graphics3DSystem *) p);
    g3ds = NULL;
    return OK;
  }   
  if (idItemType == id_camera) {
    delete ((CameraItem *) p);
    return OK;
  }   
  if (idItemType == id_light) {
    delete ((LightItem *) p);
    return OK;
  }   
  if (idItemType == id_geo) {
    delete ((GeometryItem *) p);
    return OK;
  }   
  return ERROR_TYPE_NOT_KNOWN;
}

// to send other types to an item

int hg3durho3d0_get_msg_sender(uint64_t idItemType, uint64_t idPropType, msgFP *f)
{

  // here we can see, which item understands which properties, in addition to its native message type
  
  // g3dsystem understands only cmd
  if (idItemType == id_g3dsystem && idPropType == id_cmdg3d) {
    *f = msg_g3dsystem_cmd;
    return OK;
  }   

  // camera understands camera, pos and ori
  if (idItemType == id_camera && idPropType == id_pos) {
    *f = msg_camera_pos;
    return OK;
  }   
  if (idItemType == id_camera && idPropType == id_ori) {
    *f = msg_camera_ori;
    return OK;
  }   

  if (idItemType == id_camera && idPropType == id_frustum) {
    *f = msg_camera_frustum;
    return OK;
  }   

  // light understands light, pos and ori, colour
  if (idItemType == id_light && idPropType == id_pos) {
    *f = msg_light_pos;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_ori) {
    *f = msg_light_ori;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_light) {
    *f = msg_light_light;
    return OK;
  }   
  if (idItemType == id_light && idPropType == id_col) {
    *f = msg_light_col;
    return OK;
  }   

  // geometry understands geo, material, colour,pos, scale and ori
  if (idItemType == id_geo && idPropType == id_pos) {
    *f = msg_geo_pos;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_ori) {
    *f = msg_geo_ori;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_scale) {
    *f = msg_geo_scale;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_geo) {
    *f = msg_geo_geo;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_mat) {
    *f = msg_geo_mat;
    return OK;
  }   
  if (idItemType == id_geo && idPropType == id_col) {
    *f = msg_geo_col;
    return OK;
  }   


  return ERROR_PROPERTY_NOT_AVAILABLE;
}

int hg3durho3d0_register_msg_receiver(uint64_t  idItemType, uint64_t idEvtType, void* p, msgFP f)
{
  if (idItemType == id_input && idEvtType == id_mouseevent) {
    ((InputEventHub*)p)->registerMouseEvent(f);
    return OK;
  }
}

char *hg3durho3d0_error_message(int error_id)
{
  switch (error_id) {
  case ERROR_SYSTEM_NOT_INITIALIZED : 
    return "Graphics3D: System is not initialized";
  case ERROR_SYSTEM_ALREADY_INITIALIZED : 
    return "Graphics3D: System is already initialized";
  case ERROR_PROPERTY_NOT_AVAILABLE : 
    return "Graphics3D: Item does not understand message";
  case ERROR_TYPE_NOT_KNOWN : 
    return "Graphics3D: Item type is not known (creation or deletion)";
  case ERROR_WRONG_PARAMETERS : 
    return "Graphics3D: Message send with wrong type";
  case ERROR_COULD_NOT_INITIALIZE_ENGINE :
    return "Graphics3D: Error intializing graphics engine";
  }
  return "Graphics3D: This error number is not correct";
}

