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
#include <cmath>

#include "graphics3d.hpp"

using namespace std;

// Initialization of objects, remarks:
// -----------------------------------
// Objects will get two messages, on on creation with the original data, it was created with.
// This data will be handled in the create method.
// Then, if the object supports changes, it will also listen to the msg_obid_obid and allow
// changes during live-time, this will be handled during additional methods, as laid out in 
// the file interface.hpp, interface.cpp

// to be done, change all implementations to support a create and a msg_obid_obid 



// base graphics initialisation

Graphics3DSystem::Graphics3DSystem()
{
  engine = NULL;  
  context = NULL;
}

Graphics3DSystem::~Graphics3DSystem()
{
  engine->Exit();
//  delete engine;
  delete context;
}

int Graphics3DSystem::create(char* pdata, int len)
{

  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "system: " << obj << std::endl;
  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 4) return ERROR_TYPE_NOT_KNOWN;

  // misc default engine parameters
  engineParameters = Engine::ParseParameters(GetArguments());
  engineParameters["LogName"] = "HGamer3D-Graphics.log";
  engineParameters["WindowTitle"] = "HGamer3D";
  engineParameters["Multisample"] = 4;
    engineParameters["MaterialQuality"] = 2;
  engineParameters["LogLevel"] = LOG_DEBUG;

  // engine config
  msgpack::object ec_o = obj.via.array.ptr[0];
  engineParameters["Headless"] = ec_o.via.array.ptr[0].as<bool>();
  engineParameters["FlushGPU"] = ec_o.via.array.ptr[1].as<bool>();
  engineParameters["Threads"] = ec_o.via.array.ptr[2].as<bool>();
  engineParameters["ForceGL2"] = ec_o.via.array.ptr[3].as<bool>();

  // window config
  msgpack::object wc_o = obj.via.array.ptr[1];
  if (wc_o.via.array.ptr[3].as<bool>()) {  // check full screen
    engineParameters["Fullscreen"] = true;
  } else 
  {
    engineParameters["Fullscreen"] = false;
    // set window parameters
    engineParameters["WindowWidth"] = wc_o.via.array.ptr[0].as<int>();
    engineParameters["WindowHeight"] = wc_o.via.array.ptr[1].as<int>();
    engineParameters["Borderless"] = wc_o.via.array.ptr[2].as<bool>();
    engineParameters["WindowResizable"] = wc_o.via.array.ptr[4].as<bool>();
  }

  // logging config
  msgpack::object log_o = obj.via.array.ptr[2];
  if (log_o.type != msgpack::type::NIL) {
    // log level
    if (log_o.via.array.ptr[0].as<int>() == 0) {
      // no logging
      engineParameters["LogLevel"] = LOG_WARNING;
    } else if (log_o.via.array.ptr[0].as<int>() == 1) {
    // medium level logging
      engineParameters["LogLevel"] = LOG_INFO;
    } else if (log_o.via.array.ptr[0].as<int>() == 2) {
      // high level logging
      engineParameters["LogLevel"] = LOG_DEBUG;
    } 

    // quiet logging
    engineParameters["LogQuiet"] = log_o.via.array.ptr[1].as<bool>();
  
    // file name for logging
    String fileName;
    fileName.Append(log_o.via.array.ptr[2].as<std::string>().c_str());
    engineParameters["LogName"] = fileName;
  }

  // graphics quality
  msgpack::object gq_o = obj.via.array.ptr[3];
  if (gq_o.type != msgpack::type::NIL) {

    // shadow quality
    int sq = gq_o.via.array.ptr[0].as<int>();
    if (sq > 0)
    {
      engineParameters["Shadows"] = true;
      if (sq == 1) 
      {
        engineParameters["LowQualityShadows"] = true;
      }
      else
      {
        engineParameters["LowQualityShadows"] = false;
      }

    } else
    {
      engineParameters["Shadows"] = false;
    }

    // material quality
    int mq = gq_o.via.array.ptr[1].as<int>();
    engineParameters["MaterialQuality"] = mq;

    // texture quality
    int tq = gq_o.via.array.ptr[2].as<int>();
    engineParameters["TextureQuality"] = tq;

    // multisampling
    int ms = gq_o.via.array.ptr[3].as<int>();
    if (ms == 0)
    {
      engineParameters["Multisample"] = 1;
    } else if (ms == 1)
    {
      engineParameters["Multisample"] = 2;
    } else if (ms == 2)
    {
      engineParameters["Multisample"] = 4;
    }
  }

  context = new Context();
  engine = new Engine(context);
  if (!engine->Initialize(engineParameters))
  {
      ErrorExit();
      return ERROR_COULD_NOT_INITIALIZE_ENGINE;
  }
  scene = new Scene(context);
  scene->CreateComponent<Octree>();
  return OK;
}

int Graphics3DSystem::msgCmdGraphics3DSystem(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "g3dcmd: " << obj << std::endl;
    if (obj.type != msgpack::type::ARRAY || obj.via.array.size == 0) return ERROR_TYPE_NOT_KNOWN;

    if (obj.via.array.ptr[0].as<int>() == 1) {
        if (!engine->IsExiting())
            engine->RunFrame();
    }
    // no error
    return 0;
}



// Orientation, Position, Scale

HasNode::HasNode(Graphics3DSystem*g3ds)
{
  node = g3ds->scene->CreateChild();
}

HasNode::~HasNode()
{
  delete node;
}


int HasNode::msgOri(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
//  std::cout << "ori: " << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 4) return ERROR_TYPE_NOT_KNOWN;

  node->SetRotation(Quaternion(
      obj.via.array.ptr[0].as<float>(), 
      obj.via.array.ptr[1].as<float>(), 
      obj.via.array.ptr[2].as<float>(), 
      obj.via.array.ptr[3].as<float>()
      ));

  return 0;
};

int HasNode::msgPos(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
//  std::cout << "pos: " << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 3) return ERROR_TYPE_NOT_KNOWN;

  node->SetPosition(Vector3(
      obj.via.array.ptr[0].as<float>(), 
      obj.via.array.ptr[1].as<float>(), 
      obj.via.array.ptr[2].as<float>()
      ));
  return 0;
};

int HasNode::msgScale(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
//  std::cout << "scale: " << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 3) return ERROR_TYPE_NOT_KNOWN;

  node->SetScale(Vector3(
      obj.via.array.ptr[0].as<float>(), 
      obj.via.array.ptr[1].as<float>(), 
      obj.via.array.ptr[2].as<float>()
      ));
  return 0;
};




// Camera handling

CameraItem::CameraItem(Graphics3DSystem* g)
: HasNode(g)
{
  g3ds = g;
}

int CameraItem::create(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size > 2) return ERROR_TYPE_NOT_KNOWN;

  node->CreateComponent<Camera>();
  Renderer* renderer = node->GetSubsystem<Renderer>();
  viewportSlot = renderer->GetNumViewports();
  renderer->SetNumViewports(viewportSlot+1);  

  viewport = new Viewport(g3ds->context, g3ds->scene, node->GetComponent<Camera>());
  renderer->SetViewport(viewportSlot, viewport);

  // camera is main camera, we are done
  if (obj.via.array.ptr[0].as<int>() == 0) return 0;

  // camera is overlay camera
  if (obj.via.array.ptr[0].as<int>() == 1)
  {
    // viewport settings
    msgpack::object vp_o = obj.via.array.ptr[1];

    Graphics* graphics = node->GetSubsystem<Graphics>();
    int width = graphics->GetWidth();
    int height  = graphics->GetHeight();

    int left = round(vp_o.via.array.ptr[0].as<float>() * width);
    int top = round(vp_o.via.array.ptr[1].as<float>() * height);
    int right = left + round(vp_o.via.array.ptr[2].as<float>() * width);
    int bottom = top + round(vp_o.via.array.ptr[3].as<float>() * height);

    viewport->SetRect(IntRect(left, top, right, bottom));
  }

  return 0;
}

int CameraItem::msgFrustum(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 3) return ERROR_TYPE_NOT_KNOWN;

  setFrustum(obj.via.array.ptr[0].as<float>(),
              obj.via.array.ptr[1].as<float>(),
              obj.via.array.ptr[2].as<float>() * 57.2957795);    // 180/pi

  return 0;
}

CameraItem::~CameraItem()
{
  // check correct order of deletion for Cameras
  Renderer* renderer = node->GetSubsystem<Renderer>();
  int vpsNow = renderer->GetNumViewports();
  if (viewportSlot != (vpsNow -1)) 
  {
    Log::Write(LOG_WARNING, "HGamer3D: camera destroyed in wrong order! If multiple cameras are created, they need to be destroyed in reverse order of creation.");
  }
  if (vpsNow > viewportSlot) {
    renderer->SetNumViewports(viewportSlot);
  }
  node->RemoveComponent<Camera>();
  delete viewport;
}

void CameraItem::setFrustum(float nc, float fc, float fov)
{
  Camera* camera = node->GetComponent<Camera>();
  camera->SetNearClip(nc);
  camera->SetFarClip(fc);
  camera->SetFov(fov);  
}


LightItem::LightItem(Graphics3DSystem* g)
: HasNode(g)
{
}

int LightItem::create(char* pdata, int len)
{
  light = node->CreateComponent<Light>();
  msgLight(pdata, len);
  return 0;
}

LightItem::~LightItem()
{
  node->RemoveComponent<Light>();
}

int LightItem::msgLight(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "light:" << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size < 4) return ERROR_TYPE_NOT_KNOWN;

  // Light type parameters
  msgpack::object obj_t = obj.via.array.ptr[0];
  if (obj_t.via.array.ptr[0].as<int>() == 0)
  {
    light->SetLightType(LIGHT_POINT);

  } else if (obj_t.via.array.ptr[0].as<int>() == 1)
  {
    light->SetLightType(LIGHT_DIRECTIONAL);

  } else if (obj_t.via.array.ptr[0].as<int>() == 2)
  {
    light->SetLightType(LIGHT_SPOT);
    light->SetFov(obj_t.via.array.ptr[1].as<float>() * 57.2957795);
    light->SetAspectRatio(obj_t.via.array.ptr[2].as<float>());
  } 

  // light parameters
  // brightness, range, spec intesity, per vertex
  light->SetBrightness(obj.via.array.ptr[1].as<float>());
  light->SetRange(obj.via.array.ptr[2].as<float>());
  light->SetSpecularIntensity(obj.via.array.ptr[3].as<float>());
  light->SetPerVertex(false);

  return 0;
};

int LightItem::msgColour(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "light-colour:" << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 4) return ERROR_TYPE_NOT_KNOWN;

  // colour
  light->SetColor(Color(
      obj.via.array.ptr[0].as<float>(),
      obj.via.array.ptr[1].as<float>(),
      obj.via.array.ptr[2].as<float>()
      ));

  return 0;
};



// Geometry handling

GeometryItem::GeometryItem(Graphics3DSystem* g)
: HasNode(g)
{
  material = "Materials/DefaultGrey";   // default value, if no material is set
}

int GeometryItem::create(char* pdata, int len)
{
  node->CreateComponent<StaticModel>();
  return 0;
}

GeometryItem::~GeometryItem()
{
  node->RemoveComponent<StaticModel>();
}

int GeometryItem::msgGeometry(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "geo: " << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  StaticModel* model = node->GetComponent<StaticModel>();
  if (obj.via.array.ptr[0].as<int>() == 1) { // Basic Geometry
    msgpack::object shape_o = obj.via.array.ptr[1];
    if (shape_o.as<int>() == 1) { // sphere
      model->SetModel(cache->GetResource<Model>("Models/Sphere.mdl"));
    } else if (shape_o.as<int>() == 2) { // cube
      model->SetModel(cache->GetResource<Model>("Models/Box.mdl"));
    } else if (shape_o.as<int>() == 3) { // plane
      model->SetModel(cache->GetResource<Model>("Models/Plane.mdl"));
    } else if (shape_o.as<int>() == 4) { // cylinder
      model->SetModel(cache->GetResource<Model>("Models/Cylinder.mdl"));
    } else if (shape_o.as<int>() == 5) { // pyramid
      model->SetModel(cache->GetResource<Model>("Models/Pyramid.mdl"));
    } else if (shape_o.as<int>() == 6) { // torus
      model->SetModel(cache->GetResource<Model>("Models/Torus.mdl"));
    }  
    model->SetMaterial(cache->GetResource<Material>(material));
  } else if (obj.via.array.ptr[0].as<int>() == 2) { // mesh geometry
      model->SetModel(cache->GetResource<Model>(String(obj.via.array.ptr[1].as<char*>())));
  }

  return 0;
};

int GeometryItem::msgMaterial(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  std::cout << "geo: " << obj << std::endl;

  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  StaticModel* model = node->GetComponent<StaticModel>();

  if (obj.via.array.ptr[0].as<int>() == 1) {
    material = obj.via.array.ptr[1].as<std::string>().c_str();
    model->SetMaterial(cache->GetResource<Material>(material));
  }

  return 0;
};

int GeometryItem::msgColour(char* pdata, int len)
{

  // to be implemented
  return 0;
}



