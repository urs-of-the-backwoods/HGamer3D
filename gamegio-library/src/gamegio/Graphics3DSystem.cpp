//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/Graphics3DSystem.hpp

using namespace std;

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>

#include "Fresco.hpp"
#include "Graphics3DSystem.hpp"

#include "Graphics3DConfigCbor.hpp"
#include "Graphics3DCommandCbor.hpp"

using namespace cbd;

// C Method for Messages
GIO_METHOD_FUNC(Graphics3DSystem, Command)

// Factory Implementation
GCO_FACTORY_IMP(Graphics3DSystem)
    GCO_FACTORY_METHOD(Graphics3DSystem, ctGraphics3DCommand, Command)
GCO_FACTORY_IMP_END


// utils

std::vector<std::string> &split1(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split1(s, delim, elems);
    return elems;
}
 
// Initialization of objects, remarks:
// -----------------------------------
// Objects will get two messages, on on creation with the original data, it was created with.
// This data will be handled in the create method.
// Then, if the object supports changes, it will also listen to the msg_obid_obid and allow
// changes during live-time, this will be handled during additional methods, as laid out in 
// the file interface.hpp, interface.cpp

// to be done, change all implementations to support a create and a msg_obid_obid 



// base graphics initialisation

Graphics3DSystem* Graphics3DSystem::singleton;

Graphics3DSystem::Graphics3DSystem()
{
  engine = NULL;  
  context = NULL;
}

Graphics3DSystem::~Graphics3DSystem()
{
  engine->Exit();
  delete context;
}

void Graphics3DSystem::msgDestroy()
{
  delete this;
}

/*
void hexDump(const uint8_t *buffer, size_t n)
{
    while (n--) {
        int r = printf("%02" PRIx8, *buffer++);
    }
    cout << std::endl;
}
*/

// static creation method!
FrItem Graphics3DSystem::msgCreate(FrMsg m, FrMsgLength l)
{

  Graphics3DSystem *g3ds = new Graphics3DSystem();  // create entity
  Graphics3DConfig config;

  // read message data
  CborParser parser; CborValue it;
//  hexDump(m, l);
  cbor_parser_init(m, l, 0, &parser, &it);
  readGraphics3DConfig(&it, &config);

  // misc other engine parameters
  VariantMap engineParameters = Engine::ParseParameters(GetArguments());
  engineParameters["SoundMixRate"] = 22050;
  engineParameters["WindowTitle"] = "HGamer3D";
//  engineParameters["SoundBuffer"] = 100;
//  engineParameters["SoundInterpolation"] = false;

  // engine config
  engineParameters["Headless"] = config.engine.headless;
  engineParameters["FlushGPU"] = config.engine.flushGPU;
  engineParameters["Threads"] = config.engine.threads;
  engineParameters["ForceGL2"] = config.engine.forceGL2;

  // graphics quality - shadow
  int sq = config.quality.shadow.selector;
  engineParameters["Shadows"] = (sq > Low);
  engineParameters["LowQualityShadows"] = (sq <= Medium);

  // graphics quality - material
  engineParameters["MaterialQuality"] = config.quality.material.selector;

  // graphics quality - texture
  engineParameters["TextureQuality"] = config.quality.texture.selector;

  // graphics quality - multisampling
  int ms = config.quality.multisample.selector;
  if (ms == Low)
  {
    engineParameters["Multisample"] = 1;
  } else if (ms == Medium)
  {
    engineParameters["Multisample"] = 2;
  } else if (ms == High)
  {
    engineParameters["Multisample"] = 4;
  }

  // logging - logLevel
  int loglevel =  config.logging.logLevel.selector;
  if (loglevel == Warning) {
    engineParameters["LogLevel"] = LOG_WARNING;
  } else if (loglevel == Info) {
    engineParameters["LogLevel"] = LOG_INFO;
  } else if (loglevel == Debug) {
    engineParameters["LogLevel"] = LOG_DEBUG;
  } 
  engineParameters["LogQuiet"] = config.logging.quietLogging;
  engineParameters["LogName"] = String(config.logging.logFileName.c_str());

  // window config
  engineParameters["Fullscreen"] = config.window.fullScreen;
  engineParameters["WindowWidth"] = config.window.width;
  engineParameters["WindowHeight"] = config.window.height;
  engineParameters["Borderless"] = config.window.borderless;
  engineParameters["WindowResizable"] = config.window.resizable;

  g3ds->context = new Context();
  g3ds->engine = new Engine(g3ds->context);

  if (!g3ds->engine->Initialize(engineParameters))
  {
      ErrorExit();
      return NULL;
  }

  g3ds->engine->SetAutoExit(false);
  g3ds->scene = new Scene(g3ds->context);
  g3ds->scene->CreateComponent<Octree>();
  
  // Enable OS cursor
  g3ds->context->GetSubsystem<Input>()->SetMouseVisible(true);
  
  // Initialize GUI, at least set a style
  ResourceCache* cache = g3ds->context->GetSubsystem<ResourceCache>();
  
  // add resource dirs from environment
  char* rpath = getenv("HG3D_RESOURCE_PATH");
  if (rpath != NULL) {
    std::string rp = string(rpath);
#ifdef _WIN32
    std::vector<std::string> elems = split(rp, ';');
#else
    std::vector<std::string> elems = split(rp, ':');
#endif

    for(std::vector<string>::iterator it = elems.begin(); it != elems.end(); ++it) {
      /* std::cout << *it; ... */
      cache->AddResourceDir(String((*it).c_str()));
    }
  }
 
  XMLFile* style = cache->GetResource<XMLFile>("UI/DefaultStyle.xml");
  UI* ui = g3ds->context->GetSubsystem<UI>();
  ui->GetRoot()->SetDefaultStyle(style);

  singleton = g3ds;
  return (void *)g3ds;
}

void Graphics3DSystem::msgCommand(FrMsg m, FrMsgLength l)
{
    // read message data
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    Graphics3DCommand command;
    readGraphics3DCommand(&it, &command);

    if (command.selector == Step)
    {
        if (!engine->IsExiting())
            engine->RunFrame();
    }
}


