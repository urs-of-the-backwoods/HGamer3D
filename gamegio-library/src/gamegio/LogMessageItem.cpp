//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/LogMessageItem.cpp

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "Fresco.hpp"

#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"
#include "LogMessageItem.hpp"
#include "LogMessageItemCbor.hpp"
#include "Graphics3DConfigCbor.hpp"
#include "Graphics3DSystem.hpp"
#include "Urho3D/IO/Log.h"

#include <Urho3D/Math/Vector3.h>
#include <Urho3D/Math/Quaternion.h>

using namespace std;

GIO_METHOD_FUNC(LogMessageItem, LogMessageItem)

// Factory Implementation
GCO_FACTORY_IMP(LogMessageItem)
    GCO_FACTORY_METHOD(LogMessageItem, ctLogMessage, LogMessageItem)
GCO_FACTORY_IMP_END

LogMessageItem::LogMessageItem()
{
}

FrItem LogMessageItem::msgCreate(FrMsg m, FrMsgLength l)
{
  LogMessageItem *lmi = new LogMessageItem();
  return (FrItem)lmi;
}

LogMessageItem::~LogMessageItem()
{
}

void LogMessageItem::msgDestroy()
{
    delete this;
}

void LogMessageItem::msgLogMessageItem(FrMsg m, FrMsgLength l) {
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::LogMessage logMessage;
  cbd::readLogMessage(&it, &logMessage);

  if (strlen(logMessage.message.c_str()) > 0) {
    int logLevel;
    switch(logMessage.level.selector) {
    case cbd::Debug : logLevel = LOG_DEBUG;
    case cbd::Info : logLevel = LOG_INFO;
    case cbd::Warning : logLevel = LOG_WARNING;
    }
    Urho3D::Log::Write(logLevel, logMessage.message.c_str());
  }
}

