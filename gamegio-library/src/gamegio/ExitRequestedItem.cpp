//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ExitRequestedItem.cpp

#include "ExitRequestedItem.hpp"
#include "WindowEventCbor.hpp"
#include "Graphics3DSystem.hpp"

using namespace std;
using namespace cbd;

GCO_FACTORY_IMP(ExitRequestedItem)
GCO_FACTORY_IMP_END

ExitRequestedItem::ExitRequestedItem()
: Object(Graphics3DSystem::getG3DS()->context)
{
}

ExitRequestedItem::~ExitRequestedItem()
{
  // deregister all events by following procedure
  exitREventF = NULL;
  UnsubscribeFromAllEvents();
}

FrItem ExitRequestedItem::msgCreate(FrMsg m, FrMsgLength l)
{
    return new ExitRequestedItem();
}

void ExitRequestedItem::msgDestroy()
{
    delete this;
}

void ExitRequestedItem::HandleExitRequestedEvent(StringHash eventType, VariantMap& eventData)
{
  if (exitREventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    ExitRequestedEvent er;

    writeExitRequestedEvent(&encoder, er);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    exitREventF(exitRDataP, exitREventType, buf, len);
  }
}

void ExitRequestedItem::registerExitRequestedEventFunction(FrMessageFn2 f, void* p2, uint64_t erET)
{
  exitREventF = f;
  exitRDataP = p2;
  exitREventType = erET;
  SubscribeToEvent(E_EXITREQUESTED, URHO3D_HANDLER(ExitRequestedItem, HandleExitRequestedEvent));
}

