//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/ScreenModeItem.cpp

#include "ScreenModeItem.hpp"

#include "Graphics3DSystem.hpp"
#include "WindowEventCbor.hpp"

using namespace std;
using namespace cbd;

GCO_FACTORY_IMP(ScreenModeItem)
GCO_FACTORY_IMP_END

ScreenModeItem::ScreenModeItem()
: Object(Graphics3DSystem::getG3DS()->context)
{
}

ScreenModeItem::~ScreenModeItem()
{
  UnsubscribeFromAllEvents();
}

FrItem ScreenModeItem::msgCreate(FrMsg m, FrMsgLength l)
{
  return new ScreenModeItem();
}

void ScreenModeItem::msgDestroy()
{
    delete this;
}

void ScreenModeItem::HandleScreenModeEvent(StringHash eventType, VariantMap& eventData)
{
  if (SMEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::ScreenModeEvent sme;
    sme.Width = eventData[ScreenMode::P_WIDTH].GetInt();
    sme.Height = eventData[ScreenMode::P_HEIGHT].GetInt();
    sme.Fullscreen = eventData[ScreenMode::P_FULLSCREEN].GetBool();
    sme.Borderless = eventData[ScreenMode::P_BORDERLESS].GetBool();

    writeScreenModeEvent(&encoder, sme);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    SMEventF(SMDataP, SMEventType, buf, len);
  }
}

void ScreenModeItem::registerScreenModeEventFunction(FrMessageFn2 f, void* p2, uint64_t erET)
{
  SMEventF = f;
  SMDataP = p2;
  SMEventType = erET;
  SubscribeToEvent(E_SCREENMODE, URHO3D_HANDLER(ScreenModeItem, HandleScreenModeEvent));
}
