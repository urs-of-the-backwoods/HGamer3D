//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/KeysItem.cpp

#include "KeysItem.hpp"

#include "Graphics3DSystem.hpp"
#include "KeyEventCbor.hpp"

using namespace std;
using namespace cbd;

GCO_FACTORY_IMP(KeysItem)
GCO_FACTORY_IMP_END

KeysItem::KeysItem()
: Object(Graphics3DSystem::getG3DS()->context)
{
    input =  Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();
    keyEventF = NULL;
}

KeysItem::~KeysItem()
{
  UnsubscribeFromAllEvents();
}

FrItem KeysItem::msgCreate(FrMsg m, FrMsgLength l)
{
  return new KeysItem();
}

void KeysItem::msgDestroy()
{
    delete this;
}

void KeysItem::HandleKeyUp(StringHash eventType, VariantMap& eventData)
{
	if (keyEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    KeyEvent kevt;

    kevt.selector = cbd::KeyUpEvent;
    int scancode = eventData[KeyUp::P_SCANCODE].GetInt();
    kevt.data.KeyUpEvent.value0.key = eventData[KeyUp::P_KEY].GetInt();
    kevt.data.KeyUpEvent.value0.scancode = scancode;
    kevt.data.KeyUpEvent.value0.name = std::string(input->GetScancodeName(scancode).CString());

    writeKeyEvent(&encoder, kevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    keyEventF(keyDataP, keyEventType, buf, len);
	}
}

void KeysItem::HandleKeyDown(StringHash eventType, VariantMap& eventData)
{
  if (keyEventF != NULL) {
        if (!eventData[KeyDown::P_REPEAT].GetBool()) {
          uint8_t buf[64];
          CborEncoder encoder;
          cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

          KeyEvent kevt;

          kevt.selector = cbd::KeyDownEvent;
          int scancode = eventData[KeyDown::P_SCANCODE].GetInt();
          kevt.data.KeyDownEvent.value0.key = eventData[KeyDown::P_KEY].GetInt();
          kevt.data.KeyDownEvent.value0.scancode = scancode;
          kevt.data.KeyDownEvent.value0.name = std::string(input->GetScancodeName(scancode).CString());

          writeKeyEvent(&encoder, kevt);
          size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
          keyEventF(keyDataP, keyEventType, buf, len);
        }
  }
}

void KeysItem::registerKeyEventFunction(FrMessageFn2 f, void* p2, uint64_t keyET)
{
    keyEventF = f;
    keyDataP = p2;
    keyEventType = keyET;

    SubscribeToEvent(E_KEYUP, URHO3D_HANDLER(KeysItem, HandleKeyUp));
    SubscribeToEvent(E_KEYDOWN, URHO3D_HANDLER(KeysItem, HandleKeyDown));
}

