//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/MouseItem.cpp

#include "MouseItem.hpp"

#include "Graphics3DSystem.hpp"
#include "VisibleCbor.hpp"
#include "MouseCbor.hpp"

using namespace std;
using namespace cbd;

GIO_METHOD_FUNC(MouseItem, MouseMode)
GIO_METHOD_FUNC(MouseItem, Visible)

GCO_FACTORY_IMP(MouseItem)
  GCO_FACTORY_METHOD(MouseItem, ctMouse, MouseMode)
  GCO_FACTORY_METHOD(MouseItem, ctVisible, Visible)
GCO_FACTORY_IMP_END

MouseItem::MouseItem()
: Object(Graphics3DSystem::getG3DS()->context)
{
    input =  Graphics3DSystem::getG3DS()->context->GetSubsystem<Input>();
    mouseEventF = NULL;
}

MouseItem::~MouseItem()
{
  UnsubscribeFromAllEvents();
}

FrItem MouseItem::msgCreate(FrMsg m, FrMsgLength l)
{
  return new MouseItem();
}

void MouseItem::msgDestroy()
{
    delete this;
}

void MouseItem::msgMouseMode(FrMsg m, FrMsgLength l)
{
  // read message data
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::MouseMode mm;
  readMouseMode(&it, &mm);

  // set mouse mode
  if (mm.selector == cbd::Absolute)
  {
      input->SetMouseMode(MM_ABSOLUTE);
  }
  else if (mm.selector ==  cbd::Relative)
  {
      input->SetMouseMode(MM_RELATIVE);
  }
  else if (mm.selector == cbd::Wrap)
  {
      input->SetMouseMode(MM_WRAP);
  }
}

void MouseItem::msgVisible(FrMsg m, FrMsgLength l)
{
  // read message data
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  Visible visible;
  readVisible(&it, &visible);

  input->SetMouseVisible(visible);       // true -> suppress event
}

void MouseItem::HandleMouseButtonUp(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonUpEvent;
    mevt.data.MouseButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MouseButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MouseButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void MouseItem::HandleMouseButtonDown(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseButtonDownEvent;
    mevt.data.MouseButtonUpEvent.value0.button = eventData[MouseButtonUp::P_BUTTON].GetInt();
    mevt.data.MouseButtonUpEvent.value0.buttons = eventData[MouseButtonUp::P_BUTTONS].GetInt();
    mevt.data.MouseButtonUpEvent.value0.qualifiers = eventData[MouseButtonUp::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void MouseItem::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseMoveEvent;
    if (input->IsMouseVisible()) {
      mevt.data.MouseMoveEvent.value0.x = eventData[MouseMove::P_X].GetInt();
      mevt.data.MouseMoveEvent.value0.y = eventData[MouseMove::P_Y].GetInt();
    } else {
      mevt.data.MouseMoveEvent.value0.x = 0;
      mevt.data.MouseMoveEvent.value0.y = 0;
    }
    mevt.data.MouseMoveEvent.value0.dx = eventData[MouseMove::P_DX].GetInt();
    mevt.data.MouseMoveEvent.value0.dy = eventData[MouseMove::P_DY].GetInt();
    mevt.data.MouseMoveEvent.value0.buttons = eventData[MouseMove::P_BUTTONS].GetInt();
    mevt.data.MouseMoveEvent.value0.qualifiers = eventData[MouseMove::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void MouseItem::HandleMouseWheel(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

    cbd::MouseEvent mevt;

    mevt.selector = cbd::MouseWheelEvent;
    mevt.data.MouseWheelEvent.value0.wheel = eventData[MouseWheel::P_WHEEL].GetInt();
    mevt.data.MouseWheelEvent.value0.buttons = eventData[MouseWheel::P_BUTTONS].GetInt();
    mevt.data.MouseWheelEvent.value0.qualifiers = eventData[MouseWheel::P_QUALIFIERS].GetInt();

    writeMouseEvent(&encoder, mevt);
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    mouseEventF(mouseDataP, mouseEventType, buf, len);
	}
}

void MouseItem::HandleMouseVisibleChanged(StringHash eventType, VariantMap& eventData)
{
	if (mouseEventF != NULL) {
    // we should connect this to visible flag
	}
}

void MouseItem::registerMouseEventFunction(FrMessageFn2 f, void* p2, uint64_t mouseET)
{
    mouseEventF = f;
    mouseDataP = p2;
    mouseEventType = mouseET;

    SubscribeToEvent(E_MOUSEBUTTONUP, URHO3D_HANDLER(MouseItem, HandleMouseButtonUp));
    SubscribeToEvent(E_MOUSEBUTTONDOWN, URHO3D_HANDLER(MouseItem, HandleMouseButtonDown));
    SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(MouseItem, HandleMouseMove));
    SubscribeToEvent(E_MOUSEWHEEL, URHO3D_HANDLER(MouseItem, HandleMouseWheel));
    SubscribeToEvent(E_MOUSEVISIBLECHANGED, URHO3D_HANDLER(MouseItem, HandleMouseVisibleChanged));
}
