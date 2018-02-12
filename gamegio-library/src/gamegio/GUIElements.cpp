//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/GUIElements.cpp

#include "GUIElements.hpp"

#include "ScreenRectCbor.hpp"
#include "ParentCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ButtonCbor.hpp"
#include "CheckBoxCbor.hpp"
#include "DropDownListCbor.hpp"
#include "EditTextCbor.hpp"
#include "SliderCbor.hpp"
#include "StaticTextCbor.hpp"
#include "UIElementCbor.hpp"
#include "SpriteItemCbor.hpp"

using namespace std;

//
// HasUIElement
//

GIO_METHOD_FUNC(HasUIElement, ScreenRect)
GIO_METHOD_FUNC(HasUIElement, Parent)
GIO_METHOD_FUNC(HasUIElement, EntityId)

GCO_FACTORY_IMP(HasUIElement)
    GCO_FACTORY_METHOD(HasUIElement, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(HasUIElement, ctParent, Parent)
    GCO_FACTORY_METHOD(HasUIElement, ctEntityId, EntityId)
GCO_FACTORY_IMP_END

HasUIElement::HasUIElement()
{
    g = Graphics3DSystem::getG3DS();
}

HasUIElement::~HasUIElement()
{
}

FrItem HasUIElement::msgCreate(FrMsg m, FrMsgLength l)
{
    return (FrItem)(new HasUIElement());
}

void HasUIElement::msgDestroy()
{
    delete this;
}

void HasUIElement::msgScreenRect(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::ScreenRect rect;
  readScreenRect(&it, &rect);
  
  uiElement->SetPosition(rect.x,rect.y);
  uiElement->SetSize(rect.width, rect.height);
}

void HasUIElement::msgParent(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::EntityId eid;
  cbd::readEntityId(&it, &eid);

  // add to map
  UIElement* parent = g->ui_map[eid];
  uiElement->SetParent(parent);
}

void HasUIElement::msgEntityId(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::EntityId eid;
  cbd::readEntityId(&it, &eid);

  g->ui_map[eid] = uiElement;
}

//
// SpriteItem
//

GIO_METHOD_FUNC(SpriteItem, ScreenRect)
GIO_METHOD_FUNC(SpriteItem, Parent)
GIO_METHOD_FUNC(SpriteItem, EntityId)

GCO_FACTORY_IMP(SpriteItem)
    GCO_FACTORY_METHOD(SpriteItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(SpriteItem, ctParent, Parent)
    GCO_FACTORY_METHOD(SpriteItem, ctEntityId, EntityId)
GCO_FACTORY_IMP_END

SpriteItem::SpriteItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
}

FrItem SpriteItem::msgCreate(FrMsg m, FrMsgLength l)
{
    SpriteItem *item = new SpriteItem();

    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::Sprite sp;
    cbd::readSprite(&it, &sp);

    ResourceCache* cache = item->g->context->GetSubsystem<ResourceCache>();
    Texture2D* texture = cache->GetResource<Texture2D>(sp.resource.c_str());

    UI* ui = item->g->context->GetSubsystem<UI>();
    item->sprite = new Sprite(item->g->context);
    item->uiElement.StaticCast(item->sprite);
    ui->GetRoot()->AddChild(item->sprite);
    item->sprite->SetStyleAuto();
    item->sprite->SetTexture(texture);
    item->sprite->SetOpacity(sp.opacity);
    return (FrItem)item;
}

void SpriteItem::msgDestroy()
{
    delete this;
}

SpriteItem::~SpriteItem()
{
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(sprite);
}


//
// ButtonItem
//

GIO_METHOD_FUNC(ButtonItem, ScreenRect)
GIO_METHOD_FUNC(ButtonItem, Parent)
GIO_METHOD_FUNC(ButtonItem, EntityId)

GCO_FACTORY_IMP(ButtonItem)
    GCO_FACTORY_METHOD(ButtonItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(ButtonItem, ctParent, Parent)
    GCO_FACTORY_METHOD(ButtonItem, ctEntityId, EntityId)
GCO_FACTORY_IMP_END

ButtonItem::ButtonItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
    callbackF = NULL;
}

FrItem ButtonItem::msgCreate(FrMsg m, FrMsgLength l)
{
    ButtonItem *item = new ButtonItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->button = new Button(item->g->context);
    item->uiElement.StaticCast(item->button);
    ui->GetRoot()->AddChild(item->button);
    item->button->SetStyleAuto();

    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::Button bi;
    cbd::readButton(&it, &bi);

    String label = bi.label.c_str();

    // add label to button
    item->text = new Text(item->g->context);
    item->button->AddChild(item->text);
    item->text->SetStyle("Text");
    item->text->SetHorizontalAlignment(HA_CENTER);
    item->text->SetVerticalAlignment(VA_CENTER);
    item->text->SetText(label);
 
    item->registerEvents();
    return (FrItem)item;
}

void ButtonItem::registerEvents()
{
    SubscribeToEvent(uiElement, E_PRESSED, URHO3D_HANDLER(ButtonItem, HandlePressedReleasedChanged));
    SubscribeToEvent(uiElement, E_RELEASED, URHO3D_HANDLER(ButtonItem, HandlePressedReleasedChanged));
}

void ButtonItem::msgDestroy()
{
    delete this;
}

ButtonItem::~ButtonItem()
{
    UnsubscribeFromEvent(uiElement, E_PRESSED);
    UnsubscribeFromEvent(uiElement, E_RELEASED);
    UI* ui = g->context->GetSubsystem<UI>();
    button->RemoveChild(text);
    ui->GetRoot()->RemoveChild(button);
}

void ButtonItem::registerButtonFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    callbackF = f;
    callbackData = p2;
    cbEventType = cbet;
}

void ButtonItem::HandlePressedReleasedChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        // only two events are registered, both change pressed status
        uint8_t buf[64];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

        cbd::Button bevt;
        bevt.pressed = button->IsPressed();
        bevt.label = text->GetText().CString();

        cbd::writeButton(&encoder, bevt);
        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        callbackF(callbackData, cbEventType, buf, len);
    }
}

//
// EditTextItem
//

GIO_METHOD_FUNC(EditTextItem, ScreenRect)
GIO_METHOD_FUNC(EditTextItem, Parent)
GIO_METHOD_FUNC(EditTextItem, EntityId)
GIO_METHOD_FUNC(EditTextItem, EditText)

GCO_FACTORY_IMP(EditTextItem)
    GCO_FACTORY_METHOD(EditTextItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(EditTextItem, ctParent, Parent)
    GCO_FACTORY_METHOD(EditTextItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(EditTextItem, ctEditText, EditText)
GCO_FACTORY_IMP_END

EditTextItem::EditTextItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)

{
    callbackF = NULL;
    callbackData = NULL;
}

FrItem EditTextItem::msgCreate(FrMsg m, FrMsgLength l)
{
    EditTextItem *item = new EditTextItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->edittext = new LineEdit2(item->g->context);
    item->uiElement.StaticCast(item->edittext);
    ui->GetRoot()->AddChild(item->edittext);
    item->edittext->SetStyleAuto();
    item->edittext->GetTextElement()->SetVerticalAlignment(VA_CENTER);
    item->edittext->GetCursor()->SetVerticalAlignment(VA_CENTER);
    item->registerEvents();
    return (FrItem)item;
}

void EditTextItem::registerEvents()
{
    SubscribeToEvent(uiElement, E_TEXTCHANGED, URHO3D_HANDLER(EditTextItem, HandleTextChanged));
}

void EditTextItem::msgDestroy()
{
    delete this;
}

EditTextItem::~EditTextItem()
{
    UnsubscribeFromEvent(uiElement, E_TEXTCHANGED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(edittext);
}

void EditTextItem::msgEditText(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::EditText et;
    cbd::readEditText(&it, &et);
    edittext->SetText(et.c_str());
}

void EditTextItem::registerEditTextFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    callbackF = f;
    callbackData = p2;
    cbEventType = cbet;
}

void EditTextItem::HandleTextChanged (StringHash eventType, VariantMap& eventData)
{
    String str = eventData[TextChanged::P_TEXT].GetString();
    if (callbackF != NULL) {
        uint8_t buf[64];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

        cbd::EditText etevt;
        etevt = str.CString();;
        cbd::writeEditText(&encoder, etevt);

        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        callbackF(callbackData, cbEventType, buf, len);
    }
}

//
// TextItem
//

GIO_METHOD_FUNC(TextItem, ScreenRect)
GIO_METHOD_FUNC(TextItem, Parent)
GIO_METHOD_FUNC(TextItem, EntityId)
GIO_METHOD_FUNC(TextItem, Text)

GCO_FACTORY_IMP(TextItem)
    GCO_FACTORY_METHOD(TextItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(TextItem, ctParent, Parent)
    GCO_FACTORY_METHOD(TextItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(TextItem, ctStaticText, Text)
GCO_FACTORY_IMP_END

TextItem::TextItem() : HasUIElement()
{
}

FrItem TextItem::msgCreate(FrMsg m, FrMsgLength l)
{
    TextItem *item = new TextItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->text = item->g->context->CreateObject<Text>();
    //new Text(item->g->context);
    item->uiElement.StaticCast(item->text);
    ui->GetRoot()->AddChild(item->text);
    item->text->SetStyleAuto();
    return (FrItem)item;
}

void TextItem::msgDestroy()
{
    delete this;
}

TextItem::~TextItem()
{
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(text);
}

void TextItem::msgText(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::StaticText st;
  cbd::readStaticText(&it, &st);
  text->SetText(st.c_str());
}

//
// SliderItem
//

GIO_METHOD_FUNC(SliderItem, ScreenRect)
GIO_METHOD_FUNC(SliderItem, Parent)
GIO_METHOD_FUNC(SliderItem, EntityId)
GIO_METHOD_FUNC(SliderItem, Slider)

GCO_FACTORY_IMP(SliderItem)
    GCO_FACTORY_METHOD(SliderItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(SliderItem, ctParent, Parent)
    GCO_FACTORY_METHOD(SliderItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(SliderItem, ctSlider, Slider)
GCO_FACTORY_IMP_END

SliderItem::SliderItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

FrItem SliderItem::msgCreate(FrMsg m, FrMsgLength l)
{
    SliderItem *item = new SliderItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->slider = new Slider2(item->g->context);
    item->uiElement.StaticCast(item->slider);
    ui->GetRoot()->AddChild(item->slider);
    item->slider->SetStyleAuto();
    item->registerEvents();
    return (FrItem)item;
}

void SliderItem::registerEvents()
{
    SubscribeToEvent(uiElement, E_SLIDERCHANGED, URHO3D_HANDLER(SliderItem, HandleSliderChanged));
}

void SliderItem::msgDestroy()
{
    delete this;
}

SliderItem::~SliderItem()
{
    UnsubscribeFromEvent(uiElement, E_SLIDERCHANGED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(slider);
}

void SliderItem::msgSlider(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::Slider sl;
    cbd::readSlider(&it, &sl);

    slider->SetRange(sl.range);
    slider->SetValue(sl.value);
}

void SliderItem::registerSliderFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    callbackF = f;
    callbackData = p2;
    cbEventType = cbet;
}

void SliderItem::HandleSliderChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        uint8_t buf[64];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

        cbd::Slider slevt;
        slevt.range = slider->GetRange();
        slevt.value = slider->GetValue();
        cbd::writeSlider(&encoder, slevt);

        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        callbackF(callbackData, cbEventType, buf, len);
    }
}


//
// CheckBoxItem
//

GIO_METHOD_FUNC(CheckBoxItem, ScreenRect)
GIO_METHOD_FUNC(CheckBoxItem, Parent)
GIO_METHOD_FUNC(CheckBoxItem, EntityId)
GIO_METHOD_FUNC(CheckBoxItem, CheckBox)

GCO_FACTORY_IMP(CheckBoxItem)
    GCO_FACTORY_METHOD(CheckBoxItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(CheckBoxItem, ctParent, Parent)
    GCO_FACTORY_METHOD(CheckBoxItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(CheckBoxItem, ctCheckBox, CheckBox)
GCO_FACTORY_IMP_END

CheckBoxItem::CheckBoxItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

FrItem CheckBoxItem::msgCreate(FrMsg m, FrMsgLength l)
{
    CheckBoxItem* item = new CheckBoxItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->checkbox = new CheckBox(item->g->context);
    item->uiElement.StaticCast(item->checkbox);
    ui->GetRoot()->AddChild(item->checkbox);
    item->checkbox->SetStyleAuto();
    item->registerEvents();
    return (FrItem)item;
}

void CheckBoxItem::registerEvents()
{
    SubscribeToEvent(uiElement, E_TOGGLED, URHO3D_HANDLER(CheckBoxItem, HandleToggled));
}

void CheckBoxItem::msgDestroy()
{
    delete this;
}

CheckBoxItem::~CheckBoxItem()
{
    UnsubscribeFromEvent(uiElement, E_TOGGLED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(checkbox);
}

void CheckBoxItem::msgCheckBox(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::CheckBox cb;
    cbd::readCheckBox(&it, &cb);

    checkbox->SetChecked(cb);
}

void CheckBoxItem::registerCheckBoxFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    callbackF = f;
    callbackData = p2;
    cbEventType = cbet;
}

void CheckBoxItem::HandleToggled(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        uint8_t buf[64];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

        cbd::CheckBox cbevt;
        cbevt = checkbox->IsChecked();
        cbd::writeCheckBox(&encoder, cbevt);

        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        callbackF(callbackData, cbEventType, buf, len);
    }
}


