//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/gamegio/src/GUIElements.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "GUIElements.hpp"

using namespace std;

// 
// HasUIElement
//

GIO_METHOD_FUNC(HasUIElement, ScreenRect)
GIO_METHOD_FUNC(HasUIElement, Alignment)
GIO_METHOD_FUNC(HasUIElement, Parent)
GIO_METHOD_FUNC(HasUIElement, EntityId)
GIO_METHOD_FUNC(HasUIElement, Name)
GIO_METHOD_FUNC(HasUIElement, Layout)
GIO_METHOD_FUNC(HasUIElement, MinSize)

GCO_FACTORY_IMP(HasUIElement)
    GCO_FACTORY_METHOD(HasUIElement, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(HasUIElement, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(HasUIElement, ctParent, Parent)
    GCO_FACTORY_METHOD(HasUIElement, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(HasUIElement, ctName, Name)
    GCO_FACTORY_METHOD(HasUIElement, ctLayout, Layout)
    GCO_FACTORY_METHOD(HasUIElement, ctMinSize, MinSize)
GCO_FACTORY_IMP_END

HasUIElement::HasUIElement()
{
    g = Graphics3DSystem::getG3DS();
}

HasUIElement::~HasUIElement()
{
    uiElement->Remove();
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

void HasUIElement::msgAlignment(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Alignment align;
  readAlignment(&it, &align);
  
  uiElement->SetHorizontalAlignment((Urho3D::HorizontalAlignment)align.horizontal.selector);
  uiElement->SetVerticalAlignment((Urho3D::VerticalAlignment)align.vertical.selector);
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

void HasUIElement::msgName(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Name name;
  cbd::readName(&it, &name);

  uiElement->SetName(name.c_str());
}

void HasUIElement::msgLayout(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Layout layout;
  cbd::readLayout(&it, &layout);

  IntRect rect;
  rect.top_ = layout.borders.top;
  rect.bottom_ = layout.borders.bottom;
  rect.left_ = layout.borders.left;
  rect.right_ = layout.borders.right;

  uiElement->SetLayout((LayoutMode)layout.mode.selector, layout.spacing, rect);
}

void HasUIElement::msgMinSize(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::MinSize ms;
  cbd::readMinSize(&it, &ms);

  uiElement->SetMinSize(ms.minWidth, ms.minHeight);
}

//
// ButtonItem
//

GIO_METHOD_FUNC(ButtonItem, ScreenRect)
GIO_METHOD_FUNC(ButtonItem, Alignment)
GIO_METHOD_FUNC(ButtonItem, Parent)
GIO_METHOD_FUNC(ButtonItem, EntityId)
GIO_METHOD_FUNC(ButtonItem, Name)
GIO_METHOD_FUNC(ButtonItem, Layout)
GIO_METHOD_FUNC(ButtonItem, MinSize)

GCO_FACTORY_IMP(ButtonItem)
    GCO_FACTORY_METHOD(ButtonItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(ButtonItem, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(ButtonItem, ctParent, Parent)
    GCO_FACTORY_METHOD(ButtonItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(ButtonItem, ctName, Name)
    GCO_FACTORY_METHOD(ButtonItem, ctLayout, Layout)
    GCO_FACTORY_METHOD(ButtonItem, ctMinSize, MinSize)
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
GIO_METHOD_FUNC(EditTextItem, Alignment)
GIO_METHOD_FUNC(EditTextItem, Parent)
GIO_METHOD_FUNC(EditTextItem, EntityId)
GIO_METHOD_FUNC(EditTextItem, EditText)
GIO_METHOD_FUNC(EditTextItem, Name)
GIO_METHOD_FUNC(EditTextItem, Layout)
GIO_METHOD_FUNC(EditTextItem, MinSize)

GCO_FACTORY_IMP(EditTextItem)
    GCO_FACTORY_METHOD(EditTextItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(EditTextItem, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(EditTextItem, ctParent, Parent)
    GCO_FACTORY_METHOD(EditTextItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(EditTextItem, ctEditText, EditText)
    GCO_FACTORY_METHOD(EditTextItem, ctName, Name)
    GCO_FACTORY_METHOD(EditTextItem, ctLayout, Layout)
    GCO_FACTORY_METHOD(EditTextItem, ctMinSize, MinSize)
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
GIO_METHOD_FUNC(TextItem, Alignment)
GIO_METHOD_FUNC(TextItem, Parent)
GIO_METHOD_FUNC(TextItem, EntityId)
GIO_METHOD_FUNC(TextItem, Text)
GIO_METHOD_FUNC(TextItem, Font)
GIO_METHOD_FUNC(TextItem, Colour)
GIO_METHOD_FUNC(TextItem, Name)
GIO_METHOD_FUNC(TextItem, Layout)
GIO_METHOD_FUNC(TextItem, MinSize)

GCO_FACTORY_IMP(TextItem)
    GCO_FACTORY_METHOD(TextItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(TextItem, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(TextItem, ctParent, Parent)
    GCO_FACTORY_METHOD(TextItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(TextItem, ctFont, Font)
    GCO_FACTORY_METHOD(TextItem, ctColour, Colour)
    GCO_FACTORY_METHOD(TextItem, ctStaticText, Text)
    GCO_FACTORY_METHOD(TextItem, ctName, Name)
    GCO_FACTORY_METHOD(TextItem, ctMinSize, MinSize)
GCO_FACTORY_IMP_END

TextItem::TextItem() : HasUIElement()
{
}

FrItem TextItem::msgCreate(FrMsg m, FrMsgLength l)
{
    TextItem *item = new TextItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->text = new Text(item->g->context);
    item->uiElement.StaticCast(item->text);
    ui->GetRoot()->AddChild(item->text);
    item->text->SetStyleAuto();
//    text->SetTextAlignment(HA_CENTER);
    return (FrItem)item;
}

void TextItem::msgDestroy()
{
    delete this;
}

TextItem::~TextItem()
{
}

void TextItem::msgText(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::StaticText st;
    cbd::readStaticText(&it, &st);
    text->SetText(st.c_str());
}

void TextItem::msgFont(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::Font f;
    cbd::readFont(&it, &f);

    text->SetFont(f.typeface.c_str());
    text->SetFontSize(f.size);
}

void TextItem::msgColour(FrMsg m, FrMsgLength l)
{
    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::Colour c;
    cbd::readColour(&it, &c);
    text->SetColor(Color(c.red, c.green, c.blue));    
}

//
// SliderItem
//

GIO_METHOD_FUNC(SliderItem, ScreenRect)
GIO_METHOD_FUNC(SliderItem, Alignment)
GIO_METHOD_FUNC(SliderItem, Parent)
GIO_METHOD_FUNC(SliderItem, EntityId)
GIO_METHOD_FUNC(SliderItem, Slider)
GIO_METHOD_FUNC(SliderItem, Name)
GIO_METHOD_FUNC(SliderItem, Layout)
GIO_METHOD_FUNC(SliderItem, MinSize)

GCO_FACTORY_IMP(SliderItem)
    GCO_FACTORY_METHOD(SliderItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(SliderItem, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(SliderItem, ctParent, Parent)
    GCO_FACTORY_METHOD(SliderItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(SliderItem, ctSlider, Slider)
    GCO_FACTORY_METHOD(SliderItem, ctName, Name)
    GCO_FACTORY_METHOD(SliderItem, ctLayout, Layout)
    GCO_FACTORY_METHOD(SliderItem, ctMinSize, MinSize)
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
GIO_METHOD_FUNC(CheckBoxItem, Alignment)
GIO_METHOD_FUNC(CheckBoxItem, Parent)
GIO_METHOD_FUNC(CheckBoxItem, EntityId)
GIO_METHOD_FUNC(CheckBoxItem, CheckBox)
GIO_METHOD_FUNC(CheckBoxItem, Name)
GIO_METHOD_FUNC(CheckBoxItem, Layout)
GIO_METHOD_FUNC(CheckBoxItem, MinSize)

GCO_FACTORY_IMP(CheckBoxItem)
    GCO_FACTORY_METHOD(CheckBoxItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(CheckBoxItem, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(CheckBoxItem, ctParent, Parent)
    GCO_FACTORY_METHOD(CheckBoxItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(CheckBoxItem, ctCheckBox, CheckBox)
    GCO_FACTORY_METHOD(CheckBoxItem, ctName, Name)
    GCO_FACTORY_METHOD(CheckBoxItem, ctLayout, Layout)
    GCO_FACTORY_METHOD(CheckBoxItem, ctMinSize, MinSize)
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


// 
// WindowGUI
//

GIO_METHOD_FUNC(WindowGUI, ScreenRect)
GIO_METHOD_FUNC(WindowGUI, Alignment)
GIO_METHOD_FUNC(WindowGUI, Parent)
GIO_METHOD_FUNC(WindowGUI, EntityId)
GIO_METHOD_FUNC(WindowGUI, Name)
GIO_METHOD_FUNC(WindowGUI, Layout)
GIO_METHOD_FUNC(WindowGUI, MinSize)

GCO_FACTORY_IMP(WindowGUI)
    GCO_FACTORY_METHOD(WindowGUI, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(WindowGUI, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(WindowGUI, ctParent, Parent)
    GCO_FACTORY_METHOD(WindowGUI, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(WindowGUI, ctName, Name)
    GCO_FACTORY_METHOD(WindowGUI, ctLayout, Layout)
    GCO_FACTORY_METHOD(WindowGUI, ctMinSize, MinSize)
GCO_FACTORY_IMP_END

WindowGUI::WindowGUI() : HasUIElement()
{
  std::cout << "WindowGUI con\n";
}

WindowGUI::~WindowGUI()
{
}

FrItem WindowGUI::msgCreate(FrMsg m, FrMsgLength l)
{
    WindowGUI *item = new WindowGUI();
    item->window = new Window(item->g->context);
    item->uiElement.StaticCast(item->window);
    UI* ui = item->g->context->GetSubsystem<UI>();
    ui->GetRoot()->AddChild(item->window);
    item->window->SetStyleAuto();
    return (FrItem)(item);
}

void WindowGUI::msgDestroy()
{
    delete this;
}


// 
// Tooltip
//

GIO_METHOD_FUNC(Tooltip, ScreenRect)
GIO_METHOD_FUNC(Tooltip, Alignment)
GIO_METHOD_FUNC(Tooltip, Parent)
GIO_METHOD_FUNC(Tooltip, EntityId)
GIO_METHOD_FUNC(Tooltip, Name)
GIO_METHOD_FUNC(Tooltip, Layout)
GIO_METHOD_FUNC(Tooltip, MinSize)
GIO_METHOD_FUNC(Tooltip, Tooltip)

GCO_FACTORY_IMP(Tooltip)
    GCO_FACTORY_METHOD(Tooltip, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(Tooltip, ctAlignment, Alignment)
    GCO_FACTORY_METHOD(Tooltip, ctParent, Parent)
    GCO_FACTORY_METHOD(Tooltip, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(Tooltip, ctName, Name)
    GCO_FACTORY_METHOD(Tooltip, ctLayout, Layout)
    GCO_FACTORY_METHOD(Tooltip, ctMinSize, MinSize)
    GCO_FACTORY_METHOD(Tooltip, ctTooltip, Tooltip)
GCO_FACTORY_IMP_END

Tooltip::Tooltip() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
}

Tooltip::~Tooltip()
{
}

FrItem Tooltip::msgCreate(FrMsg m, FrMsgLength l)
{
    Tooltip *item = new Tooltip();
    item->toolTip = new ToolTip(item->g->context);
    item->uiElement.StaticCast(item->toolTip);
    UI* ui = item->g->context->GetSubsystem<UI>();
    ui->GetRoot()->AddChild(item->toolTip);
    item->toolTip->SetStyleAuto();

    BorderImage* textHolder = new BorderImage(item->g->context);
    item->toolTip->AddChild(textHolder);
    textHolder->SetStyle("ToolTipBorderImage");
    item->toolTipText = new Text(item->g->context);
    textHolder->AddChild(item->toolTipText);
    item->toolTipText->SetStyle("ToolTipText");
    return (FrItem)item;
}

void Tooltip::msgDestroy()
{
    delete this;
}

void Tooltip::msgTooltip(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Tooltip tt;
  cbd::readTooltip(&it, &tt);
  
  toolTipText->SetText(tt.c_str());
}