//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/gui.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#include "gui.hpp"

using namespace std;

extern "C" {
#include "interface.h"
}


// 
// HasUIElement
//

HasUIElement::HasUIElement(Graphics3DSystem*g3ds)
{
    g = g3ds;
}

HasUIElement::~HasUIElement()
{
    delete uiElement;
}

int HasUIElement::msgScreenRect(char* pdata, int len)
{
  msgpack::unpacked msg;
  msgpack::unpack(&msg, pdata, len);
  msgpack::object obj = msg.get();
  
  if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 4) return ERROR_TYPE_NOT_KNOWN;
  
  // set properties of screenrect
  int x = obj.via.array.ptr[0].as<int>();
  int y = obj.via.array.ptr[1].as<int>();
  int width = obj.via.array.ptr[2].as<int>();
  int height = obj.via.array.ptr[3].as<int>();
  
  uiElement->SetPosition(x,y);
  uiElement->SetSize(width, height);
  return 0;
}

//
// ButtonItem
//

ButtonItem::ButtonItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds), Object(g3ds->context)
{
    callbackF = NULL;
}

int ButtonItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    button = new Button(g->context);
    uiElement.StaticCast(button);
    SubscribeToEvent(uiElement, E_PRESSED, HANDLER(ButtonItem, HandlePressedReleasedChanged));
    SubscribeToEvent(uiElement, E_RELEASED, HANDLER(ButtonItem, HandlePressedReleasedChanged));
    ui->GetRoot()->AddChild(button);
    button->SetStyleAuto();
    return 0;
}

ButtonItem::~ButtonItem()
{
    UnsubscribeFromEvent(uiElement, E_PRESSED);
    UnsubscribeFromEvent(uiElement, E_RELEASED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(button);
    delete button;
}

void ButtonItem::registerPressedReleasedFunction(msgFP2 f, void* p2)
{
    callbackF = f;
    callbackData = p2;
}

void ButtonItem::HandlePressedReleasedChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        // only two events are registered, both change pressed status
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack(button->IsPressed());
        callbackF((void*)this, callbackData, buffer.data(), buffer.size());
    }
}

//
// EditTextItem
//

EditTextItem::EditTextItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds), Object(g3ds->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

int EditTextItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    edittext = new LineEdit(g->context);
    uiElement.StaticCast(edittext);
    SubscribeToEvent(uiElement, E_TEXTCHANGED, HANDLER(EditTextItem, HandleTextChanged));
    ui->GetRoot()->AddChild(edittext);
    edittext->SetStyleAuto();
    return 0;
}

EditTextItem::~EditTextItem()
{
    UnsubscribeFromEvent(uiElement, E_TEXTCHANGED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(edittext);
    delete edittext;
}

int EditTextItem::msgEditText(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
    std::string str;
    obj.convert(str);
    edittext->SetText(str.c_str());
    return 0;
}

void EditTextItem::registerTextEventFunction(msgFP2 f, void* p2)
{
    callbackF = f;
    callbackData = p2;
}

void EditTextItem::HandleTextChanged (StringHash eventType, VariantMap& eventData)
{
    String str = eventData[TextChanged::P_TEXT].GetString();
    if (callbackF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack(str.CString());
        callbackF((void*)this, callbackData, buffer.data(), buffer.size());
    }
}

//
// TextItem
//

TextItem::TextItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds)
{
}

int TextItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    text = new Text(g->context);
    uiElement.StaticCast(text);
    ui->GetRoot()->AddChild(text);
    text->SetStyleAuto();
    return 0;
}

TextItem::~TextItem()
{
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(text);
    delete text;
}

int TextItem::msgText(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
    std::string str;
    obj.convert(str);
    text->SetText(str.c_str());
    return 0;
}

//
// SliderItem
//

SliderItem::SliderItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds), Object(g3ds->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

int SliderItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    slider = new Slider(g->context);
    uiElement.StaticCast(slider);
    SubscribeToEvent(uiElement, E_SLIDERCHANGED, HANDLER(SliderItem, HandleSliderChanged));
    ui->GetRoot()->AddChild(slider);
    slider->SetStyleAuto();
    return 0;
}

SliderItem::~SliderItem()
{
    UnsubscribeFromEvent(uiElement, E_SLIDERCHANGED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(slider);
    delete slider;
}

int SliderItem::msgSlider(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "msgSlider: " << obj << std::endl;
    if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;
    slider->SetRange(obj.via.array.ptr[0].as<float>());
    slider->SetValue(obj.via.array.ptr[1].as<float>());
    return 0;
}

void SliderItem::registerSliderEventFunction(msgFP2 f, void* p2)
{
    callbackF = f;
    callbackData = p2;
}

void SliderItem::HandleSliderChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
		pk.pack_array(2);
        pk.pack(slider->GetRange());
        pk.pack(slider->GetValue());
        callbackF((void*)this, callbackData, buffer.data(), buffer.size());
    }
}


//
// CheckBoxItem
//

CheckBoxItem::CheckBoxItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds), Object(g3ds->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

int CheckBoxItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    checkbox = new CheckBox(g->context);
    uiElement.StaticCast(checkbox);
    SubscribeToEvent(uiElement, E_TOGGLED, HANDLER(CheckBoxItem, HandleToggled));
    ui->GetRoot()->AddChild(checkbox);
    checkbox->SetStyleAuto();
    return 0;
}

CheckBoxItem::~CheckBoxItem()
{
    UnsubscribeFromEvent(uiElement, E_TOGGLED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(checkbox);
    delete checkbox;
}

int CheckBoxItem::msgCheckBox(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "msgCheckBox: " << obj << std::endl;
    if (obj.type != msgpack::type::BOOLEAN) return ERROR_TYPE_NOT_KNOWN;
    checkbox->SetChecked(obj.as<bool>());
    return 0;
}

void CheckBoxItem::registerToggledEventFunction(msgFP2 f, void* p2)
{
    callbackF = f;
    callbackData = p2;
}

void CheckBoxItem::HandleToggled(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack(checkbox->IsChecked());
        callbackF((void*)this, callbackData, buffer.data(), buffer.size());
    }
}


//
// DropDownListItem
//

DropDownListItem::DropDownListItem(Graphics3DSystem*g3ds) : HasUIElement(g3ds), Object(g3ds->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

int DropDownListItem::create(char* pdata, int len)
{
    UI* ui = g->context->GetSubsystem<UI>();
    dropdownlist = new DropDownList(g->context);
    uiElement.StaticCast(dropdownlist);
    SubscribeToEvent(uiElement, E_ITEMSELECTED, HANDLER(DropDownListItem, HandleSelectionChanged));
    ui->GetRoot()->AddChild(dropdownlist);
    dropdownlist->SetStyleAuto();
    return 0;
}

DropDownListItem::~DropDownListItem()
{
    UnsubscribeFromEvent(uiElement, E_ITEMSELECTED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(dropdownlist);
    delete dropdownlist;
}

int DropDownListItem::msgDropDownList(char* pdata, int len)
{
    msgpack::unpacked msg;
    msgpack::unpack(&msg, pdata, len);
    msgpack::object obj = msg.get();
//    std::cout << "msgDropDownList: " << obj << std::endl;
    if (obj.type != msgpack::type::ARRAY || obj.via.array.size != 2) return ERROR_TYPE_NOT_KNOWN;
    // remove all items
    dropdownlist->RemoveAllItems();
    // create list of text items
    msgpack::object texts_o = obj.via.array.ptr[0];
    std::vector<std::string> texts;
    texts_o.convert(&texts);
    for(std::vector<std::string>::iterator it = texts.begin(); it != texts.end(); ++it) {
        Text* t = new Text(g->context);
        t->SetText(it->c_str());
        t->SetStyleAuto();
        dropdownlist->AddItem(t);
    }
    // create selection from maybe list
    msgpack::object sel_o = obj.via.array.ptr[1];
    if (sel_o.via.array.ptr[0].as<int>() == 0) {
        dropdownlist->SetSelection(-1);
    } else {
        dropdownlist->SetSelection(sel_o.via.array.ptr[1].as<int>());
    }
    
    return 0;
}

void DropDownListItem::registerSelectionEventFunction(msgFP2 f, void* p2)
{
    callbackF = f;
    callbackData = p2;
}

void DropDownListItem::HandleSelectionChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {
        msgpack::sbuffer buffer;
        msgpack::packer<msgpack::sbuffer> pk(&buffer);
        pk.pack_array(2);
        int len = dropdownlist->GetNumItems();
        pk.pack_array(len);
        for (int i = 0; i < len; i++) {
            pk.pack(((Text*)dropdownlist->GetItem(i))->GetText().CString());
        }
        pk.pack_array(2);
        pk.pack(1);
        int s = eventData[ItemSelected::P_SELECTION].GetInt();
        pk.pack(s);
        callbackF((void*)this, callbackData, buffer.data(), buffer.size());
    }
}


