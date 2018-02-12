//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/GUIElements.hpp

#ifndef __gui_elements_hpp__
#define __gui_elements_hpp__

#include "Urho3D/Urho3DAll.h"
#include "Graphics3DSystem.hpp"
#include "Fresco.hpp"
#include "Slider2.hpp"
#include "LineEdit2.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(HasUIElement, ScreenRect)
GIO_METHOD_DEC(HasUIElement, Parent)
GIO_METHOD_DEC(HasUIElement, EntityId)
GCO_FACTORY_DEC(HasUIElement)

class HasUIElement {

protected:
  SharedPtr<UIElement> uiElement;
  Graphics3DSystem* g;
  cbd::EntityId myId;

public:
  HasUIElement();
  ~HasUIElement();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();

  void msgScreenRect(FrMsg m, FrMsgLength l);
  void msgParent(FrMsg m, FrMsgLength l);
  void msgEntityId(FrMsg m, FrMsgLength l);
};

//
// SPRITE
//

GCO_FACTORY_DEC(SpriteItem)

class SpriteItem : public HasUIElement, public Object {

  URHO3D_OBJECT(SpriteItem, Object);

protected:
  SharedPtr<Sprite> sprite;

public:
  SpriteItem();
  ~SpriteItem();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
};

//
// BUTTON
//

GIO_METHOD_DEC(ButtonItem, ScreenRect)
GIO_METHOD_DEC(ButtonItem, Parent)
GIO_METHOD_DEC(ButtonItem, EntityId)
GCO_FACTORY_DEC(ButtonItem)

class ButtonItem : public HasUIElement, public Object {

URHO3D_OBJECT(ButtonItem, Object);

protected:
    SharedPtr<Button> button;
    SharedPtr<Text> text;

    FrMessageFn2 callbackF;
    void* callbackData;
    uint64_t cbEventType;
    
public:
  ButtonItem();
  ~ButtonItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void registerEvents();
  void registerButtonFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandlePressedReleasedChanged(StringHash eventType, VariantMap& eventData);
};

GIO_METHOD_DEC(EditTextItem, ScreenRect)
GIO_METHOD_DEC(EditTextItem, Parent)
GIO_METHOD_DEC(EditTextItem, EntityId)
GIO_METHOD_DEC(EditTextItem, EditText)
GCO_FACTORY_DEC(EditTextItem)

class EditTextItem : public HasUIElement, public Object {

URHO3D_OBJECT(EditTextItem, Object);

protected:
    SharedPtr<LineEdit2> edittext;
    FrMessageFn2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  EditTextItem();
  ~EditTextItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgEditText(FrMsg m, FrMsgLength l);
  
  void registerEvents();
  void registerEditTextFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleTextChanged(StringHash eventType, VariantMap& eventData);
};

GIO_METHOD_DEC(TextItem, ScreenRect)
GIO_METHOD_DEC(TextItem, Parent)
GIO_METHOD_DEC(TextItem, EntityId)
GIO_METHOD_DEC(TextItem, Text)
GCO_FACTORY_DEC(TextItem)

class TextItem : public HasUIElement {

protected:
    SharedPtr<Text> text;
    Urho3D::String contents;

public:
  TextItem();
  ~TextItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgText(FrMsg m, FrMsgLength l);
};


GIO_METHOD_DEC(SliderItem, ScreenRect)
GIO_METHOD_DEC(SliderItem, Parent)
GIO_METHOD_DEC(SliderItem, EntityId)
GIO_METHOD_DEC(SliderItem, Slider)
GCO_FACTORY_DEC(SliderItem)

class SliderItem : public HasUIElement, public Object  {

URHO3D_OBJECT(SliderItem, Object);

protected:
    SharedPtr<Slider2> slider;
    FrMessageFn2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  SliderItem();
  ~SliderItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgSlider(FrMsg m, FrMsgLength l);
  
  void registerEvents();
  void registerSliderFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleSliderChanged(StringHash eventType, VariantMap& eventData);
};

GIO_METHOD_DEC(CheckBoxItem, ScreenRect)
GIO_METHOD_DEC(CheckBoxItem, Parent)
GIO_METHOD_DEC(CheckBoxItem, EntityId)
GIO_METHOD_DEC(CheckBoxItem, CheckBox)
GCO_FACTORY_DEC(CheckBoxItem)

class CheckBoxItem : public HasUIElement, public Object  {

URHO3D_OBJECT(CheckBoxItem, Object);

protected:
    SharedPtr<CheckBox> checkbox;
    FrMessageFn2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  CheckBoxItem();
  ~CheckBoxItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgCheckBox(FrMsg m, FrMsgLength l);

  void registerEvents();
  void registerCheckBoxFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleToggled(StringHash eventType, VariantMap& eventData);
};



#endif
