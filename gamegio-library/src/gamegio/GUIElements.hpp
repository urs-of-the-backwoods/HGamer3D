//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/gamegio/src/GUIElements.hpp

#ifndef __gui_elements_hpp__
#define __gui_elements_hpp__

#include "Urho3D/Urho3D.h"

#include "Urho3D/Core/Context.h"
#include "Urho3D/Core/Main.h"
#include "Urho3D/Core/Object.h"
#include "Urho3D/Container/Str.h"

#include "Urho3D/Engine/Application.h"
#include "Urho3D/Engine/Engine.h"
#include "Urho3D/Graphics/Graphics.h"
#include "Urho3D/Graphics/GraphicsImpl.h"
#include "Urho3D/IO/IOEvents.h"
#include "Urho3D/IO/Log.h"
#include "Urho3D/Core/ProcessUtils.h"

#include <Urho3D/Graphics/Camera.h>
#include <Urho3D/Core/CoreEvents.h>
#include <Urho3D/UI/Font.h>
#include <Urho3D/Input/Input.h>
#include <Urho3D/Graphics/Material.h>
#include <Urho3D/Graphics/Model.h>
#include <Urho3D/Graphics/Octree.h>
#include <Urho3D/Graphics/Renderer.h>
#include <Urho3D/Resource/ResourceCache.h>
#include <Urho3D/Scene/Scene.h>
#include <Urho3D/Graphics/StaticModel.h>
#include <Urho3D/UI/Text.h>
#include <Urho3D/UI/Button.h>
#include <Urho3D/UI/Sprite.h>
#include <Urho3D/UI/LineEdit.h>
#include <Urho3D/UI/Slider.h>
#include <Urho3D/UI/CheckBox.h>
#include <Urho3D/UI/DropDownList.h>
#include <Urho3D/UI/Window.h>
#include <Urho3D/UI/UI.h>
#include <Urho3D/UI/UIEvents.h>

#include <exception>

#include "Graphics3DSystem.hpp"
#include "Urho3D/DebugNew.h"
#include "Slider2.hpp"
#include "EntityIdCbor.hpp"
#include "LineEdit2.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <iostream>
#include <fstream>

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
