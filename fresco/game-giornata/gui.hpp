//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/gui.hpp

#ifndef __gui_hpp__
#define __gui_hpp__

#include <iostream>
#include <fstream>
#include <string>

#include "msgpack.hpp"

#include "Urho3D/Urho3D.h"

#include "Urho3D/Core/Context.h"
#include "Urho3D/Core/Main.h"
#include "Urho3D/Core/Object.h"

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
#include <Urho3D/UI/LineEdit.h>
#include <Urho3D/UI/Slider.h>
#include <Urho3D/UI/CheckBox.h>
#include <Urho3D/UI/DropDownList.h>
#include <Urho3D/UI/Window.h>
#include <Urho3D/UI/UI.h>
#include <Urho3D/UI/UIEvents.h>

#include <exception>

#include "errors.hpp"
#include "graphics3d.hpp"
#include "Urho3D/DebugNew.h"
#include "Slider2.hpp"
#include "LineEdit2.hpp"

using namespace Urho3D;

extern "C" {
#include "interface.h"
}

class HasUIElement {

protected:
  SharedPtr<UIElement> uiElement;
  Graphics3DSystem* g;
  
public:
  HasUIElement(Graphics3DSystem*g3ds);
  ~HasUIElement();

  int msgScreenRect(char* pdata, int len);
};


class ButtonItem : public HasUIElement, public Object {

OBJECT(ButtonItem);

protected:
    SharedPtr<Button> button;
    msgFP2 callbackF;
    void* callbackData;
    uint64_t cbEventType;
    
public:
  ButtonItem(Graphics3DSystem* g3ds);
  ~ButtonItem();
 
  int create(char* pdata, int len);
  
  void registerPressedReleasedFunction(msgFP2 f, void* p2, uint64_t evt_t);
  void HandlePressedReleasedChanged(StringHash eventType, VariantMap& eventData);
};

class EditTextItem : public HasUIElement, public Object {

OBJECT(EditTextItem);

protected:
    SharedPtr<LineEdit2> edittext;
    msgFP2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  EditTextItem(Graphics3DSystem* g3ds);
  ~EditTextItem();
 
  int create(char* pdata, int len);
  int msgEditText(char* pdata, int len);
  
  void registerTextEventFunction(msgFP2 f, void* p2, uint64_t evt_t);
  void HandleTextChanged(StringHash eventType, VariantMap& eventData);
};

class TextItem : public HasUIElement {

protected:
    SharedPtr<Text> text;

public:
  TextItem(Graphics3DSystem* g3ds);
  ~TextItem();
 
  int create(char* pdata, int len);
  int msgText(char* pdata, int len);
};

class SliderItem : public HasUIElement, public Object  {

OBJECT(SliderItem);

protected:
    SharedPtr<Slider2> slider;
    msgFP2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  SliderItem(Graphics3DSystem* g3ds);
  ~SliderItem();
 
  int create(char* pdata, int len);
  int msgSlider(char* pdata, int len);
  
  void registerSliderEventFunction(msgFP2 f, void* p2, uint64_t evt_t);
  void HandleSliderChanged(StringHash eventType, VariantMap& eventData);
};

class CheckBoxItem : public HasUIElement, public Object  {

OBJECT(CheckBoxItem);

protected:
    SharedPtr<CheckBox> checkbox;
    msgFP2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  CheckBoxItem(Graphics3DSystem* g3ds);
  ~CheckBoxItem();
 
  int create(char* pdata, int len);
  int msgCheckBox(char* pdata, int len);

  void registerToggledEventFunction(msgFP2 f, void* p2, uint64_t evt_t);
  void HandleToggled(StringHash eventType, VariantMap& eventData);
};

class DropDownListItem : public HasUIElement, public Object  {

OBJECT(DropDownListItem);

protected:
    SharedPtr<DropDownList> dropdownlist;
    msgFP2 callbackF;
    void* callbackData;
    uint64_t cbEventType;

public:
  DropDownListItem(Graphics3DSystem* g3ds);
  ~DropDownListItem();
 
  int create(char* pdata, int len);
  int msgDropDownList(char* pdata, int len);

  void registerSelectionEventFunction(msgFP2 f, void* p2, uint64_t evt_t);
  void HandleSelectionChanged(StringHash eventType, VariantMap& eventData);
};


#endif
