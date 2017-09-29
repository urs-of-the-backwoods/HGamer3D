//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
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
#include <Urho3D/UI/BorderImage.h>
#include <Urho3D/UI/LineEdit.h>
#include <Urho3D/UI/Slider.h>
#include <Urho3D/UI/CheckBox.h>
#include <Urho3D/UI/DropDownList.h>
#include <Urho3D/UI/Window.h>
#include <Urho3D/UI/ToolTip.h>
#include <Urho3D/UI/UI.h>
#include <Urho3D/UI/UIEvents.h>
#include <Urho3D/UI/UIElement.h>

#include <exception>

#include "Graphics3DSystem.hpp"
#include "Urho3D/DebugNew.h"
#include "Slider2.hpp"

#include "SliderCbor.hpp"
#include "EditTextCbor.hpp"
#include "EntityIdCbor.hpp"
#include "LineEdit2.hpp"
#include "FontCbor.hpp"
#include "AlignmentCbor.hpp"
#include "ColourCbor.hpp"
#include "WindowGUICbor.hpp"
#include "TooltipCbor.hpp"
#include "NameCbor.hpp"
#include "LayoutCbor.hpp"
#include "ParentCbor.hpp"
#include "MinSizeCbor.hpp"
#include "ButtonCbor.hpp"
#include "CheckBoxCbor.hpp"
#include "StaticTextCbor.hpp"
#include "UIStyleCbor.hpp"
#include "IntVec2Cbor.hpp"
#include "UIEventCbor.hpp"
#include "TextureCbor.hpp"
#include "BlendModeCbor.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <iostream>
#include <fstream>

using namespace Urho3D;

//
// HasUIElement
//

GCO_FACTORY_DEC(HasUIElement)

class HasUIElement : public Object {

URHO3D_OBJECT(HasUIElement, Object);

protected:
  SharedPtr<UIElement> uiElement;
  Graphics3DSystem* g;
  cbd::EntityId myId;

  // hover
  FrMessageFn2 cbfHover;
  void* cbdHover;
  uint64_t cbetHover;
  // drag
  FrMessageFn2 cbfDrag;
  void* cbdDrag;
  uint64_t cbetDrag;
  
public:
  HasUIElement();
  ~HasUIElement();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgScreenRect(FrMsg m, FrMsgLength l);
  void msgAlignment(FrMsg m, FrMsgLength l);
  void msgParent(FrMsg m, FrMsgLength l);
  void msgEntityId(FrMsg m, FrMsgLength l);
  void msgName(FrMsg m, FrMsgLength l);
  void msgLayout(FrMsg m, FrMsgLength l);
  void msgMinSize(FrMsg m, FrMsgLength l);
  void msgPosition2D(FrMsg m, FrMsgLength l);
  void msgSize2D(FrMsg m, FrMsgLength l);
  void msgUIStyle(FrMsg m, FrMsgLength l);


  // Hover
  void registerUIHoverEventFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleHover(StringHash eventType, VariantMap& eventData);
  // Drag
  void registerUIDragEventFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleDrag(StringHash eventType, VariantMap& eventData);
};

//
// ButtonItem
//

GCO_FACTORY_DEC(ButtonItem)

class ButtonItem : public HasUIElement {

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
  
  void registerButtonFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandlePressedReleasedChanged(StringHash eventType, VariantMap& eventData);
};

//
// Standard ButtonItem
//

GCO_FACTORY_DEC(StandardButtonItem)

class StandardButtonItem : public ButtonItem {

protected:
  FrMessageFn2 bevtF;
  void* bevtD;
  uint64_t bevtET;

public:
  StandardButtonItem();
  ~StandardButtonItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void registerButtonEventFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleButtonEvent(StringHash eventType, VariantMap& eventData);
};


//
// ImageButtonItem
//

GCO_FACTORY_DEC(ImageButtonItem)

class ImageButtonItem : public StandardButtonItem {

public:
  ImageButtonItem();
  ~ImageButtonItem();
 
  void msgBlendMode(FrMsg m, FrMsgLength l);
  
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
};


//
// EditText
//

GCO_FACTORY_DEC(EditTextItem)

class EditTextItem : public HasUIElement {

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


//
// TextItem
//

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
  void msgFont(FrMsg m, FrMsgLength l);
  void msgFontSize(FrMsg m, FrMsgLength l);
  void msgColour(FrMsg m, FrMsgLength l);
};


//
// SliderItem
//

GCO_FACTORY_DEC(SliderItem)

class SliderItem : public HasUIElement  {

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

//
// CheckBoxItem
//

GCO_FACTORY_DEC(CheckBoxItem)

class CheckBoxItem : public HasUIElement  {

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


//
// WindowGUI
//

GCO_FACTORY_DEC(WindowGUI)

class WindowGUI : public HasUIElement {

protected:
  SharedPtr<Window> window;
  
public:
  WindowGUI();
  ~WindowGUI();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
};


//
// Tooltip
//

GCO_FACTORY_DEC(Tooltip)

class Tooltip : public HasUIElement {

protected:
  SharedPtr<ToolTip> toolTip;
  SharedPtr<Text> toolTipText;
  
public:
  Tooltip();
  ~Tooltip();

  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  void msgTooltip(FrMsg m, FrMsgLength l);
};


#endif
