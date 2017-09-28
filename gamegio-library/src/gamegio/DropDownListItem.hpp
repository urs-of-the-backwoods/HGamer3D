//	C++ part of bindings for gui
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/gamegio/src/GUIElements.hpp

#ifndef __dropdownlistitem_hpp__
#define __dropdownlistitem_hpp__

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
#include <Urho3D/UI/LineEdit.h>
#include <Urho3D/UI/Slider.h>
#include <Urho3D/UI/CheckBox.h>
#include <Urho3D/UI/DropDownList.h>
#include <Urho3D/UI/Window.h>
#include <Urho3D/UI/UI.h>
#include <Urho3D/UI/UIEvents.h>

#include <exception>

#include "Graphics3DSystem.hpp"
#include "DropDownListCbor.hpp"
#include "GUIElements.hpp"
#include "Urho3D/DebugNew.h"

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <iostream>
#include <fstream>

using namespace Urho3D;

GIO_METHOD_DEC(DropDownListItem, ScreenRect)
GIO_METHOD_DEC(DropDownListItem, Parent)
GIO_METHOD_DEC(DropDownListItem, EntityId)
GIO_METHOD_DEC(DropDownListItem, DropDownList)
GCO_FACTORY_DEC(DropDownListItem)

class DropDownListItem : public HasUIElement  {

protected:
    SharedPtr<DropDownList> dropdownlist;
    FrMessageFn2 callbackF;
    void* callbackData;
    uint64_t cbEventType;
    cbd::DropDownList ddl;

public:
  DropDownListItem();
  ~DropDownListItem();
 
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDestroy();
  
  void msgDropDownList(FrMsg m, FrMsgLength l);

  void registerEvents();
  void registerDropDownListFunction(FrMessageFn2 f, void* p2, uint64_t evt_t);
  void HandleSelectionChanged(StringHash eventType, VariantMap& eventData);
};


#endif
