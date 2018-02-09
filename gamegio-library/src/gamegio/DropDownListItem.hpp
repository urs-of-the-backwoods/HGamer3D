//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/DropDownListItem.hpp

#ifndef __dropdownlistitem_hpp__
#define __dropdownlistitem_hpp__

#include "DropDownListCbor.hpp"
#include "GUIElements.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(DropDownListItem, ScreenRect)
GIO_METHOD_DEC(DropDownListItem, Parent)
GIO_METHOD_DEC(DropDownListItem, EntityId)
GIO_METHOD_DEC(DropDownListItem, DropDownList)
GCO_FACTORY_DEC(DropDownListItem)

class DropDownListItem : public HasUIElement, public Object  {

URHO3D_OBJECT(DropDownListItem, Object);

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
