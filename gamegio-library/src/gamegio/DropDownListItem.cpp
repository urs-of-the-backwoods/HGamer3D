//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/DropDownListItem.cpp

#include "ScreenRectCbor.hpp"
#include "ParentCbor.hpp"

#include "DropDownListItem.hpp"

using namespace std;


//
// DropDownListItem
//

GIO_METHOD_FUNC(DropDownListItem, ScreenRect)
GIO_METHOD_FUNC(DropDownListItem, Parent)
GIO_METHOD_FUNC(DropDownListItem, EntityId)
GIO_METHOD_FUNC(DropDownListItem, DropDownList)

GCO_FACTORY_IMP(DropDownListItem)
    GCO_FACTORY_METHOD(DropDownListItem, ctScreenRect, ScreenRect)
    GCO_FACTORY_METHOD(DropDownListItem, ctParent, Parent)
    GCO_FACTORY_METHOD(DropDownListItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(DropDownListItem, ctDropDownList, DropDownList)
GCO_FACTORY_IMP_END

DropDownListItem::DropDownListItem() : HasUIElement(), Object(Graphics3DSystem::getG3DS()->context)
{
    callbackF = NULL;
    callbackData = NULL;
}

FrItem DropDownListItem::msgCreate(FrMsg m, FrMsgLength l)
{
    DropDownListItem* item = new DropDownListItem();
    UI* ui = item->g->context->GetSubsystem<UI>();
    item->dropdownlist = new DropDownList(item->g->context);
    item->uiElement.StaticCast(item->dropdownlist);
    ui->GetRoot()->AddChild(item->dropdownlist);
    item->dropdownlist->SetStyleAuto();
    item->dropdownlist->GetPlaceholder()->SetVerticalAlignment(VA_CENTER);
    item->registerEvents();
    return (FrItem)item;
}

void DropDownListItem::registerEvents()
{
    SubscribeToEvent(uiElement, E_ITEMSELECTED, URHO3D_HANDLER(DropDownListItem, HandleSelectionChanged));
}

void DropDownListItem::msgDestroy()
{
    delete this;
}

DropDownListItem::~DropDownListItem()
{
    UnsubscribeFromEvent(uiElement, E_ITEMSELECTED);
    UI* ui = g->context->GetSubsystem<UI>();
    ui->GetRoot()->RemoveChild(dropdownlist);
}

void DropDownListItem::msgDropDownList(FrMsg m, FrMsgLength l)
{

    CborParser parser; CborValue it;
    cbor_parser_init(m, l, 0, &parser, &it);
    cbd::readDropDownList(&it, &ddl);

    dropdownlist->RemoveAllItems();
    // create list of text items
    for(std::vector<std::string>::iterator it = ddl.content.begin(); it != ddl.content.end(); ++it) {
//        std::cout << "found dd element: " << it->c_str() << std::endl;
        Text* t = new Text(g->context);
        t->SetText(it->c_str());
        t->SetStyleAuto();
        t->SetSize(dropdownlist->GetWidth(), dropdownlist->GetHeight());
        dropdownlist->AddItem(t);
    }
    dropdownlist->SetResizePopup(true);
    
    // create selection
    if (ddl.selected.selector == cbd::NoSelection) {
        dropdownlist->SetSelection(-1);
    } else {
        dropdownlist->SetSelection(ddl.selected.data.Selection.value0);
    }
}

void DropDownListItem::registerDropDownListFunction(FrMessageFn2 f, void* p2, uint64_t cbet)
{
    callbackF = f;
    callbackData = p2;
    cbEventType = cbet;
}

void DropDownListItem::HandleSelectionChanged(StringHash eventType, VariantMap& eventData)
{
    if (callbackF != NULL) {

        uint8_t buf[1048];
        CborEncoder encoder;
        cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

        int i = dropdownlist->GetSelection();

        if (i >= 0) {
            ddl.selected.selector = cbd::Selection;
            ddl.selected.data.Selection.value0 = i;
        }
        else {
            ddl.selected.selector = cbd::NoSelection;
        }

        cbd::writeDropDownList(&encoder, ddl);

        size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
        callbackF(callbackData, cbEventType, buf, len);

   }
}


