//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/DropDownListCbor.hpp

#ifndef __DropDownList_cbor__
#define __DropDownList_cbor__

#include <string>
#include <vector>

#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Selection = 0,
    NoSelection = 1,
} EnumTextSelection;

typedef struct {
    EnumTextSelection selector;
    struct {
        struct {
            int32_t value0;
        } Selection;
        struct {
        } NoSelection;
    } data;
} TextSelection;

typedef struct {
    std::vector<std::string> content;
    TextSelection selected;
} DropDownList;

void readTextSelection(CborValue *it0, TextSelection *textSelection);
void writeTextSelection(CborEncoder *enc0, TextSelection textSelection);
void readDropDownList(CborValue *it0, DropDownList *dropDownList);
void writeDropDownList(CborEncoder *enc0, DropDownList dropDownList);

} // end of namespacd cdb

extern const uint64_t ctDropDownList;

#endif
