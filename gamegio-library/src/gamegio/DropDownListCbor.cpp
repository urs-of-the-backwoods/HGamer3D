//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/DropDownListCbor.cpp

#include "DropDownListCbor.hpp"

namespace cbd {

    void readTextSelection(CborValue *it, TextSelection *textSelection) {
        { CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
            int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
            textSelection->selector = (EnumTextSelection)i;
            if (textSelection->selector == 0) {
                { int i; cbor_value_get_int(it, &i); textSelection->data.Selection.value0 = (int32_t)i;} cbor_value_advance_fixed(it);
            };
            if (textSelection->selector == 1) {
            };
            cbor_value_leave_container(ita, it); }
        }

        void writeTextSelection(CborEncoder *enc, TextSelection textSelection) {
            if (textSelection.selector == 0)
                { CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
                    cbor_encode_uint(enc, (uint64_t)textSelection.selector);
                    cbor_encode_int(enc, (int64_t)textSelection.data.Selection.value0);
                    cbor_encoder_close_container_checked(enca, enc); }
            if (textSelection.selector == 1)
                { CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 1);
                    cbor_encode_uint(enc, (uint64_t)textSelection.selector);
                    cbor_encoder_close_container_checked(enca, enc); }
            }


            void readDropDownList(CborValue *it, DropDownList *dropDownList) {

                { CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);

                    { CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
                      dropDownList->content.clear();
                      while (!cbor_value_at_end(it)) {
                        std::string item; {
                            size_t l; cbor_value_calculate_string_length(it, &l);
                            item.resize(l+1);
                            cbor_value_copy_text_string(it, (char *)(item.c_str()), &l, NULL);
                            cbor_value_advance(it);
                            dropDownList->content.push_back(item);
                        };
                      }
                      cbor_value_leave_container(ita, it);
                    }
                    readTextSelection(it, &(dropDownList->selected));
                    cbor_value_leave_container(ita, it); }
            }

            void writeDropDownList(CborEncoder *enc, DropDownList dropDownList) {
                { CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
                    { size_t l; l = dropDownList.content.size();
                        { CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, l);
                            for (int i = 0; i < l; i++) {cbor_encode_text_stringz(enc, dropDownList.content[i].c_str());
                                ; }cbor_encoder_close_container_checked(enca, enc); }
                            }
                            writeTextSelection(enc, dropDownList.selected);
                            cbor_encoder_close_container_checked(enca, enc); }
                        }


} // end of namespacd cdb

const uint64_t ctDropDownList = 0x200de0e837a8e590;
