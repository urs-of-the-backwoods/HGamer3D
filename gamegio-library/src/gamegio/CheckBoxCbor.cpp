//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/CheckBoxCbor.cpp

#include "CheckBoxCbor.hpp"

namespace cbd {

void readCheckBox(CborValue *it, CheckBox *checkBox)
{
    bool rval;
    cbor_value_get_boolean(it, &(rval)); cbor_value_advance_fixed(it);
    *checkBox = rval;
}

void writeCheckBox(CborEncoder *enc, CheckBox checkBox)
{
    cbor_encode_boolean(enc, checkBox);
}


} // end of namespacd cdb

const uint64_t ctCheckBox = 0xd2425f880fcdd9a4;
