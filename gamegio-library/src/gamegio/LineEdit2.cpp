//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/LineEdit2.cpp

#include "LineEdit2.hpp"

using namespace std;

void LineEdit2::SetText(const String& text)
{
    if (text != line_)
    {
        line_ = text;
        cursorPosition_ = line_.LengthUTF8();
        LineEdit2::UpdateText();
        UpdateCursor();
    }
}

LineEdit2::LineEdit2(Context* context) : LineEdit(context) {
}

void LineEdit2::UpdateText()
{
   unsigned utf8Length = line_.LengthUTF8();

    if (!echoCharacter_)
        text_->SetText(line_);
    else
    {
        String echoText;
        for (unsigned i = 0; i < utf8Length; ++i)
            echoText.AppendUTF8(echoCharacter_);
        text_->SetText(echoText);
    }
    if (cursorPosition_ > utf8Length)
    {
        cursorPosition_ = utf8Length;
        UpdateCursor();
    }

    using namespace TextChanged;
/*
    VariantMap& eventData = GetEventDataMap();
    eventData[P_ELEMENT] = this;
    eventData[P_TEXT] = line_;
    SendEvent(E_TEXTCHANGED, eventData);}
*/
}
