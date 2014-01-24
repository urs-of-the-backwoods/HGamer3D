/*
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011 Peter Althainz

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include "Ogre.h"
#include "HG3DMessagePump.h"


	enum CEGUIScan
    {
		CEGUINull = 0x00,
        CEGUIEscape          =0x01,
        One             =0x02,
        Two             =0x03,
        Three           =0x04,
        Four            =0x05,
        Five            =0x06,
        Six             =0x07,
        Seven           =0x08,
        Eight           =0x09,
        Nine            =0x0A,
        Zero            =0x0B,
        Minus           =0x0C,    /* - on main keyboard */
        Equals			=0x0D,
        Backspace		=0x0E,    /* backspace */
        Tab				=0x0F,
        Q               =0x10,
        W               =0x11,
        E               =0x12,
        R               =0x13,
        T               =0x14,
        Y               =0x15,
        U               =0x16,
        I               =0x17,
        O               =0x18,
        P               =0x19,
        LeftBracket     =0x1A,
        RightBracket    =0x1B,
        Return			=0x1C,    /* Enter on main keyboard */
        LeftControl		=0x1D,
        A               =0x1E,
        S               =0x1F,
        D               =0x20,
        F               =0x21,
        G               =0x22,
        H               =0x23,
        J               =0x24,
        K               =0x25,
        L               =0x26,
        Semicolon       =0x27,
        Apostrophe		=0x28,
        Grave           =0x29,    /* accent grave */
        LeftShift       =0x2A,
        Backslash       =0x2B,
        Z               =0x2C,
        X               =0x2D,
        C               =0x2E,
        V               =0x2F,
        B               =0x30,
        N               =0x31,
        M               =0x32,
        Comma           =0x33,
        Period          =0x34,    /* . on main keyboard */
        Slash           =0x35,    /* '/' on main keyboard */
        RightShift      =0x36,
        Multiply        =0x37,    /* * on numeric keypad */
        LeftAlt        =0x38,    /* left Alt */
        Space           =0x39,
        Capital         =0x3A,
        F1              =0x3B,
        F2              =0x3C,
        F3              =0x3D,
        F4              =0x3E,
        F5              =0x3F,
        F6              =0x40,
        F7              =0x41,
        F8              =0x42,
        F9              =0x43,
        F10             =0x44,
        NumLock         =0x45,
        ScrollLock      =0x46,    /* Scroll Lock */
        Numpad7         =0x47,
        Numpad8         =0x48,
        Numpad9         =0x49,
        Subtract        =0x4A,    /* - on numeric keypad */
        Numpad4         =0x4B,
        Numpad5         =0x4C,
        Numpad6         =0x4D,
        Add				=0x4E,    /* + on numeric keypad */
        Numpad1         =0x4F,
        Numpad2         =0x50,
        Numpad3         =0x51,
        Numpad0         =0x52,
        Decimal			=0x53,    /* . on numeric keypad */
        OEM_102         =0x56,    /* < > | on UK/Germany keyboards */
        F11             =0x57,
        F12             =0x58,
        F13             =0x64,    /*                     (NEC PC98) */
        F14             =0x65,    /*                     (NEC PC98) */
        F15             =0x66,    /*                     (NEC PC98) */
        Kana            =0x70,    /* (Japanese keyboard)            */
        ABNT_C1         =0x73,    /* / ? on Portugese (Brazilian) keyboards */
        Convert         =0x79,    /* (Japanese keyboard)            */
        NoConvert       =0x7B,    /* (Japanese keyboard)            */
        Yen             =0x7D,    /* (Japanese keyboard)            */
        ABNT_C2         =0x7E,    /* Numpad . on Portugese (Brazilian) keyboards */
        NumpadEquals    =0x8D,    /* = on numeric keypad (NEC PC98) */
        PrevTrack       =0x90,    /* Previous Track (KC_CIRCUMFLEX on Japanese keyboard) */
        At              =0x91,    /*                     (NEC PC98) */
        Colon           =0x92,    /*                     (NEC PC98) */
        Underline       =0x93,    /*                     (NEC PC98) */
        Kanji           =0x94,    /* (Japanese keyboard)            */
        Stop            =0x95,    /*                     (NEC PC98) */
        AX              =0x96,    /*                     (Japan AX) */
        Unlabeled       =0x97,    /*                        (J3100) */
        NextTrack       =0x99,    /* Next Track */
        NumpadEnter     =0x9C,    /* Enter on numeric keypad */
        RightControl    =0x9D,
        Mute            =0xA0,    /* Mute */
        Calculator      =0xA1,    /* Calculator */
        PlayPause       =0xA2,    /* Play / Pause */
        MediaStop       =0xA4,    /* Media Stop */
        VolumeDown      =0xAE,    /* Volume - */
        VolumeUp        =0xB0,    /* Volume + */
        WebHome         =0xB2,    /* Web home */
        NumpadComma     =0xB3,    /* , on numeric keypad (NEC PC98) */
        Divide          =0xB5,    /* / on numeric keypad */
        SysRq           =0xB7,
        RightAlt        =0xB8,    /* right Alt */
        Pause           =0xC5,    /* Pause */
        Home            =0xC7,    /* Home on arrow keypad */
        ArrowUp         =0xC8,    /* UpArrow on arrow keypad */
        PageUp          =0xC9,    /* PgUp on arrow keypad */
        ArrowLeft       =0xCB,    /* LeftArrow on arrow keypad */
        ArrowRight      =0xCD,    /* RightArrow on arrow keypad */
        End             =0xCF,    /* End on arrow keypad */
        ArrowDown       =0xD0,    /* DownArrow on arrow keypad */
        PageDown		=0xD1,    /* PgDn on arrow keypad */
        Insert          =0xD2,    /* Insert on arrow keypad */
        Delete          =0xD3,    /* Delete on arrow keypad */
        LeftWindows     =0xDB,    /* Left Windows key */
        RightWindows    =0xDC,    /* Right Windows key - Correct spelling :) */
        AppMenu         =0xDD,    /* AppMenu key */
        Power           =0xDE,    /* System Power */
        CEGUISleep           =0xDF,    /* System Sleep */
        Wake			=0xE3,    /* System Wake */
        WebSearch		=0xE5,    /* Web Search */
        WebFavorites	=0xE6,    /* Web Favorites */
        WebRefresh		=0xE7,    /* Web Refresh */
        WebStop			=0xE8,    /* Web Stop */
        WebForward		=0xE9,    /* Web Forward */
        WebBack			=0xEA,    /* Web Back */
        MyComputer		=0xEB,    /* My Computer */
        Mail			=0xEC,    /* Mail */
        MediaSelect		=0xED     /* Media Select */
    };


enum CEGUIScan scanCodeToKeyScan( int scancode )
{
   switch(scancode)
   {
      case VK_ESCAPE:
      {
         return CEGUIScan::CEGUIEscape;
      }
      case VK_F1:
      {
         return CEGUIScan::F1;
      }
      case VK_F2:
      {
         return CEGUIScan::F2;
      }
      case VK_F3:
      {
         return CEGUIScan::F3;
      }
      case VK_F4:
      {
         return CEGUIScan::F4;
      }
      case VK_F5:
      {
         return CEGUIScan::F5;
      }
      case VK_F6:
      {
         return CEGUIScan::F6;
      }
      case VK_F7:
      {
         return CEGUIScan::F7;
      }
      case VK_F8:
      {
         return CEGUIScan::F8;
      }
      case VK_F9:
      {
         return CEGUIScan::F9;
      }
      case VK_F10:
      {
         return CEGUIScan::F10;
      }
      case VK_F11:
      {
         return CEGUIScan::F11;
      }
      case VK_F12:
      {
         return CEGUIScan::F12;
      }
/*
      case VK_'`':
      {
         return CEGUIScan::Apostrophe;
      }
      case VK_'1':
      {
         return CEGUIScan::One;
      }
      case VK_'2':
      {
         return CEGUIScan::Two;
      }
      case VK_'3':
      {
         return CEGUIScan::Three;
      }
      case VK_'4':
      {
         return CEGUIScan::Four;
      }
      case VK_'5':
      {
         return CEGUIScan::Five;
      }
      case VK_'6':
      {
         return CEGUIScan::Six;
      }
      case VK_'7':
      {
         return CEGUIScan::Seven;
      }
      case VK_'8':
      {
         return CEGUIScan::Eight;
      }
      case VK_'9':
      {
         return CEGUIScan::Nine;
      }
      case VK_'0':
      {
         return CEGUIScan::Zero;
      }
      case VK_'-':
      {
         return CEGUIScan::Minus;
      }
      case VK_'=':
      {
         return CEGUIScan::Equals;
      }
*/
      case VK_BACK:
      {
         return CEGUIScan::Backspace;
      }
      case VK_TAB:
      {
         return CEGUIScan::Tab;
      }
/*
      case VK_'A':
      {
         return CEGUIScan::A;
      }
      case VK_'B':
      {
         return CEGUIScan::B;
      }
      case VK_'C':
      {
         return CEGUIScan::C;
      }
      case VK_'D':
      {
         return CEGUIScan::D;
      }
      case VK_'E':
      {
         return CEGUIScan::E;
      }
      case VK_'F':
      {
         return CEGUIScan::F;
      }
      case VK_'G':
      {
         return CEGUIScan::G;
      }
      case VK_'H':
      {
         return CEGUIScan::H;
      }
      case VK_'I':
      {
         return CEGUIScan::I;
      }
      case VK_'J':
      {
         return CEGUIScan::J;
      }
      case VK_'K':
      {
         return CEGUIScan::K;
      }
      case VK_'L':
      {
         return CEGUIScan::L;
      }
      case VK_'M':
      {
         return CEGUIScan::M;
      }
      case VK_'N':
      {
         return CEGUIScan::N;
      }
      case VK_'O':
      {
         return CEGUIScan::O;
      }
      case VK_'P':
      {
         return CEGUIScan::P;
      }
      case VK_'Q':
      {
         return CEGUIScan::Q;
      }
      case VK_'R':
      {
         return CEGUIScan::R;
      }
      case VK_'S':
      {
         return CEGUIScan::S;
      }
      case VK_'T':
      {
         return CEGUIScan::T;
      }
      case VK_'U':
      {
         return CEGUIScan::U;
      }
      case VK_'V':
      {
         return CEGUIScan::V;
      }
      case VK_'W':
      {
         return CEGUIScan::W;
      }
      case VK_'X':
      {
         return CEGUIScan::X;
      }
      case VK_'Y':
      {
         return CEGUIScan::Y;
      }
      case VK_'Z':
      {
         return CEGUIScan::Z;
      }
      case VK_'(':
      {
         return CEGUIScan::LeftBracket;
      }
      case VK_')':
      {
         return CEGUIScan::RightBracket;
      }
      case VK_'\\':
      {
         return CEGUIScan::Backslash;
      }
      case VK_Shift_Lock:
      {
         return CEGUIScan::Capital;
      }
      case VK_';':
      {
         return CEGUIScan::Semicolon;
      }
      case VK_'\'':
      {
         return CEGUIScan::Apostrophe;
      }
 */
      case VK_RETURN:
      {
         return CEGUIScan::Return;
      }
      case VK_LSHIFT:
      {
         return CEGUIScan::LeftShift;
      }
      case VK_SHIFT:
      {
         return CEGUIScan::LeftShift;
      }
/*
      case VK_',':
      {
         return CEGUIScan::Comma;
      }
      case VK_'.':
      {
         return CEGUIScan::Period;
      }
      case VK_'/':
      {
         return CEGUIScan::Slash;
      }
*/
      case VK_RSHIFT:
      {
         return CEGUIScan::RightShift;
      }
      case VK_CONTROL:
      {
         return CEGUIScan::LeftControl;
      }
      case VK_LCONTROL:
      {
         return CEGUIScan::LeftControl;
      }
/*      
      case VK_Super_L:
      {
         return CEGUIScan::Scan(0);//?
      }
      case VK_' ':
      {
         return CEGUIScan::Space;
      }
      case VK_LALT:
      {
         return CEGUIScan::LeftAlt;
      }
      case VK_RALT:
      {
         return CEGUIScan::RightAlt;
      }
      case VK_Super_R:
      {
         return CEGUIScan::Scan(0);//?
      }
*/
      case VK_RCONTROL:
      {
         return CEGUIScan::RightControl;
      }
      case VK_PRINT:
      {
         return CEGUIScan::SysRq;
      }
/*      
      case VK_Scroll_Lock:
      {
         return CEGUIScan::ScrollLock;
      }
      case VK_Pause:
      {
         return CEGUIScan::Pause;
      }
*/
      case VK_HOME:
      {
         return CEGUIScan::Home;
      }
/*
      case VK_Page_Up:
      {
         return CEGUIScan::PageUp;
      }
      case VK_End:
      {
         return CEGUIScan::End;
      }
      case VK_Page_Down:
      {
         return CEGUIScan::PageDown;
      }
*/
      case VK_DELETE:
      {
         return CEGUIScan::Delete;
      }
      case VK_INSERT:
      {
         return CEGUIScan::Insert;
      }
      case VK_LEFT:
      {
         return CEGUIScan::ArrowLeft;
      }
      case VK_UP:
      {
         return CEGUIScan::ArrowUp;
      }
      case VK_RIGHT:
      {
         return CEGUIScan::ArrowRight;
      }
      case VK_DOWN:
      {
         return CEGUIScan::ArrowDown;
      }
/*
      case VK_Num_Lock:
      {
         return CEGUIScan::NumLock;
      }
      case VK_KP_Divide:
      {
         return CEGUIScan::Divide;
      }
      case VK_KP_Multiply:
      {
         return CEGUIScan::Multiply;
      }
      case VK_KP_Subtract:
      {
         return CEGUIScan::Subtract;
      }
      case VK_KP_Add:
      {
         return CEGUIScan::Add;
      }
      case VK_KP_Home:
      {
         return CEGUIScan::Numpad7;
      }
      case VK_KP_Up:
      {
         return CEGUIScan::Numpad8;
      }
      case VK_KP_Page_Up:
      {
         return CEGUIScan::Numpad9;
      }
      case VK_KP_Left:
      {
         return CEGUIScan::Numpad4;
      }
      case VK_KP_Begin:
      {
         return CEGUIScan::Scan(0); //?
      }
      case VK_KP_Right:
      {
         return CEGUIScan::Numpad6;
      }
      case VK_KP_End:
      {
         return CEGUIScan::Numpad1;
      }
      case VK_KP_Down:
      {
         return CEGUIScan::Numpad2;
      }
      case VK_KP_Page_Down:
      {
         return CEGUIScan::Numpad3;
      }
      case VK_KP_Insert:
      {
         return CEGUIScan::Numpad0;
      }
      case VK_KP_Delete:
      {
         return CEGUIScan::Decimal;
      }
      case VK_KP_Enter:
      {
         return CEGUIScan::NumpadEnter;
      }
*/
      default:
      {
         return CEGUIScan::CEGUINull;
      }
   }
}

HG3DMessagePump::HG3DMessagePump()
{
}

HG3DMessagePump::~HG3DMessagePump()
{
}
	
void HG3DMessagePump::messagePump()
{
	MSG msg;
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) 
	{
		if ( 
			(msg.message == WM_KEYDOWN) ||
			(msg.message == WM_KEYUP) ||
			(msg.message == WM_CHAR) 
		   )
			{
				mMsgList.push_back( msg );
			};
		 TranslateMessage(&msg);
		 DispatchMessage(&msg);
	}
}

bool HG3DMessagePump::eventsPending()
{
	return !mMsgList.empty();
}

int HG3DMessagePump::getKeyUpEvent()
{
	if (!mMsgList.empty())
	{
		MSG msg = mMsgList.front();
		if (msg.message == WM_KEYUP) {
			mMsgList.pop_front();
			return (int) scanCodeToKeyScan(msg.wParam);
		};
	};
	return 0;
}

int HG3DMessagePump::getKeyDownEvent()
{
	if (!mMsgList.empty())
	{
		MSG msg = mMsgList.front();
		if (msg.message == WM_KEYDOWN) {
			mMsgList.pop_front();
			return (int) scanCodeToKeyScan(msg.wParam);
		};
	};
	return 0;
}

int HG3DMessagePump::getCharEvent()
{
	if (!mMsgList.empty())
	{
		MSG msg = mMsgList.front();
		if (msg.message == WM_CHAR) {
			mMsgList.pop_front();
			return msg.wParam;
		};
	};
	return 0;
}

