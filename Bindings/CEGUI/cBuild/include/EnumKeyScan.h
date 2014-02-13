// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// EnumKeyScan.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumKeyScan
#define _DEFINED_HG3D_EnumKeyScan


enum EnumKeyScan
{
  KeyEscape = 0x01, // 
  KeyOne = 0x02, // 
  KeyTwo = 0x03, // 
  KeyThree = 0x04, // 
  KeyFour = 0x05, // 
  KeyFive = 0x06, // 
  KeySix = 0x07, // 
  KeySeven = 0x08, // 
  KeyEight = 0x09, // 
  KeyNine = 0x0A, // 
  KeyZero = 0x0B, // 
  KeyMinus = 0x0C, // 
  KeyEquals = 0x0D, // 
  KeyBackspace = 0x0E, // 
  KeyTab = 0x0F, // 
  KeyQ = 0x10, // 
  KeyW = 0x11, // 
  KeyE = 0x12, // 
  KeyR = 0x13, // 
  KeyT = 0x14, // 
  KeyY = 0x15, // 
  KeyU = 0x16, // 
  KeyI = 0x17, // 
  KeyO = 0x18, // 
  KeyP = 0x19, // 
  KeyLeftBracket = 0x1A, // 
  KeyRightBracket = 0x1B, // 
  KeyReturn = 0x1C, // 
  KeyLeftControl = 0x1D, // 
  KeyA = 0x1E, // 
  KeyS = 0x1F, // 
  KeyD = 0x20, // 
  KeyF = 0x21, // 
  KeyG = 0x22, // 
  KeyH = 0x23, // 
  KeyJ = 0x24, // 
  KeyK = 0x25, // 
  KeyL = 0x26, // 
  KeySemicolon = 0x27, // 
  KeyApostrophe = 0x28, // 
  KeyGrave = 0x29, // 
  KeyLeftShift = 0x2A, // 
  KeyBackslash = 0x2B, // 
  KeyZ = 0x2C, // 
  KeyX = 0x2D, // 
  KeyC = 0x2E, // 
  KeyV = 0x2F, // 
  KeyB = 0x30, // 
  KeyN = 0x31, // 
  KeyM = 0x32, // 
  KeyComma = 0x33, // 
  KeyPeriod = 0x34, // 
  KeySlash = 0x35, // 
  KeyRightShift = 0x36, // 
  KeyMultiply = 0x37, // 
  KeyLeftAlt = 0x38, // 
  KeySpace = 0x39, // 
  KeyCapital = 0x3A, // 
  KeyF1 = 0x3B, // 
  KeyF2 = 0x3C, // 
  KeyF3 = 0x3D, // 
  KeyF4 = 0x3E, // 
  KeyF5 = 0x3F, // 
  KeyF6 = 0x40, // 
  KeyF7 = 0x41, // 
  KeyF8 = 0x42, // 
  KeyF9 = 0x43, // 
  KeyF10 = 0x44, // 
  KeyNumLock = 0x45, // 
  KeyScrollLock = 0x46, // 
  KeyNumpad7 = 0x47, // 
  KeyNumpad8 = 0x48, // 
  KeyNumpad9 = 0x49, // 
  KeySubtract = 0x4A, // 
  KeyNumpad4 = 0x4B, // 
  KeyNumpad5 = 0x4C, // 
  KeyNumpad6 = 0x4D, // 
  KeyAdd = 0x4E, // 
  KeyNumpad1 = 0x4F, // 
  KeyNumpad2 = 0x50, // 
  KeyNumpad3 = 0x51, // 
  KeyNumpad0 = 0x52, // 
  KeyDecimal = 0x53, // 
  KeyOEM_102 = 0x56, // 
  KeyF11 = 0x57, // 
  KeyF12 = 0x58, // 
  KeyF13 = 0x64, // 
  KeyF14 = 0x65, // 
  KeyF15 = 0x66, // 
  KeyKana = 0x70, // 
  KeyABNT_C1 = 0x73, // 
  KeyConvert = 0x79, // 
  KeyNoConvert = 0x7B, // 
  KeyYen = 0x7D, // 
  KeyABNT_C2 = 0x7E, // 
  KeyNumpadEquals = 0x8D, // 
  KeyPrevTrack = 0x90, // 
  KeyAt = 0x91, // 
  KeyColon = 0x92, // 
  KeyUnderline = 0x93, // 
  KeyKanji = 0x94, // 
  KeyStop = 0x95, // 
  KeyAX = 0x96, // 
  KeyUnlabeled = 0x97, // 
  KeyNextTrack = 0x99, // 
  KeyNumpadEnter = 0x9C, // 
  KeyRightControl = 0x9D, // 
  KeyMute = 0xA0, // 
  KeyCalculator = 0xA1, // 
  KeyPlayPause = 0xA2, // 
  KeyMediaStop = 0xA4, // 
  KeyVolumeDown = 0xAE, // 
  KeyVolumeUp = 0xB0, // 
  KeyWebHome = 0xB2, // 
  KeyNumpadComma = 0xB3, // 
  KeyDivide = 0xB5, // 
  KeySysRq = 0xB7, // 
  KeyRightAlt = 0xB8, // 
  KeyPause = 0xC5, // 
  KeyHome = 0xC7, // 
  KeyArrowUp = 0xC8, // 
  KeyPageUp = 0xC9, // 
  KeyArrowLeft = 0xCB, // 
  KeyArrowRight = 0xCD, // 
  KeyEnd = 0xCF, // 
  KeyArrowDown = 0xD0, // 
  KeyPageDown = 0xD1, // 
  KeyInsert = 0xD2, // 
  KeyDelete = 0xD3, // 
  KeyLeftWindows = 0xDB, // 
  KeyRightWindows = 0xDC, // 
  KeyAppMenu = 0xDD, // 
  KeyPower = 0xDE, // 
  KeySleep = 0xDF, // 
  KeyWake = 0xE3, // 
  KeyWebSearch = 0xE5, // 
  KeyWebFavorites = 0xE6, // 
  KeyWebRefresh = 0xE7, // 
  KeyWebStop = 0xE8, // 
  KeyWebForward = 0xE9, // 
  KeyWebBack = 0xEA, // 
  KeyMyComputer = 0xEB, // 
  KeyMail = 0xEC, // 
  KeyMediaSelect = 0xED // 
};
#endif 
