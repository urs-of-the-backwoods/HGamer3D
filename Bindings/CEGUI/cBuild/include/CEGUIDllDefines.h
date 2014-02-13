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


// CEGUIDllDefines.h

#ifndef _HGamer3DCEGUI030_DLLDEFINES_H_
#define _HGamer3DCEGUI030_DLLDEFINES_H_

/* Cmake will define HGamer3DCEGUI030_EXPORTS on Windows when it
configures to build a shared library. If you are going to use
another build system on windows or create the visual studio
projects by hand you need to define MyLibrary_EXPORTS when
building a DLL on windows.
*/

// We are using the Visual Studio Compiler and building Shared libraries

#if (defined (_WIN32)) && !(defined (__GNUC__)) 
  #if defined(HGamer3DCEGUI030_EXPORTS)
    #define  CEGUI_LIB_EXPORT __declspec(dllexport)
  #else
    #define  CEGUI_LIB_EXPORT __declspec(dllimport)
  #endif /* HGamer3DCEGUI030_EXPORTS */
#else /* defined (_WIN32) */
 #define CEGUI_LIB_EXPORT
#endif

#endif /* _HGamer3DCEGUI030_DLLDEFINES_H_ */
