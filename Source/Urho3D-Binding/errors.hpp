// This source file is part of HGamer3D
// (A project to enable 3D game development in Haskell)
// For the latest info, see http://www.hgamer3d.org
//
// (c) 2015 Peter Althainz
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

#ifndef __errors_hpp__
#define __errors_hpp__

enum Errors {
  OK = 0,
  ERROR_SYSTEM_NOT_INITIALIZED,
  ERROR_SYSTEM_ALREADY_INITIALIZED,
  ERROR_PROPERTY_NOT_AVAILABLE,
  ERROR_TYPE_NOT_KNOWN,
  ERROR_WRONG_PARAMETERS,
  ERROR_COULD_NOT_INITIALIZE_ENGINE
};

#endif
