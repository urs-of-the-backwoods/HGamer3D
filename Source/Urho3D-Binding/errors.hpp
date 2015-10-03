//	error codes for C++ part of bindings
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/errors.hpp

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
