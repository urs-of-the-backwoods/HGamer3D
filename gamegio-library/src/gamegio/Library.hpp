//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/Library.hpp

#ifndef __gamegio_library_hpp
#define __gamegio_library_hpp

#include <map>

#include "Fresco.hpp"

class Library
{
    private:
        std::map<FrItemType, GCOFactory*> _factories;

    public:
        Library();
        ~Library();
        GCOFactory* getFactory(FrItemType);
};


#endif
