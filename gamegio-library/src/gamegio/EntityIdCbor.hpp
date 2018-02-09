//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/EntityIdCbor.hpp

#ifndef __EntityId_cbor__
#define __EntityId_cbor__

#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef std::vector<uint8_t> EntityId;

void readEntityId(CborValue *it, EntityId *entityId);
void writeEntityId(CborEncoder *enc, EntityId entityId);

} // end of namespacd cdb

extern const uint64_t ctEntityId;
#endif
