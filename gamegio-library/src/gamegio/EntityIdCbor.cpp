#include "EntityIdCbor.hpp"

namespace cbd {

void readEntityId(CborValue *it, EntityId *entityId)
{
    std::vector<uint8_t> rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_byte_string(it, rval.data(), &l, NULL); cbor_value_advance(it);}
    *entityId = rval;
}

void writeEntityId(CborEncoder *enc, EntityId entityId)
{
    cbor_encode_byte_string(enc, entityId.data(), entityId.size());
}


} // end of namespacd cdb

const uint64_t ctEntityId = 0x112cc0dc2647d39e;
