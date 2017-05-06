#ifndef __DropDownList_cbor__
#define __DropDownList_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Just = 0,
    Nothing = 1,
} EnumMaybeInt;

typedef struct {
    EnumMaybeInt selector;
    struct {
        struct {
            int32_t value0;
        } Just;
        struct {
        } Nothing;
    } data;
} MaybeInt;

typedef struct {
    std::vector<std::string> content;
    MaybeInt selected;
} DropDownList;

void readMaybeInt(CborValue *it0, MaybeInt *maybeInt);
void writeMaybeInt(CborEncoder *enc0, MaybeInt maybeInt);
void readDropDownList(CborValue *it0, DropDownList *dropDownList);
void writeDropDownList(CborEncoder *enc0, DropDownList dropDownList);

} // end of namespacd cdb

extern const uint64_t ctDropDownList;
#endif
