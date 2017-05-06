#ifndef __KeyEvent_cbor__
#define __KeyEvent_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    int32_t key;
    int32_t scancode;
    std::string name;
} KeyData;

typedef enum {
    NoKeyEvent = 0,
    KeyUpEvent = 1,
    KeyDownEvent = 2,
} EnumKeyEvent;

typedef struct {
    EnumKeyEvent selector;
    struct {
        struct {
        } NoKeyEvent;
        struct {
            KeyData value0;
        } KeyUpEvent;
        struct {
            KeyData value0;
        } KeyDownEvent;
    } data;
} KeyEvent;

void readKeyData(CborValue *it0, KeyData *keyData);
void writeKeyData(CborEncoder *enc0, KeyData keyData);
void readKeyEvent(CborValue *it0, KeyEvent *keyEvent);
void writeKeyEvent(CborEncoder *enc0, KeyEvent keyEvent);

} // end of namespacd cdb

extern const uint64_t ctKeyEvent;
#endif
