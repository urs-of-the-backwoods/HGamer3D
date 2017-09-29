#ifndef __Button_cbor__
#define __Button_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef struct {
    bool pressed;
    std::string label;
} Button;

typedef enum {
    NoButtonEvent = 0,
    Pressed = 1,
    Released = 2,
} EnumButtonEvent;

typedef struct {
    EnumButtonEvent selector;
    struct {
        struct {
        } NoButtonEvent;
        struct {
        } Pressed;
        struct {
        } Released;
    } data;
} ButtonEvent;

typedef std::string BasicButton;

typedef std::string ImageButton;

void readButton(CborValue *it, Button *button);
void writeButton(CborEncoder *enc, Button button);
void readButtonEvent(CborValue *it, ButtonEvent *buttonEvent);
void writeButtonEvent(CborEncoder *enc, ButtonEvent buttonEvent);
void readBasicButton(CborValue *it, BasicButton *basicButton);
void writeBasicButton(CborEncoder *enc, BasicButton basicButton);
void readImageButton(CborValue *it, ImageButton *imageButton);
void writeImageButton(CborEncoder *enc, ImageButton imageButton);

} // end of namespacd cdb

extern const uint64_t ctButton;
extern const uint64_t ctButtonEvent;
extern const uint64_t ctBasicButton;
extern const uint64_t ctImageButton;
#endif
