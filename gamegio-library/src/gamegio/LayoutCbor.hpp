#ifndef __Layout_cbor__
#define __Layout_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

#include <ScreenRectCbor.hpp>

namespace cbd {

typedef enum {
    LMFree = 0,
    LMHorizontal = 1,
    LMVertical = 2,
} EnumLayoutMode;

typedef struct {
    EnumLayoutMode selector;
    struct {
        struct {
        } LMFree;
        struct {
        } LMHorizontal;
        struct {
        } LMVertical;
    } data;
} LayoutMode;

typedef struct {
    LayoutMode mode;
    int32_t spacing;
    ScreenRect2 borders;
} Layout;

void readLayoutMode(CborValue *it, LayoutMode *layoutMode);
void writeLayoutMode(CborEncoder *enc, LayoutMode layoutMode);
void readLayout(CborValue *it, Layout *layout);
void writeLayout(CborEncoder *enc, Layout layout);

} // end of namespacd cdb

extern const uint64_t ctLayout;
#endif
