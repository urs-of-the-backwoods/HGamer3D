#ifndef __Alignment_cbor__
#define __Alignment_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    HALeft = 0,
    HACenter = 1,
    HARight = 2,
} EnumHorizontalAlignment;

typedef struct {
    EnumHorizontalAlignment selector;
    struct {
        struct {
        } HALeft;
        struct {
        } HACenter;
        struct {
        } HARight;
    } data;
} HorizontalAlignment;

typedef enum {
    VATop = 0,
    VACenter = 1,
    VABottom = 2,
} EnumVerticalAlignment;

typedef struct {
    EnumVerticalAlignment selector;
    struct {
        struct {
        } VATop;
        struct {
        } VACenter;
        struct {
        } VABottom;
    } data;
} VerticalAlignment;

typedef struct {
    HorizontalAlignment horizontal;
    VerticalAlignment vertical;
} Alignment;

void readHorizontalAlignment(CborValue *it, HorizontalAlignment *horizontalAlignment);
void writeHorizontalAlignment(CborEncoder *enc, HorizontalAlignment horizontalAlignment);
void readVerticalAlignment(CborValue *it, VerticalAlignment *verticalAlignment);
void writeVerticalAlignment(CborEncoder *enc, VerticalAlignment verticalAlignment);
void readAlignment(CborValue *it, Alignment *alignment);
void writeAlignment(CborEncoder *enc, Alignment alignment);

} // end of namespacd cdb

extern const uint64_t ctAlignment;

#endif
