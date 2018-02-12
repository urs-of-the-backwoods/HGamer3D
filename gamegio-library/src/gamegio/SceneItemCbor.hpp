#ifndef __SceneItem_CBOR__
#define __SceneItem_CBOR__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    XmlScene = 0,
    BinaryScene = 1,
} EnumScene;

typedef struct {
    EnumScene selector;
    struct {
        struct {
            std::string value0;
        } XmlScene;
        struct {
            std::string value0;
        } BinaryScene;
    } data;
} Scene;

void readScene(CborValue *it, Scene *scene);
void writeScene(CborEncoder *enc, Scene scene);

} // end of namespacd cdb

extern const uint64_t ctScene;

#endif
