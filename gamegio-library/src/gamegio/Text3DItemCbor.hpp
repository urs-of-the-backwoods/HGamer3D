#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    FCNone = 0,
    FCRotateXYZ = 1,
    FCRotateY = 2,
    FCLookatXYZ = 3,
    FCLookatY = 4,
    FCLookatMixed = 5,
    FCDirection = 6,
} EnumFaceCameraMode;

typedef struct {
    EnumFaceCameraMode selector;
    struct {
        struct {
        } FCNone;
        struct {
        } FCRotateXYZ;
        struct {
        } FCRotateY;
        struct {
        } FCLookatXYZ;
        struct {
        } FCLookatY;
        struct {
        } FCLookatMixed;
        struct {
        } FCDirection;
    } data;
} FaceCameraMode;

typedef struct {
    std::string Font;
    int32_t FontSize;
    FaceCameraMode FCMode;
    bool FixedScreenSize;
} Text3D;

void readFaceCameraMode(CborValue *it, FaceCameraMode *faceCameraMode);
void writeFaceCameraMode(CborEncoder *enc, FaceCameraMode faceCameraMode);
void readText3D(CborValue *it, Text3D *text3D);
void writeText3D(CborEncoder *enc, Text3D text3D);

} // end of namespacd cdb

extern const uint64_t ctText3D;
