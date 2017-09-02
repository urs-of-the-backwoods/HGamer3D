#ifndef __Geometry_cbor__
#define __Geometry_cbor__

#include <stdint.h>
#include <stdbool.h>
#include <string>
#include "cbor.h"
#include "cborconstants_p.h"

namespace cbd {

typedef enum {
    Sphere = 0,
    Cube = 1,
    Plane = 2,
    Cylinder = 3,
    Pyramid = 4,
    Torus = 5,
} EnumShape;

typedef struct {
    EnumShape selector;
    struct {
        struct {
        } Sphere;
        struct {
        } Cube;
        struct {
        } Plane;
        struct {
        } Cylinder;
        struct {
        } Pyramid;
        struct {
        } Torus;
    } data;
} Shape;

typedef enum {
    ShapeGeometry = 0,
    ResourceGeometry = 1,
} EnumGeometry;

typedef struct {
    EnumGeometry selector;
    struct {
        struct {
            Shape value0;
        } ShapeGeometry;
        struct {
            std::string value0;
        } ResourceGeometry;
    } data;
} Geometry;

void readShape(CborValue *it0, Shape *shape);
void writeShape(CborEncoder *enc0, Shape shape);
void readGeometry(CborValue *it0, Geometry *geometry);
void writeGeometry(CborEncoder *enc0, Geometry geometry);

} // end of namespacd cdb

extern const uint64_t ctGeometry;
extern const uint64_t ctGraphicsElement;
#endif
