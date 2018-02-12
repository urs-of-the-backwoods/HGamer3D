//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/GeometryCbor.hpp

#ifndef __Geometry_cbor__
#define __Geometry_cbor__

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
