//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: Urho3D-Binding/graphics3d.cpp

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "Fresco.hpp"

#include "GeometryItem.hpp"
#include "GeometryCbor.hpp"
#include "MaterialCbor.hpp"
#include "ColourCbor.hpp"
#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"

using namespace std;

GIO_METHOD_FUNC(GeometryItem, Geometry)
GIO_METHOD_FUNC(GeometryItem, Material)
GIO_METHOD_FUNC(GeometryItem, Colour)
GIO_METHOD_FUNC(GeometryItem, Pos)
GIO_METHOD_FUNC(GeometryItem, Scale)
GIO_METHOD_FUNC(GeometryItem, Ori)
GIO_METHOD_FUNC(GeometryItem, EntityId)
GIO_METHOD_FUNC(GeometryItem, Parent)

// Factory Implementation
GCO_FACTORY_IMP(GeometryItem)
    GCO_FACTORY_METHOD(GeometryItem, ctGeometry, Geometry)
    GCO_FACTORY_METHOD(GeometryItem, ctMaterial, Material)
    GCO_FACTORY_METHOD(GeometryItem, ctColour, Colour)
    GCO_FACTORY_METHOD(GeometryItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(GeometryItem, ctScale, Scale)
    GCO_FACTORY_METHOD(GeometryItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(GeometryItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(GeometryItem, ctParent, Parent)
GCO_FACTORY_IMP_END

GeometryItem::GeometryItem()
{
  material = "Materials/DefaultGrey.xml";   // default value, if no material is set
}

FrItem GeometryItem::msgCreate(FrMsg m, FrMsgLength l)
{
//  std::cout << "in msgCreate GeometryItem\n";
  GeometryItem *geo = new GeometryItem();
  geo->node->CreateComponent<StaticModel>();
  return (FrItem)geo;
}

GeometryItem::~GeometryItem()
{
  node->RemoveComponent<StaticModel>();
}

void GeometryItem::msgDestroy()
{
    delete this;
}

void GeometryItem::msgGeometry(FrMsg m, FrMsgLength l)
{
//  std::cout << "in Con GeometryItem\n";

  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Geometry geo;
  cbd::readGeometry(&it, &geo);

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  StaticModel* model = node->GetComponent<StaticModel>();
  if (geo.selector == cbd::ShapeGeometry) { // Basic Geometry
    if (geo.data.ShapeGeometry.value0.selector == cbd::Sphere) { // sphere
      model->SetModel(cache->GetResource<Model>("Models/Sphere.mdl"));
    } else if (geo.data.ShapeGeometry.value0.selector == cbd::Cube) { // cube
      model->SetModel(cache->GetResource<Model>("Models/Box.mdl"));
    } else if (geo.data.ShapeGeometry.value0.selector == cbd::Plane) { // plane
      model->SetModel(cache->GetResource<Model>("Models/Plane.mdl"));
    } else if (geo.data.ShapeGeometry.value0.selector == cbd::Cylinder) { // cylinder
      model->SetModel(cache->GetResource<Model>("Models/Cylinder.mdl"));
    } else if (geo.data.ShapeGeometry.value0.selector == cbd::Pyramid) { // pyramid
      model->SetModel(cache->GetResource<Model>("Models/Pyramid.mdl"));
    } else if (geo.data.ShapeGeometry.value0.selector == cbd::Torus) { // torus
      model->SetModel(cache->GetResource<Model>("Models/Torus.mdl"));
    }  
    model->SetMaterial(cache->GetResource<Material>(material));
  } else if (geo.selector == cbd::ResourceGeometry) { // mesh geometry
      model->SetModel(cache->GetResource<Model>(String(geo.data.ResourceGeometry.value0.c_str())));
      model->ApplyMaterialList(); // sets materials from material list file (AssetImporter)
  }
};

void GeometryItem::msgMaterial(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Material mat;
  cbd::readMaterial(&it, &mat);

  ResourceCache* cache = node->GetSubsystem<ResourceCache>();
  StaticModel* model = node->GetComponent<StaticModel>();

  if (mat.selector == cbd::ResourceMaterial) {
    material = mat.data.ResourceMaterial.value0.c_str();
//    cout << "in material: " << material.CString() << "\n";
    model->SetMaterial(cache->GetResource<Material>(material));
  }

};

void GeometryItem::msgColour(FrMsg m, FrMsgLength l)
{
  // to be implemented
}




