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

#include "CameraItem.hpp"
#include "CameraCbor.hpp"
#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"


using namespace std;

// Camera handling

GIO_METHOD_FUNC(CameraItem, Frustum)
GIO_METHOD_FUNC(CameraItem, Pos)
GIO_METHOD_FUNC(CameraItem, Scale)
GIO_METHOD_FUNC(CameraItem, Ori)
GIO_METHOD_FUNC(CameraItem, EntityId)
GIO_METHOD_FUNC(CameraItem, Parent)

// Factory Implementation
GCO_FACTORY_IMP(CameraItem)
    GCO_FACTORY_METHOD(CameraItem, ctFrustum, Frustum)
    GCO_FACTORY_METHOD(CameraItem, ctPosition, Pos)
    GCO_FACTORY_METHOD(CameraItem, ctScale, Scale)
    GCO_FACTORY_METHOD(CameraItem, ctOrientation, Ori)
    GCO_FACTORY_METHOD(CameraItem, ctEntityId, EntityId)
    GCO_FACTORY_METHOD(CameraItem, ctParent, Parent)
GCO_FACTORY_IMP_END

CameraItem::CameraItem()
{
}

FrItem CameraItem::msgCreate(FrMsg m, FrMsgLength l)
{
  std::cout << "CameraItem::msgCreate 0\n";
  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();

  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Camera cam;
  cbd::readCamera(&it, &cam);

  CameraItem *camp = new CameraItem();

  camp->node->CreateComponent<Camera>();
  Renderer* renderer = camp->node->GetSubsystem<Renderer>();
  camp->viewportSlot = renderer->GetNumViewports();
  renderer->SetNumViewports(camp->viewportSlot+1);  

  camp->viewport = new Viewport(g3ds->context, g3ds->scene, camp->node->GetComponent<Camera>());
  renderer->SetViewport(camp->viewportSlot, camp->viewport);

  // camera is main camera, we are done
  if (cam.selector == cbd::FullViewCamera) return (FrItem)camp;

  // camera is overlay camera
  if (cam.selector == cbd::OverlayCamera)
  {
    // viewport settings
    Graphics* graphics = camp->node->GetSubsystem<Graphics>();
    int width = graphics->GetWidth();
    int height  = graphics->GetHeight();

    int left = round(cam.data.OverlayCamera.value0.x * width);
    int top = round(cam.data.OverlayCamera.value0.y * height);
    int right = left + round(cam.data.OverlayCamera.value0.width * width);
    int bottom = top + round(cam.data.OverlayCamera.value0.height * height);

    camp->viewport->SetRect(IntRect(left, top, right, bottom));
  }

  return (FrItem)camp;
}

void CameraItem::msgFrustum(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::Frustum fr;
  cbd::readFrustum(&it, &fr);

  setFrustum(fr.nearDistance, 
            fr.farDistance, 
            getAngleAsRadians(fr.fieldOfViewHorizontal) ); 

}

CameraItem::~CameraItem()
{
  // check correct order of deletion for Cameras
  Renderer* renderer = node->GetSubsystem<Renderer>();
  int vpsNow = renderer->GetNumViewports();
  if (viewportSlot != (vpsNow -1)) 
  {
    Log::Write(LOG_WARNING, "HGamer3D: camera destroyed in wrong order! If multiple cameras are created, they need to be destroyed in reverse order of creation.");
  }
  if (vpsNow > viewportSlot) {
    renderer->SetNumViewports(viewportSlot);
  }
  node->RemoveComponent<Camera>();
  delete viewport;
}

void CameraItem::msgDestroy()
{
    delete this;
}

void CameraItem::setFrustum(float nc, float fc, float fov)
{
  Camera* camera = node->GetComponent<Camera>();
  camera->SetNearClip(nc);
  camera->SetFarClip(fc);
  camera->SetFov(fov);  
}

