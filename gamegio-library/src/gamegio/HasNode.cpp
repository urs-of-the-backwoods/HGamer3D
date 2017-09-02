//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/HasNode/graphics3d.cpp

#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "HasNode.hpp"
#include "Graphics3DSystem.hpp"
#include "Urho3D/Math/Quaternion.h"

#include "Vec3Cbor.hpp"
#include "UnitQuaternionCbor.hpp"
#include "EntityIdCbor.hpp"
#include "ParentCbor.hpp"

using namespace std;
using namespace cbd;

// Orientation, Position, Scale

// HasNode::HasNode(Graphics3DSystem *g)
HasNode::HasNode()
{
  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  node = g3ds->scene->CreateChild();
}

HasNode::~HasNode()
{
  node->Remove();
}

void HasNode::msgOri(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::UnitQuaternion uq;
  readUnitQuaternion(&it, &uq);
  node->SetRotation(Quaternion(uq.w, uq.x, uq.y, uq.z));
};

void HasNode::msgPos(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  Vec3 vec;
  readVec3(&it, &vec);
  node->SetPosition(Vector3(vec.x, vec.y, vec.z));
};

void HasNode::msgScale(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  Vec3 vec;
  readVec3(&it, &vec);
  node->SetScale(Vector3(vec.x, vec.y, vec.z));
};

void printEID(EntityId eid)
{
  for(int j = 0; j < 16; j++)
    printf("%02X", eid[j]);
  cout << "\n";
}

void HasNode::msgParent(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  cbd::EntityId eid;
  readEntityId(&it, &eid);

  // set new parent
  Graphics3DSystem* g3ds = Graphics3DSystem::getG3DS();

//  std::cout << "HasNode - msgParent: ";
//  printEID(eid);

  std::map<EntityId, Node*>::iterator newParent = g3ds->node_map.find(eid);

  if (newParent != g3ds->node_map.end()) {
    Node* oldParent = node->GetParent();
    if (oldParent) {
      oldParent->RemoveChild(node);
    }
    newParent->second->AddChild(node);
  }
  else {
//    std::cout << "HasNode-msgParent: parent id not found";
  }
}

void HasNode::msgEntityId(FrMsg m, FrMsgLength l)
{
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  EntityId eid;
  readEntityId(&it, &eid);
//  std::cout << "HasNode - set id: ";
//  printEID(eid);

  Graphics3DSystem::getG3DS()->node_map[eid] = node;
}

