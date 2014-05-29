// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// ClassVertexAnimationTrack.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumVertexAnimationType.h"
#include "EnumVertexAnimationTrackTargetMode.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// Constructor. 
extern "C" Ogre_LIB_EXPORT void ogre_vat_construct(struct hg3dclass_struct * parent_c, unsigned short handle_c, enum EnumVertexAnimationType animType_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * parent_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*parent_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  enum Ogre::VertexAnimationType animType_cpp = (enum Ogre::VertexAnimationType)animType_c;
  Ogre::VertexAnimationTrack * result_cpp;
  result_cpp = (new Ogre::VertexAnimationTrack(parent_cpp, handle_cpp, animType_cpp));
  *result_c = getHG3DClass_VertexAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vat_getAnimationType(struct hg3dclass_struct * thisclass_c, enum EnumVertexAnimationType * result_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  enum Ogre::VertexAnimationType result_cpp;
  result_cpp = (thisclass_cpp->getAnimationType());
  *result_c = (enum EnumVertexAnimationType) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vat_getVertexAnimationIncludesNormals(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getVertexAnimationIncludesNormals());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vat_apply(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * timeIndex_c, float weight_c, float scale_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  const Ogre::TimeIndex * timeIndex_cpp = static_cast<Ogre::TimeIndex*> (getHG3DClassPtr(*timeIndex_c, "Ogre::TimeIndex"));
  Real weight_cpp = (Real)weight_c;
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->apply(*timeIndex_cpp, weight_cpp, scale_cpp));
};

// Set the target mode. 
extern "C" Ogre_LIB_EXPORT void ogre_vat_setTargetMode(struct hg3dclass_struct * thisclass_c, enum EnumVertexAnimationTrackTargetMode m_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  enum Ogre::VertexAnimationTrack::TargetMode m_cpp = (enum Ogre::VertexAnimationTrack::TargetMode)m_c;
  (thisclass_cpp->setTargetMode(m_cpp));
};

// Get the target mode. 
extern "C" Ogre_LIB_EXPORT void ogre_vat_getTargetMode(struct hg3dclass_struct * thisclass_c, enum EnumVertexAnimationTrackTargetMode * result_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  enum Ogre::VertexAnimationTrack::TargetMode result_cpp;
  result_cpp = (thisclass_cpp->getTargetMode());
  *result_c = (enum EnumVertexAnimationTrackTargetMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vat_hasNonZeroKeyFrames(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasNonZeroKeyFrames());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vat_optimise(struct hg3dclass_struct * thisclass_c)
{
  Ogre::VertexAnimationTrack * thisclass_cpp = static_cast<Ogre::VertexAnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::VertexAnimationTrack"));
  (thisclass_cpp->optimise());
};

