-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
--
-- Licensed under the Apache License, VersRaion 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


-- Animated-Lights.hs

module Main where

import HGamer3D.APIs.One

import HGamer3D.Bindings.Ogre.ClassNode
import HGamer3D.Bindings.Ogre.ClassSceneNode

import HGamer3D.Bindings.Ogre.ClassAnimationState
import HGamer3D.Bindings.Ogre.ClassAnimation
--import HGamer3D.Bindings.Ogre.ClassTransformKeyFrame
import HGamer3D.Bindings.Ogre.ClassNodeAnimationTrack
import HGamer3D.Bindings.Ogre.ClassEntity
import HGamer3D.Bindings.Ogre.ClassControllerManager
import HGamer3D.Bindings.Ogre.ClassSceneManager

import HGamer3D.Bindings.Ogre.ClassLight

import HGamer3D.Bindings.Ogre.EnumLightType
import HGamer3D.Bindings.Ogre.EnumAnimationInterpolationMode

import HGamer3D.Bindings.Ogre.ClassBillboard
import HGamer3D.Bindings.Ogre.ClassBillboardSet

stepFunc es vs time anims = do
	ti <- getTime es
	setAnimationTime (Animation anims) ti
	return (True, anims)

main = do

	-- initialize
	(es, vs) <- initializeHG3D "HGamer3D Example" "OctreeSceneManager"
	
--	let (EngineStatus root sm rgm inman  renderw cam viewp keybd) = es
	let sm = (esSceneManager es)
	let cm = (esControllerManager es)
	
	
	-- create a light
	
	light <- cSmCreateLight (esSceneManager es) "AnimLight"
	cLSetType light LtSpotlight
	let ltc = Colour 0.25 0.25 0.0 1.0
	cLSetDiffuseColour2 light ltc
	cLSetSpecularColour2 light ltc
	cLSetAttenuation light 8000.0 1.0 0.0005 0.0
	cLSetSpotlightRange light (Radians 60) (Radians 70) 1.0
	let vn = vector3 (0.0) (-1.0) (0.0)
	cLSetDirection2 light vn
	
	-- create Billboard
	cSmSetAmbientLight sm (Colour 1.0 1.0 1.0 1.0)
	bs <- cSmCreateBillboardSet sm "bbs1" 20
	
	cBsSetMaterialName bs "Examples/Flare" "General"
	bb <- cBsCreateBillboard bs (vector3 0 0 0) (Colour 0.2 0.2 0.7 1.0) 
	
	-- create corresponding Node
	rn <- cSmGetRootSceneNode sm
	let vzero = vector3 0.0 0.0 0.0
	let qident = quaternion 1.0 0.0 0.0 0.0
	ln <- cSnCreateChildSceneNode rn vzero qident
	cSnAttachObject ln light
	cSnAttachObject ln bs
	

	-- camera position
	let pos = vector3 0.0 0.0 (-8.0)
	setCameraPos vs pos
	let at = vector3 0.0 0.0 (300.0)
	setCameraLookAt vs at
	
	-- define light
--	let col = ColourValue 0.5 0.5 0.5 1.0
--	light2 <- createLight es col 20.0 80.0 50.0 "MainLight"
	
	-- light node scale and position
	let scale = 0.5
	let vscale = vector3 scale scale scale
	cNSetScale ln vscale
	
	-- define coords
	let x = 20.0
	let y = 20.0
	let z = 100.0
	
	cNSetPosition ln (vector3 x y z)
	
	let duration = 4.0
	let step = duration / 4.0

	-- create the animation, then node track
	anim <- cSmCreateAnimation sm "LightAnim" duration
	cAnSetInterpolationMode anim ImSpline
	track <- cAnCreateNodeTrack2 anim 0 ln
	
	-- create the keyframes for the track
	keyf <- cNoantCreateNodeKeyFrame track (0.0*step)
	cTkfSetTranslate keyf (vector3 (-x) (-y) z)
	cTkfSetScale keyf vscale
	keyf <- cNoantCreateNodeKeyFrame track (1.0*step)
	cTkfSetTranslate keyf (vector3 (-x) y z)
	cTkfSetScale keyf vscale
	keyf <- cNoantCreateNodeKeyFrame track (2.0*step)
	cTkfSetTranslate keyf (vector3 x y z)
	cTkfSetScale keyf vscale
	keyf <- cNoantCreateNodeKeyFrame track (3.0*step)
	cTkfSetTranslate keyf (vector3 x (-y) z)
	cTkfSetScale keyf vscale
	keyf <- cNoantCreateNodeKeyFrame track (4.0*step)
	cTkfSetTranslate keyf (vector3 (-x) (-y) z)
	cTkfSetScale keyf vscale
	
	anims <- cSmCreateAnimationState sm "LightAnim"
	cAnsSetEnabled anims True
	cAnsSetLoop anims True

	time <- getTime es
	renderLoop es vs time anims stepFunc
		
	

 
