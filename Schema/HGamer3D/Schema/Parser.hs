-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
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

-- Parser.hs

-- | The Parser to implement a DSL for describing the Scene inclusive all nodes in text
module HGamer3D.Schema.Parser (
parseScene
) 

where

import qualified HGamer3D.Data as Dat
import qualified Data.Map as Map

import qualified HGamer3D.Schema.Material as Mat
import qualified HGamer3D.Schema.Texture as Tex
import qualified HGamer3D.Schema.Colour as Col
import qualified HGamer3D.Schema.Light as Li
import qualified HGamer3D.Schema.Camera as Cam
import qualified HGamer3D.Schema.Scene as Sc
import qualified HGamer3D.Schema.Geometry as Geo
import qualified HGamer3D.Schema.Node as No


import Text.Parsec hiding (State)
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Combinator
import Text.Parsec.Indent

import Control.Monad.State
import Control.Lens

-- import HGamer3D.Graphics3D.Base
-- import HGamer3D.Graphics3D.Light
-- import HGamer3D.Graphics3D.Shapes
-- import HGamer3D.Graphics3D.PlatonShapes

{--

The parser for generating a Node in the code, the syntax resembles the following example:

Scene

  coordinate YUpRight
  AmbientLight
    colour <name>
    ...

  position 0.0 3.0 2.0
  orientation 0.2 0.1 0.2
  size 1.0 1.0 1.0

  Material <name>
    resource "material resource"

  Colour <name>
    rgb 0.0 2.3 3.0 | pink ...
    alpha 0.3

  Texture <name>
    resource "Texture resource"

  Children

    Camera
      position 0.0 3.0 2.0
      orientation 0.2 0.1 0.2 30.0
      Frustum 
        nearclip 0.3
        farclip 10.0
        fov 0.3
        
    PointLight
      colour <name>      

    ResourceMesh
      material <name>
      colour <name>
      resource "resource x"
      texture <name>

    Line
      colour <name>
      start 0.0 1.0 0.0
      end 10.0 10.0 10.0

    Cuboid


    Cube
      
  
--}

-- we need separate types for this tree to parse them correctly
---------------------------------------------------------------

data ParseContent =   PCPosition Dat.Vec3
                    | PCSize Dat.Vec3
                    | PCOrientation Dat.UnitQuaternion
                    | PCCoordinate Sc.CoordinateSystem
                    | PCNear Float
                    | PCFar Float
                    | PCFoV Dat.Angle  
                    | PCAlpha Float
                    | PCRGB Float Float Float
                    | PCResource String
                      
                    | PCMaterial Mat.MaterialSlot Mat.Material
                    | PCTexture Tex.TextureSlot Tex.Texture
                    | PCColour Col.ColourSlot Dat.Colour 
                    
                    | PCFrustumBlock Cam.Frustum
                    
                    | PCNode No.Node
                    | PCChildren [No.Node] 
                      
type IParser a = ParsecT String () (State SourcePos) a

-- float parsing 
----------------

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer               

-- parsing all Node specific elements (there is no "Node" blok or line, to be parsed)
-------------------------------------------------------------------------------------

aNodeLine :: IParser ParseContent
aNodeLine = aPosition
            <|> aOrientation
            <|> aSize
            <|> aNodeBlock
            
aSize :: IParser ParseContent
aSize = do
  string "size" >> spaces
  vec3 <- aVec3
  return $ PCSize vec3
  
aOrientation :: IParser ParseContent
aOrientation = do
  string "orientation"
  spaces
  vec3 <- aVec3
  spaces
  rotation <- float
  return $ PCOrientation (Dat.rotU vec3 rotation)
  
aPosition :: IParser ParseContent
aPosition = do
  string "position"
  spaces
  vec3 <- aVec3
  return $ PCPosition vec3
  
aVec3 :: IParser Dat.Vec3
aVec3 = do
  x <- float
  spaces
  y <- float
  spaces
  z <- float
  return $ Dat.Vec3 x y z
  
aNodeBlock :: IParser ParseContent
aNodeBlock = aChildrenBlock
             <|> aTextureBlock
             <|> aColourBlock
             <|> aMaterialBlock
             
aChildrenBlock :: IParser ParseContent
aChildrenBlock = spaces *> (withBlock makeChildren (string "Children") aChildrenLine) 
                 
aChildrenLine :: IParser ParseContent
aChildrenLine = aCameraBlock
--                <|> aPointLightBlock
--                <|> aSpotLighBlock
--                <|> aResourceMeshBlock
--                <|> aCubeLine
--                <|> aSphereLine
--                <|> aPlaneLine
--                <|> aLineLine

makeChildren :: String -> [ParseContent] -> ParseContent 
makeChildren _ lines = let
  nodes = []
  nodes' = foldl mfun nodes lines where  
        mfun n line = case line of
          PCNode no ->  (no : n) 
  in PCChildren nodes'

defaultNode :: No.Node
defaultNode = No.Node Dat.vec3Zero Dat.vec3U Dat.unitU (Map.fromList []) (Map.fromList []) (Map.fromList []) [] []

makeNode :: [ParseContent] -> No.Node
makeNode lines = let
  node = defaultNode
  node' = foldl mfun node lines where  
        mfun n line = case line of
          PCPosition p -> (No.position .~ p) n 
          PCSize s -> (No.size .~ s) n
          PCOrientation o -> (No.orientation .~ o) n
          
          PCMaterial name mat -> (No.materials . at name ?~ mat) n
          PCTexture name tex -> (No.textures . at name ?~ tex) n
          PCColour name col ->  (No.colours . at name ?~ col) n
          
          PCChildren nodes -> (No.children .~ nodes) n
          _ -> n   -- this function is used with lines, which contain more than Node lines !
  in node'
   
-- Node specific, parse the material type
  
aMaterialBlock = aResourceMaterialBlock

aResourceMaterialBlock :: IParser ParseContent
aResourceMaterialBlock = do
  spaces
  mat <- withBlock makeResourceMaterial (aSpecificSlot "Material") aResourceLine
  return mat

makeResourceMaterial :: String -> [ParseContent] -> ParseContent
makeResourceMaterial slotname lines = let
  PCResource res = lines !! 0
  in PCMaterial slotname (Mat.ResourceMaterial res)
  
-- Node specific, parse the colour type
  
aColourBlock :: IParser ParseContent
aColourBlock = do
  spaces
  col <- withBlock makeColour (aSpecificSlot "Colour") aColourLine
  return col
  
aColourLine =  aAlpha
               <|> aRGB
--               <|> aNamedColour
  
aAlpha :: IParser ParseContent
aAlpha = do
  string "alpha"
  spaces
  alpha <- float
  return $ PCAlpha alpha
  
aRGB :: IParser ParseContent
aRGB = do
  string "rgb"
  spaces
  r <- float
  spaces
  g <- float
  spaces
  b <- float
  return $ PCRGB r g b

defaultColour = Dat.Colour 1.0 1.0 1.0 1.0

makeColour :: String -> [ParseContent] -> ParseContent
makeColour slotname lines = let
  colour = defaultColour
  colour' = foldl mfun colour lines where
        mfun c line = let
          Dat.Colour r g b a = c
          in case line of 
            PCAlpha a' -> Dat.Colour r g b a'                 
            PCRGB r' g' b' -> Dat.Colour r' g' b' a
            _ -> c
  in PCColour slotname colour'
  

  
-- Node specific, parse the texture type

aTextureBlock = aResourceTextureBlock

aResourceTextureBlock :: IParser ParseContent
aResourceTextureBlock = do
  spaces
  tex <- withBlock makeResourceTexture (aSpecificSlot "Texture") aResourceLine
  return $ tex

aSpecificSlot :: String -> IParser String
aSpecificSlot slotname = do
  string slotname
  spaces
  name <- many1 (oneOf (['a'..'z']++['A'..'Z']))
  return name

aResourceLine :: IParser ParseContent
aResourceLine = do
  spaces
  string "resource"
  spaces
  char '"'
  name <- many1 (Text.Parsec.noneOf "\"")
  char '"'
  return $ PCResource name
  
makeResourceTexture :: String -> [ParseContent] -> ParseContent
makeResourceTexture slotname lines = let
  PCResource res = lines !! 0
  in PCTexture slotname (Tex.ResourceTexture res)

-- parse the scene type
-----------------------

parseScene input = runIndent "" $ runParserT aScene () "" input

defaultScene :: Sc.Scene
defaultScene = Sc.Scene Sc.YUpRight

aScene :: IParser No.Node
aScene = spaces *> (withBlock makeSceneNode (string "Scene" <* spaces) aSceneLine ) 

makeSceneNode :: String -> [ParseContent] -> No.Node
makeSceneNode _ lines = let
  scene = defaultScene
  scene' = foldl mfun scene lines where
        mfun s line = case line of 
          PCCoordinate c -> (Sc.coordinate .~ c) s
          _ -> s
  node = makeNode lines  
  node' = (No.attributes .~ [No.SceneAttribute scene']) node
  in node'
  
-- be careful, lines in "withBlock"s need spaces at the end, not at the beginning     
-- since block function checks each line position before (!) parsing the line!

aSceneLine :: IParser ParseContent
aSceneLine = (aCoordinate 
             <|> aNodeLine) <* spaces

aCoordinate :: IParser ParseContent
aCoordinate = do
  string "coordinate"
  spaces
  typStr <- string "YUpRight" <|> string "YUpLeft" <|> string "ZUpRight"
  let coord = case typStr of
        "YUpRight" -> Sc.YUpRight
        "YUpLeft" -> Sc.YUpLeft
        "ZUpRight" -> Sc.ZUpRight
  return $ PCCoordinate coord



-- parse the camera type
------------------------

defaultCamera :: Cam.Camera
defaultCamera = Cam.Camera defaultFrustum

aCameraBlock :: IParser ParseContent
aCameraBlock = spaces *> (withBlock makeCameraNode (string "Camera") aCameraLine) 

aCameraLine :: IParser ParseContent
aCameraLine = aFrustumBlock
             <|> aPosition            -- similar to aNodeLine, but without size, makes no sense for Camera
             <|> aOrientation
             <|> aNodeBlock
  
makeCameraNode :: String -> [ParseContent] -> ParseContent
makeCameraNode _ lines = let
  camera = defaultCamera
  camera' = foldl mfun camera lines where
        mfun cam line = case line of 
          PCFrustumBlock fr -> (Cam.frustum .~ fr) cam
  node = makeNode lines  
  node' = (No.attributes .~ [No.CameraAttribute camera']) node
  in PCNode node'
  
-- parse the frustum type
-------------------------

defaultFrustum :: Cam.Frustum
defaultFrustum = Cam.Frustum 0.0 10000.0 (Dat.Deg 30.0)

aFrustumBlock :: IParser ParseContent
aFrustumBlock = spaces *> (withBlock makeFrustum (string "Frustum") aFrustumLine) 

aFrustumLine :: IParser ParseContent
aFrustumLine = aNearLine
               <|> aFarLine
               <|> aFoVLine
               
aNearLine :: IParser ParseContent
aNearLine = do
  string "nearclip"
  spaces
  clip <- float
  return $ PCNear clip

aFarLine :: IParser ParseContent
aFarLine = do
  string "farclip"
  spaces
  clip <- float
  return $ PCFar clip

aFoVLine :: IParser ParseContent
aFoVLine = do
  string "fov"
  spaces
  fov <- float
  return $ PCFoV (Dat.Rad fov)
  
makeFrustum :: String -> [ParseContent] -> ParseContent
makeFrustum _ lines = let
  frustum = defaultFrustum
  frustum' = foldl mfun frustum lines where
        mfun f line = case line of 
          PCNear n -> (Cam.nearDistance .~ n) f
          PCFar fa -> (Cam.farDistance .~ fa) f
          PCFoV fov -> (Cam.fieldOfViewHorizontal .~ fov) f
  in PCFrustumBlock frustum'


