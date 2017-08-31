{-
	GUI: DropDownList functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/DropDownList.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.DropDownList
where

import Fresco                                                                                                            
import Data.Binary.Serialise.CBOR                                                                                        
import Data.Binary.Serialise.CBOR.Encoding                                                                               
import Data.Binary.Serialise.CBOR.Decoding                                                                               
                                                                                                                         
import Data.Text                                                                                                         
import Data.Monoid                                                                                                       
import Control.Applicative                                                                                               
                                                                                                                         
                                                                                                                         
data TextSelection = Selection Int                                                                                       
    | NoSelection                                                                                                        
    deriving (Eq, Read, Show)                                                                                            
                                                                                                                         
data DropDownList = DropDownList {                                                                                       
    dropDownListContent::[Text],                                                                                         
    dropDownListSelected::TextSelection                                                                                  
    } deriving (Eq, Read, Show)                                                                                          
                                                                                                                         
ctDropDownList :: ComponentType DropDownList                                                                             
ctDropDownList = ComponentType 0x200de0e837a8e590                                                                        
                                                                                                                         
instance Serialise TextSelection where                                                                                   
    encode (Selection v1) = encodeListLen 2 <>  encode (0::Int) <> encode v1                                             
    encode (NoSelection) = encodeListLen 1 <>  encode (1::Int)                                                           
    decode = do                                                                                                          
        decodeListLen                                                                                                    
        i <- decode :: Decoder s Int                                                                                       
        case i of                                                                                                        
            0 -> (Selection <$> decode)                                                                                  
            1 -> (pure NoSelection)                                                                                      
                                                                                                                         
instance Serialise DropDownList where                                                                                    
    encode (DropDownList v1 v2) = encodeListLen 2 <> encode v1 <> encode v2                                              
    decode = decodeListLenOf 2 >> DropDownList <$> decode <*> decode                                                                                                                                          
                                                                                         

