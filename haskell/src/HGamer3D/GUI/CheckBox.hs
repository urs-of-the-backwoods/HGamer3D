{-
	GUI: Checkbox functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/CheckBox.hs
-}

-- | Module providing the CheckBox gui element
module HGamer3D.GUI.CheckBox
where 

import Fresco                                                                       
import Data.Binary.Serialise.CBOR                                                     
import Data.Binary.Serialise.CBOR.Encoding                                            
import Data.Binary.Serialise.CBOR.Decoding                                              
                                                                                         
import Data.Text                                                                          
import Data.Monoid                                                                          
import Control.Applicative                                                                    
                                                                                                
                                                                                                    
type CheckBox = Bool                                                                                   

ctCheckBox :: ComponentType CheckBox
ctCheckBox = ComponentType 0xd2425f880fcdd9a4
