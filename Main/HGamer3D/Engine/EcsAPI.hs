-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2014 Peter Althainz
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

-- HGamer3D EcsAPI Module
--

-- | HGamer3D - A game engine for the Haskell Programmer, this module includes the common modules for the ECS API.
--   ECS is the Entity - Component - System

module HGamer3D.Engine.EcsAPI
(
  -- $Ecs

  -- * Component
  -- $Component
  Component,
  ComponentType (..),
  newC,
  readC,
  isTypeC,
  updateC,
  idC,
  
  
  -- * Entity
  -- $Entity
  Entity,
  entity,
  (#:),
  (#?),
  (#),

  -- * System
  -- $System
  System,
  ECSGraphics3D,
  runSystemGraphics3D,

  SomeSystem,
  (#+),
  addToWorld,
  removeFromWorld,
  
) where

import HGamer3D.Internal.Engine



{- $Ecs
An Entity-Component-System is a specific architecture for building the object model of a game engine. It has been evolved to address some shortcomings of the traditional OO style of building game objects, which led to large and inflexible object inheritance trees, which did not cover well the needs of flexible game entity organization. Interestingly whereas the traditional OO model does not have a good and usable representation in the functional world, the ECS system does, since it is founded on a data driven model, something which plays well with functional architectures.

A full explanation of ECS is beyond this documentation, you can easily find it by searching for the term in the Internet. Here we describe shortly the implementation of the ECS model in HGamer3D.

The components of an ECS are describing the basic data building blocks, from which game entities are build. Typical components for a game entity are for example its 3D picture, its position, the sounds associated with it, its animations, its hit count and health and so on. Important to acknowledge is that a component is only the data part of that, not the behaviour part as in OO objects. Due to the modular structure of components, they might be also referred to as properties. Game entities now are build, by assigning an identity to the entity and by giving it a number of components or properties. This architecture by composition gives much more flexible systems, than designing large object hierarchies. Finally the behaviour of the entities in different contexts are implemented in the so called systems.
-}

{- $Entity

-}

{- $Component
The component implements something like the most granular modifiable state in the game engine world. This state is accessible by multiple threads and it has an associated id, with which it can be uniquely identified. (The id is implemented as a StableName). Since the overall mechanism is somehow poll based, we need a mechanism to determine changed values fast, this is done by the timestamp mechanism, which allows, to detect changed values fast in exchange to a small penalty on the cost of doing a change.
-}


{- $System

-}
