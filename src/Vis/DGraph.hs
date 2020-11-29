{-# LANGUAGE RankNTypes #-}

module Vis.DGraph where

import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Tuple.Extra
import           System.Random
import           Vis.Geom

type Edge = Int

type DGraph a = M.Map Edge (DNode a)
data DNode a = Node a (S.Set Edge, S.Set Edge) (V2D, V2D)
               -- ^ data,
               -- the tuple of the set of the incomming links and the set of the outgoing links
               -- and the tuple of position and velocity.

height, width :: Double
height = 600.0
width = 400.0


initialPosAndVel :: (V2D, V2D)
initialPosAndVel = (zero, zero)



-- | A helper function for the `map2DGraph`
collectEdges :: Edge -> (a, [Edge]) -> M.Map Edge (S.Set Edge) -> M.Map Edge (S.Set Edge)
collectEdges incommingEdge (_, outgoingEdges) mapping
  = foldl (flip $ flip (M.insertWith S.union) $ S.singleton incommingEdge) mapping outgoingEdges

-- | Translate map to Directed Graph
map2DGraph :: M.Map Edge (a, [Edge]) -> DGraph a
map2DGraph mapping
  = M.mapWithKey (\key (a, outGoingEdges) ->
                     Node
                     a
                     (M.findWithDefault S.empty key incommingMap, S.fromList outGoingEdges)
                     initialPosAndVel
                 )
    mapping
  where
    incommingMap = M.foldrWithKey collectEdges M.empty mapping


springConstance, frictionCoefficient, mass, coulombConstance, naturalSpringLength :: Double
springConstance = 1.0
frictionCoefficient = 0.98
mass = 1.0
coulombConstance = 1.0
naturalSpringLength = 1.0



-- type State a = (DGraph a, PVMap)

phyEnergy :: V2D -> Double
phyEnergy velocity
  = 0.5 * mass * (normSq velocity)


randomizePos g (Node a edges (_, vel))
  = let (x, g') = randomR (0.0, width) g
        (y, g'') = randomR (0.0, height) g'
       in (g'', Node a edges $ (V2 x y, vel))

randamizeDGraph g oldGraph
  = M.mapAccum randomizePos g oldGraph

posOfNode, velOfNode :: DNode a -> V2D
posOfNode (Node _ _ (pos, _)) = pos
velOfNode (Node _ _ (_, vel)) = vel


updatePosAndVel :: Double -> DGraph a -> Double -> DNode a -> (DNode a, Double)
updatePosAndVel timeDiff oldDGraph oldEnergy (Node a (inEdges, outEdges) (oldPos, oldVel))
  = let forceOfSpring edges accForce 
          = S.fold
            (\edge
              -> let pos2     = posOfNode $ oldDGraph M.! edge
                     pDiff    = oldPos ^-^ pos2
                     d        = norm pDiff
                 in
                   (^+^) (direct (springConstance * (d - naturalSpringLength)) pDiff))
            accForce
            edges

        forceOfNode
          = forceOfSpring inEdges
            $ forceOfSpring outEdges
            $ M.foldr
            (\node
              -> let pos2     = posOfNode node
                     pDiff    = oldPos ^-^ pos2
                     d        = norm pDiff
                 in
                   (^+^) (direct (coulombConstance / (d ** 3)) pDiff)
            )
            zero
            oldDGraph
        -- ノード１の速度 := (ノード1の速度 +　微小時間 * 力 / ノード1の質量) * 減衰定数
        newVel = frictionCoefficient *^ (oldVel ^+^ ((timeDiff / mass) *^ forceOfNode))
        -- ノード１の位置 := ノード1の位置 + 微小時間 * ノード1の速度
        newPos = oldPos ^+^ (timeDiff *^ newVel)
        -- 運動エネルギーの合計 := 運動エネルギーの合計 + ノード1の質量 * ノード1の速度 ^ 2
        newEnergy = oldEnergy + mass * normSq newVel
    in
      (Node a (inEdges, outEdges) (newPos, newVel), newEnergy)
  

{--|
simulate :: Float -> State a -> State a
simulate timeDiff (oldDGraph, oldPVMap)
  = 
|--}
