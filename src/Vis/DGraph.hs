{-# LANGUAGE RankNTypes #-}

module Vis.DGraph (
  randamizeDGraph,
  Edge,
  DGraph,
  DNode (..),
  map2DGraph,
  updateDGraph,
  -- updatePosAndVel,
  posOfNode,
  ) where

-- import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
-- import           Data.Tuple.Extra
import           System.Random
import           Vis.Geom

type Edge = Int

type DGraph a s = Num s => M.Map Edge (DNode a s)
data DNode a s = Node a (S.Set Edge, S.Set Edge) (V2 s, V2 s)
               -- ^ data,
               -- the tuple of the set of the incomming links and the set of the outgoing links
               -- and the tuple of position and velocity.


initialPosAndVel :: Floating s => (V2 s, V2 s)
initialPosAndVel = (zero, zero)

-- | A helper function for the `map2DGraph`
collectEdges :: Edge -> (a, [Edge]) -> M.Map Edge (S.Set Edge) -> M.Map Edge (S.Set Edge)
collectEdges incommingEdge (_, outgoingEdges) mapping
  = foldl (flip $ flip (M.insertWith S.union) $ S.singleton incommingEdge) mapping outgoingEdges

-- | Translate map to Directed Graph
map2DGraph :: Floating s =>
              M.Map Edge (a, [Edge]) -> DGraph a s
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


springConstance, frictionCoefficient, mass, coulombConstance, naturalSpringLength :: Floating s => s
springConstance = 100.0
frictionCoefficient = 0.95
mass = 1.0
coulombConstance = 50.0
naturalSpringLength = 50.0


-- type State a = (DGraph a, PVMap)

{--|
-- | Caluculate the physical energy
phyEnergy :: V2D -> Double
phyEnergy velocity
  = 0.5 * mass * (normSq velocity)
|--}

-- | Randomize the position of the node
--   (Probably) should be rewritten with monad
randomizePosOfNode :: (Floating s, Random s, RandomGen g) =>
                      s -> s -> g -> DNode node s -> (g, DNode node s)
randomizePosOfNode width height g (Node a edges (_, vel))
  = let (x, g') = randomR (0, width) g
        (y, g'') = randomR (0, height) g'
       in (g'', Node a edges $ (V2 x y, vel))

-- | Randamize the position of the nodes of the given graph
randamizeDGraph :: (Floating s, Random s, RandomGen g) =>
                   s -> s -> g -> M.Map k (DNode node s)
                -- -> (g, M.Map k (DNode node s))
                -> M.Map k (DNode node s)
randamizeDGraph width height g oldGraph
  = snd $ M.mapAccum (randomizePosOfNode width height) g oldGraph

posOfNode :: DNode a s -> V2 s
posOfNode (Node _ _ (pos, _)) = pos
{--|
velOfNode (Node _ _ (_, vel)) = vel
|--}



-- | UpdatePosAndVel takes the `time difference` ,the old graph and the old physical energy
--   and returns the new graph and the updated physical energy,
--   which are based on the simulation on the _spring model_.
updatePosAndVel :: (Eq s, Floating s) =>
                   s -> DGraph a s -> s -> DNode a s -> (s, DNode a s)
updatePosAndVel timeDiff oldDGraph oldEnergy (Node a (inEdges, outEdges) (oldPos, oldVel))
  = let forceOfSpring edges accForce 
          = S.fold
            (\edge
              -> let pos2     = posOfNode $ oldDGraph M.! edge
                     pDiff    = pos2 ^-^ oldPos
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
                     pDiff    = pos2 ^-^ oldPos
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
      (newEnergy, Node a (inEdges, outEdges) (newPos, newVel))

-- | (ViewPort -> Float -> model -> model)
--   現在の状態を次の状態に発展させる関数
--   引数としてビューポートと前のステップから経過した時間が与えられる

updateDGraph :: (Eq s, Floating s) =>
                s -> DGraph a s -> DGraph a s
updateDGraph timeDiff oldDGraph
  = snd
    $ M.mapAccum (updatePosAndVel timeDiff oldDGraph) 0 oldDGraph
