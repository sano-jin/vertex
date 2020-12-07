module Vis.DGVis where

import           Graphics.Gloss
-- import           Graphics.Gloss.Data.Color
-- import           Graphics.Gloss.Interface.IO.Game
import           Vis.DGraph                     ( DGraph
                                                , DNode(..)
                                                , outgoingPosesOfNode
                                                , elems
                                                )
-- import qualified Data.Map.Strict               as M
import           Vis.Geom

radius :: Float
radius = 10

circleNode :: Picture
circleNode = circleSolid radius

dGraph2Picture :: DGraph String Float -> Picture
dGraph2Picture dGraph = Pictures
  [ Pictures . map (dNode2PictureArrow dGraph) . elems $ dGraph
  , Pictures . map dNode2PictureNode . elems $ dGraph
  ]

triangle :: Picture
triangle = polygon [(-8, 0), (0, 15), (8, 0)]

arrow :: V2 Float -> V2 Float -> Picture
arrow start end =
  let vec  = end ^-^ start
      end2 = end ^-^ direct 30.0 vec
      arrowHead
        = applyV2 translate end2 $ rotate (toDeg $ angleH vec) triangle
  in  color (greyN 0.7)
      $ Pictures [line [pos2Tup start, pos2Tup end2], arrowHead]

dNode2PictureNode :: DNode String Float -> Picture
dNode2PictureNode (Node a _ (pos, _)) = applyV2 translate pos
  $ Pictures [color aquamarine circleNode, scale 0.2 0.2 $ text a]

dNode2PictureArrow :: DGraph String Float -> DNode String Float -> Picture
dNode2PictureArrow dGraph node@(Node _ _ (pos, _)) =
  Pictures $ map (arrow pos) $ outgoingPosesOfNode dGraph node

