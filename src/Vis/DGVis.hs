module Vis.DGVis where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Interface.IO.Game
import           Vis.DGraph
import qualified Data.Map.Strict               as M
import           Vis.Geom

diameter :: Float
diameter = 10

circleNode :: Picture
circleNode = circleSolid diameter

dGraph2Picture :: M.Map Int (DNode String Float) -> Picture
dGraph2Picture dGraph = Pictures
  [ Pictures . map (dNode2PictureArrow dGraph) . M.elems $ dGraph
  , Pictures . map (dNode2PictureNode dGraph) . M.elems $ dGraph
  ]

triangle :: Picture
triangle = polygon [(-8, 0), (0, 15), (8, 0)]

arrow :: V2 Float -> V2 Float -> Picture
arrow start end =
  let vec  = end ^-^ start
      end2 = end ^-^ (direct 30.0 vec)
      head = applyV2 translate end2 $ rotate (toDeg $ angleH vec) triangle
  in  color (greyN 0.7) $ Pictures [line [pos2Tup start, pos2Tup end2], head]

dNode2PictureNode
  :: M.Map Int (DNode String Float) -> DNode String Float -> Picture
dNode2PictureNode dGraph node@(Node a _ (pos, _)) = applyV2 translate pos
  $ Pictures [color (aquamarine) $ circleNode, scale 0.2 0.2 $ text a]

dNode2PictureArrow
  :: M.Map Int (DNode String Float) -> DNode String Float -> Picture
dNode2PictureArrow dGraph node@(Node a _ (pos, _)) =
  Pictures $ map (arrow pos) $ outgoingPosesOfNode dGraph node

