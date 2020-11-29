module Vis.DGVis where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Vis.DGraph 
import qualified Data.Map.Strict               as M
import Vis.Geom

diameter :: Float
diameter = 10

circleNode :: Picture
circleNode = circleSolid diameter

{--|
dGraph2Picture :: M.Map Int (DNode String Float) -> Picture
dGraph2Picture  =
  Pictures
--  . map (flip (applyV2 translate) circleNode . )
  . map (\(a, pos) -> applyV2 translate pos $ Pictures [circleNode, scale 0.2 0.2 $ text a])
  . map (\(Node a _ (pos, _)) -> (a, pos))
  . M.elems 
|--}


dGraph2Picture :: M.Map Int (DNode String Float) -> Picture
dGraph2Picture dGraph =
  Pictures
  . map (dNode2Picture dGraph)
  . M.elems
  $ dGraph


dNode2Picture :: M.Map Int (DNode String Float) -> DNode String Float -> Picture
dNode2Picture dGraph node@(Node a _ (pos, _))
  = Pictures [ applyV2 translate pos
               $ Pictures [ circleNode
                          , scale 0.2 0.2 $ text a ]
             , Pictures 
               $ map (line . (:pos2Tup pos:[]) . pos2Tup) $ incommingPosesOfNode dGraph node
             , Pictures
               $ map (line . (:pos2Tup pos:[]) . pos2Tup) $ outgoingPosesOfNode dGraph node
             ]
