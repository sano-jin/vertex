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

dGraph2Picture :: M.Map Int (DNode String Float) -> Picture
dGraph2Picture  =
  Pictures
--  . map (flip (applyV2 translate) circleNode . )
  . map (\(a, pos) -> applyV2 translate pos $ Pictures [circleNode, scale 0.2 0.2 $ text a])
  . map (\(Node a _ (pos, _)) -> (a, pos))
  . M.elems 
