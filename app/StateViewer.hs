module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Vis.DGVis
import           Vis.DGraph
import           System.Environment
import           Compiler.Compiler              ( compile )
import           Compiler.Normalize             ( normalize )
import           VM.VM                          ( State(..)
                                                , initializeHeap
                                                , reduce
                                                , state2DGraph
                                                )
import           System.Random
import           ND                             ( readAndVisND )

-- | Setting the display
windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "DHLMNtal Visualizer" (windowWidth, windowHeight) (100, 100)

main :: IO ()
main = do
  [f] <- getArgs
  s   <- readFile f
  g   <- getStdGen
  displayStates g =<< readAndVisND show s

displayStates :: RandomGen g => g -> DGraph String Float -> IO ()
displayStates g dGraph = simulate
  window
  white
  24
  (randamizeDGraph (windowWidth * 0.3) (windowHeight * 0.3) g dGraph)
  (\(_, dGraph) -> dGraph2Picture dGraph)
  (\_ timeDiff (g', dGraph) -> (g', updateDGraph timeDiff dGraph))


