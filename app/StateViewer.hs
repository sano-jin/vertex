module Main where

import           Graphics.Gloss
import           Vis.DGVis
import           Vis.DGraph
import           System.Environment
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
  (dGraph2Picture . snd)
  (\_ timeDiff (g', oldDGraph) -> (g', updateDGraph timeDiff oldDGraph))


