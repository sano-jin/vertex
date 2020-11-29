module Main where

import            Graphics.Gloss
-- import Graphics.Gloss.Interface.IO.Game
import           Vis.DGVis
import           Vis.DGraph
import           System.Environment
import           Compiler.Compiler              ( compile )
import           Compiler.Normalize             ( normalize )
-- import           Compiler.Process               ( Rule )
-- import           Data.List
-- import           Data.Maybe
-- import           System.IO
import           VM.VM                          ( State(..)
                                                , initializeHeap
 --                                               , isStateEq
--                                                , reduce
--                                                , reduceND
                                                , state2DGraph
                                                )
-- import           Control.Monad
-- import           Data.Tuple.Extra
import           System.Random


-- | Setting the display
windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "DHLMNtal Visualizer" (windowWidth, windowHeight) (100, 100)

main :: IO ()
main = do
  [f]   <- getArgs
  s     <- readFile f
  readStateAndDisplay s


readStateAndDisplay :: String -> IO ()
readStateAndDisplay input
  = case normalize =<< compile input of
      Left err -> putStrLn ("Error : " ++ show err)
      Right (procVals, rules) ->
        let initialState = State (initializeHeap procVals) rules
        in  do g <- getStdGen
               displayState initialState g
--            >> run state2String 1 initialState
            
-- displayState :: State -> g -> IO ()
displayState :: RandomGen g => State -> g -> IO ()
displayState state g
  = simulate
    window
    white
    24
    (randamizeDGraph (windowWidth * 0.3) (windowHeight * 0.3) g
     $ state2DGraph state)
    dGraph2Picture
    (const updateDGraph)
