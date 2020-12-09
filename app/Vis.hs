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
import           Compiler.TypeCheck             ( typeCheck )

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
  readStateAndDisplay s


readStateAndDisplay :: String -> IO ()
readStateAndDisplay input = case typeCheck =<< normalize =<< compile input of
  Left err -> putStrLn ("Error : " ++ show err)
  Right (procVals, rules) ->
    let initialState = State (initializeHeap procVals) rules
    in  do
          g <- getStdGen
          displayState initialState g



displayState :: RandomGen g => State -> g -> IO ()
displayState state g = play
  window
  white
  24
  ( randamizeDGraph (windowWidth * 0.3) (windowHeight * 0.3) g
    $ state2DGraph state
  , state
  )
  (\((_, dGraph), _) -> dGraph2Picture dGraph)
  updateState
  (\timeDiff ((g', dGraph), state') ->
    ((g', updateDGraph timeDiff dGraph), state')
  )


updateState
  :: (Floating s, Random s, RandomGen g)
  => Event
  -> ((g, DGraph String s), State)
  -> ((g, DGraph String s), State)
updateState (EventKey (SpecialKey KeySpace) Down _ _) ((g, oldDGraph), oldState)
  = case reduce oldState of
      (nextState, _):_ ->
        ( ( randamizeDGraph (windowWidth * 0.3) (windowHeight * 0.3) g
            $ state2DGraph nextState
          )
        , nextState
        )
      [] -> ((g, oldDGraph), oldState)
updateState _ model = model
