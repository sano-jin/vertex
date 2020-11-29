module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

-- | Setting the display
windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "DHLMNtal Visualizer" (windowWidth, windowHeight) (100, 100)


-- | Implementing graph visualizer
type DGraph a = M.Map Int (DNode a)
type DNode a = (a, S.Set Int, S.Set Int)
               -- ^ data, the set of the incomming links, the set of the outgoing links.

boxWidth, boxHeight :: Float
boxWidth  = 50
boxHeight = 50

data BoxState = BoxState
  { _x  :: Float -- x 座標の位置
  , _y  :: Float -- y 座標の位置
  , _vx :: Float -- x 方向の速度
  , _vy :: Float -- y 方向の速度
  }


initialBox :: BoxState
initialBox = BoxState 0 0 0 0

drawBox :: BoxState -> Picture
drawBox box = translate (_x box) (_y box) $ rectangleSolid boxWidth boxHeight

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateBox :: Event -> BoxState -> BoxState
updateBox (EventKey key ks _ _) box = updateBoxWithKey key ks box
updateBox (EventMotion _)       box = box
updateBox (EventResize _)       box = box

-- | 上下左右の速度を与える関数
up, down, right, left :: BoxState -> BoxState
up    box = box { _vy = _vy box + 100 }
down  box = box { _vy = _vy box - 100 }
right box = box { _vx = _vx box + 100 }
left  box = box { _vx = _vx box - 100 }

-- | 方向キーとWASDキーに対応して四角形を移動させる
updateBoxWithKey :: Key -> KeyState -> BoxState -> BoxState
updateBoxWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else down
updateBoxWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else up
updateBoxWithKey (SpecialKey KeyRight) ks = if ks == Down then right else left
updateBoxWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else right
updateBoxWithKey (Char 'w')            ks = if ks == Down then up    else down
updateBoxWithKey (Char 's')            ks = if ks == Down then down  else up
updateBoxWithKey (Char 'd')            ks = if ks == Down then right else left
updateBoxWithKey (Char 'a')            ks = if ks == Down then left  else right
updateBoxWithKey _ _ = id

nextBox :: Float -> BoxState -> BoxState
nextBox dt box =
  let -- 速度を考慮した次のステップでの位置を計算
      x  = _x box + _vx box * dt
      y  = _y box + _vy box * dt

   in box { _x = x, _y = y }

-------------
-- main 関数
-------------

main :: IO ()
main = play window white 24 initialBox drawBox updateBox nextBox
{--|

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-------------------
-- Display の設定
-------------------

windowWidth, windowHeight :: Num a => a
windowWidth  = 640
windowHeight = 480

window :: Display
window = InWindow "Hello World" (windowWidth, windowHeight) (100, 100)

--------------------------
-- シミュレーションの実装
--------------------------

boxWidth, boxHeight :: Float
boxWidth  = 50
boxHeight = 50

data BoxState = BoxState
  { _x  :: Float -- x 座標の位置
  , _y  :: Float -- y 座標の位置
  , _vx :: Float -- x 方向の速度
  , _vy :: Float -- y 方向の速度
  }

initialBox :: BoxState
initialBox = BoxState 0 0 150 150

drawBox :: BoxState -> Picture
drawBox box = translate (_x box) (_y box) $ rectangleSolid boxWidth boxHeight

nextBox :: ViewPort -> Float -> BoxState -> BoxState
nextBox vp dt box =
  let -- 速度を考慮した次のステップでの位置を計算
      x  = _x box + _vx box * dt
      y  = _y box + _vy box * dt

      -- 壁との当たり判定
      isOverTop    = y >  (windowHeight - boxHeight) / 2
      isOverBottom = y < -(windowHeight - boxHeight) / 2
      isOverRight  = x >  (windowWidth - boxWidth) / 2
      isOverLeft   = x < -(windowWidth - boxWidth) / 2

      -- 壁と衝突していれば速度を反転させる
      vx = if isOverRight || isOverLeft   then (-_vx box) else (_vx box)
      vy = if isOverTop   || isOverBottom then (-_vy box) else (_vy box)

   in BoxState x y vx vy

-------------
-- main 関数
-------------

main :: IO ()
main = simulate window white 24 initialBox drawBox nextBox

|--}

{--|
module Main where
import           System.Environment

import           Repl
-- import           VM.VM

main :: IO ()
main = do
  (f : args) <- getArgs
  s          <- readFile f
  case args of
    ["--nd"] -> readAndRunND show s
    _        -> readAndRun show s
|--}
