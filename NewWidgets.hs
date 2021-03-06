--------------------------------------------------------------------------------
-- Author:   Harley Trung
-- Contact:  github.com/harleyttd
--           twitter.com/harleytt
-- Final Project, CPSC 432, Sound Representation & Synthesis, Fall 2009 at Yale
-- Revised date: December 15, 2009
--------------------------------------------------------------------------------
module NewWidgets where
import Euterpea hiding (line)
import Euterpea.UI.Widget
import Euterpea.UI.UIMonad
import Euterpea.UI
import Euterpea.UI.SOE
import Euterpea.UI.Signal
import Data.Char

--import System.IO.Unsafe
--debug s x = unsafePerformIO (print s) `seq` x

showS :: Show a => Signal a -> UI ()
showS = display . lift1 show

titleNdisplay :: (Show a) => String -> UI (Signal a) -> UI (Signal a)
titleNdisplay str action = title str $ leftRight $ action >>= \x -> showS x >> return x

-- button that have different string when pressed or unpressed
-- TODO: Have a button that stay pressed until a done signal is sent
smartButton :: String -> String -> UI (Signal Bool)
smartButton labelUnpressed labelPressed =
  mkUI False d draw (const nullSound) (const id)
       process dup (constant ())
  where
    -- compute width and height for text placeholder
    (tw, th) = (8 * (max (length labelUnpressed) (length labelPressed)) , 16)
    (minw, minh) = (tw + padding * 2, th + padding * 2)
    -- explain this layout?
    d = Layout 1 0 0 minh minw minh
    -- draw button based on on/off state
    draw b@((x,y), (w,h)) down =
      let x' = x + (w - tw) `div` 2 + if down then 0 else -1
          y' = y + (h - th) `div` 2 + if down then 0 else -1
      in (withColor Black $ text (x', y') (if down then labelPressed else labelUnpressed)) //
         (box (if down then pushed else popped) b)
    process ctx (s, (evt, sys)) = (s', markDirty sys' (s /= s'))
      where
        (s', sys') = case evt of
          UIEvent (Button pt True down) -> case (focused, s, down) of
            (True, False, True) -> (True, sys)
            (True, True, False) -> (False, sys)
            _ -> (s, sys)
          UIEvent (MouseMove pt) -> if pt `inside` bbx
            then (s, if focused then sys else sys { nextFocus = Just myid })
            else (False, if focused then sys { focus = Nothing } else sys)
          _ -> (s, sys)
          where
            bbx = computeBBX ctx d
            myid = uid ctx
            focused = focus sys == Just myid

-- TODO: It would be cool to have textbox display gray hint text
data TextBox = TextBox { str   :: String,  -- current string value of textbox
                         prev  :: String, -- latest string input from outside, to detect changes
                         foc   :: Bool,    -- being focused or not
                         flash :: Int,   -- for blinking caret/cursor
                         pos   :: Int}   -- current pos of caret/cursor
               deriving Show

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 c lst = c:lst
insertAt i c []  = [c]
insertAt i c (x:xs) = x : insertAt (i-1) c xs

removeAt :: Int -> [a] -> [a]
removeAt 0 lst = tail lst
removeAt i (x:xs) = x : removeAt (i-1) xs
removeAt _ lst = lst

-- CREDIT: original textbox was written by Reynard Le, and I greatly improved it with:
--   * a blinking cursor/carat
--   * more efficient & streamline, less parameters, using records
--   * allows Signal String instead of String, this may make it slightly less efficient but most likely not noticeable
isAcceptable c = (ord c < 128) && (isAlphaNum c || isSpace c || isPunctuation c || isSymbol c)
textbox :: Signal String -> UI (Signal String)
textbox inputS = mkUI (TextBox "" "" False  0  0)           -- initial state
                    layout                                   -- layout
                    draw                                     -- display routine
                    (const nullSound)                        -- sound routine
                    inputInj                                 -- input injection
                    process                                  -- computation
                    proj                                     -- output projection
                    inputS                                   -- continuous input (signal)
  where
      minh = 16 + padding * 2
      layout = Layout 1 0 0 minh 8 minh
      flashFreq = 40    -- flash for 40% of of the time
      inputInj :: Signal String -> Signal TextBox -> Signal TextBox
      inputInj sS tS = lift2 aux sS tS  -- check if signal string input changes, then update internal state
        where aux s t = if s /= prev t then t{str=s, prev=s} else t
      proj x = (lift1 str x, x)         -- only project out the current string, project the whole x if debugging
      draw b@(p@(x, y), (w, h)) (TextBox str _ foc flash pos) =
          let n = (w - padding * 2) `div` 8 -- num of characters allowed
              len = length str
          in (if (flash < flashFreq && foc) -- to enable blinking, add flash here
              then (withColor Black $ text (x + pos *  8, y + padding) "|" )
              else nullGraphic)
              // withColor Black (text (x + padding, y + padding) str)
              // (box pushed b)
              // (withColor Cyan $ block b) -- will need a diff color but this for now, to differentiate from other display box
      process ctx (TextBox str prev foc flash pos, (evt, sys)) =
          (TextBox str' prev foc' flash' pos',  markDirty sys ((str, foc, pos) /= (str', foc', pos') || flash<flashFreq))
          where
              bbx@((bx, by), (bw, bh)) = computeBBX ctx layout
              len = length str
              flash' = if flash == 100 then 0 else succ flash
              foc' = case evt of UIEvent (Button pt True True) -> pt `inside` bbx
                                 _ -> foc
              (str', pos') = if (not foc') then (str, len) else case evt of
                  UIEvent (Button (px, py) True True) -> (let d = (px - bx - padding) `div` 8 in (str, max 0 (min d len)))
                  UIEvent (Key c True) -> if (isAcceptable c)
                                          then (insertAt pos c str, pos + 1)
                                          else (case c of
                                                  '\b' -> if (pos > 0) then (removeAt (pos-1) str, pos - 1) else (str, pos) -- backspace
                                                  '\0177' -> if (pos < len) then (removeAt pos str, pos) else (str, pos)    -- delete
                                                  '\0208' -> if pos > 0 then (str, pos - 1) else (str, 0)                   -- left arrow
                                                  '\0214' -> if pos < len then (str, pos + 1) else (str, len)               -- right arrow
                                                  '\0213'  -> (str, 0)
                                                  '\0200'  -> (str, len)
                                                  _ -> error "Wo, What key is that?") -- I DONT KNOW ANY KEY UNCOSIDERED. Let me know if there's any. -Harley
                  _ -> (str, pos)

-- Non signal version if the above version is slow. Dont think it's needed
{-textbox :: String -> UI (Signal String)
textbox init = mkUI (TextBox init  False  0  (length init))  -- initial state
                    layout                                   -- layout
                    draw                                     -- display routine
                    (const nullSound)                        -- sound routine
                    (const id)                               -- input injection
                    process                                  -- computation
                    proj                                     -- output projection
                    (const ())                               -- input
  where
      minh = 16 + padding * 2
      layout = Layout 1 0 0 minh 8 minh
      flashFreq = 40
      draw b@(p@(x, y), (w, h)) (TextBox str foc flash pos) =
          let n = (w - padding * 2) `div` 8 -- num of characters allowed
              len = length str
          in (if (flash < flashFreq && foc) -- to enable blinking, add flash here
              then (withColor Black $ text (x + pos *  8, y + padding) "|" )
              else nullGraphic)
              // withColor Black (text (x + padding, y + padding) str)
              // (box pushed b)
              // (withColor White $ block b)
      process ctx (TextBox str foc flash pos, (evt, sys)) =
          (TextBox str' foc' flash' pos',  markDirty sys ((str, foc, pos) /= (str', foc', pos') || flash<flashFreq))
          where
              bbx@((bx, by), (bw, bh)) = computeBBX ctx layout
              len = length str
              flash' = if flash == 100 then 0 else succ flash
              foc' = case evt of UIEvent (Button pt True True) -> pt `inside` bbx
                                 _ -> foc
              (str', pos') = if (not foc') then (str, len) else case evt of
                  UIEvent (Button (px, py) True True) -> (let d = (px - bx - padding) `div` 8 in (str, max 0 (min d len)))
                  UIEvent (Key c True) -> if (isAcceptable c)
                                          then (insertAt pos c str, pos + 1)
                                          else (case c of
                                                  '\b' -> if (pos > 0) then (removeAt (pos-1) str, pos - 1) else (str, pos) -- backspace
                                                  '\0177' -> if (pos < len) then (removeAt pos str, pos) else (str, pos)    -- delete
                                                  '\0208' -> if pos > 0 then (str, pos - 1) else (str, 0)                   -- left arrow
                                                  '\0214' -> if pos < len then (str, pos + 1) else (str, len)               -- right arrow
                                                  '\0213'  -> (str, 0)
                                                  '\0200'  -> (str, len)
                                                  _ -> error "Wo, What key is that?") -- I DONT KNOW ANY KEY UNCOSIDERED. Let me know if there's any. -Harley
                  _ -> (str, pos)

-}

-- Knob widgets, all written by Reynard Le.  I haven't reviewed these yet.
bndKnob :: RealFloat a => (a, a) -> a -> a -> a -> UI (Signal a)
bndKnob (min, max) scale initVal initAng = mkKnob v2a a2v initVal
  where
      scale' = - (abs scale) -- to use clockwise direction
      v2a val = let val' = if val < min then min else if val > max then max else val
                in (val' - initVal) / scale' + initAng
      a2v ang = let val = (ang - initAng) * scale' + initVal
                in if val < min then min else if val > max then max else val

infKnob :: RealFloat a => a -> a -> a -> UI (Signal a)
infKnob scale initVal initAng = mkKnob v2a a2v initVal
  where
      scale' = - (abs scale) -- to use clockwise direction
      v2a val = (val - initVal) / scale' + initAng
      a2v ang = (ang - initAng) * scale' + initVal

mkKnob :: RealFloat a => (a -> a) -> (a -> a) -> a -> UI (Signal a)
mkKnob val2ang ang2val val0 =
  mkUI (val0, Nothing) d draw (const nullSound) (const id)
       process proj (const ())
  where
      diameter = 60
      d = Layout 0 0 diameter diameter diameter diameter
      proj = unzipS . (lift1 temp) where temp (a, b) = (a, (a, b))
      pt2ang (cx, cy) (x, y) = alpha
          where (dx, dy) = (fromIntegral (x - cx), fromIntegral (y - cy))
                (dx', dy') = (dx, -dy) -- change to normal coordinate system
                r = sqrt (dx^2 + dy^2)
                sinA = dy' / r
                alpha' = asin sinA
                alpha = if (dx' >= 0) then alpha'
                            else (if (dy' >= 0) then (pi - alpha') else (-alpha' - pi))
      epsilon = pi / 18 -- angular toleration of 5 degrees
      fixCycle refAng ang = if (ang - refAng > pi) then (fixCycle refAng (ang - pi * 2))
                              else if (refAng - ang > pi) then (fixCycle refAng (ang + pi * 2)) else ang
      ang2line (cx, cy) rad ang = line (cx, cy) (x2, y2)
          where (cosA, sinA) = (cos ang, sin ang)
                r2 = (fromIntegral rad) * 4 / 5
                (x2, y2) = (cx + truncate (r2 * cosA), cy - truncate (r2 * sinA))
      draw b@((x, y), (w, h)) (val, _) =
          let radius = ((min w h) `div` 2) - padding * 2
              cent@(centX, centY) = (x + radius + padding, y + radius + padding)
              ang = val2ang val
          in (withColor' gray3 $ ang2line cent radius ang) //
             (withColor' gray3 $ arc (x + padding, y + padding) (x + padding + radius * 2, y + padding + radius * 2) 0 360)
      process ctx ((val, s), (evt, sys)) = ((val', s'), markDirty sys' (val /= val'))
          where
              ((val', s'), sys') = case evt of
                  UIEvent (Button pt True down) -> let captured = handleCaptured ref pt in
                      case (captured, down) of
                          (True, True) -> ((val, Just (angDif ref pt)),
                                            if focused then sys else sys {nextFocus = Just myid})
                          (_, False) -> ((val, Nothing),
                                          if focused then sys {focus = Nothing} else sys)
                          _ -> ((val, s), sys)
                  UIEvent (MouseMove pt) -> case s of
                      Just ad -> ((pt2val ref ad pt, Just ad), sys)
                      Nothing -> ((val, s), sys)
                  _ -> ((val, s), sys)
              bbx@((bx, by), (bw, bh)) = computeBBX ctx d
              rad = ((min bw bh) `div` 2) - padding * 2
              centr@(cenX, cenY) = (bx + rad + padding, by + rad + padding)
              ref = val2ang val
              angDif refAng p@(x, y)= let ang = fixCycle refAng (pt2ang centr p)
                                      in refAng - ang
              pt2val refAng aD p@(x, y) = ang2val ((fixCycle refAng (pt2ang centr p)) + aD)
              handleCaptured refAng p@(x, y)  = let aDiff = angDif refAng p
                                                in ((p `inside` bbx) && (aDiff <= epsilon) && (aDiff >= (- epsilon)))
              myid = uid ctx
              focused = focus sys == Just myid

