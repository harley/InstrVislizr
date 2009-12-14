> {-# OPTIONS -fglasgow-exts #-}

> module NewWidgets where

> import Euterpea.UI.Signal
> import Euterpea.UI.UIMonad
> import Euterpea.UI.SOE hiding (Event)
> import qualified Euterpea.UI.SOE as SOE
> import Euterpea.UI.Widget
> import System.IO
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad.Fix
> import Control.Applicative
> import Data.Char

> uiTest = runUIEx (150, 300) "Textbox" $ do
>       label "Testing"
>       sp <- textbox "textbox"
>       title "Returned string" (display sp)
>       f1 <- infKnob (9 / pi) 0.0 (pi / 2)
>       display (lift1 show f1)
>       f2 <- bndKnob (-20.0, 20.0) (18 / pi) 0.0 (pi / 2)
>       display (lift1 show f2)

> textbox :: String -> UI (Signal String)
> textbox init = mkUI (init, False, 0, length init, 0, length init) d draw (const nullSound) (const id) process proj (const ())
>   where
>       minh = 16 + padding * 2
>       d = Layout 1 0 0 minh 8 minh
>       isAcceptable c = (ord c < 128) && (isAlphaNum c || isSpace c || isPunctuation c || isSymbol c)
>       proj = unzipS . (lift1 temp) where temp (a, b, c, d, e, f) = (a, (a, b, c, d, e, f))
>       draw b@(p@(x, y), (w, h)) (str, foc, flash, pos, spanS, spanE) =
>           let n = (w - padding * 2) `div` 8
>               len = length str
>           in (if (flash == 0 && foc == True)
>               then (withColor Black $ text (x + (pos -spanS) *  8, y + padding) "|" )
>               else nullGraphic) //
>               withColor Black (text (x + padding, y + padding) (takeMid spanS spanE str)) //
>               (box pushed b) // (withColor White $ block b)
>       process ctx ((str, foc, flash, pos, spanS, spanE), (evt, sys)) = ((str', foc', flash', pos', spanS', spanE'), markDirty sys ((str, foc, pos) /= (str', foc', pos')))
>           where
>               bbx@((bx, by), (bw, bh)) = computeBBX ctx d
>               nChar = (bw - padding * 2) `div` 8
>               len = length str
>               flash' = (flash + 1) `mod` 30
>               foc' = case evt of
>                   UIEvent (Button pt True True) -> if pt `inside` bbx then True else False
>                   _ -> foc
>               (str', pos') = if (foc' == False) then (str, len) else case evt of
>                   UIEvent (Button (px, py) True True) -> (let d = (px - bx - padding) `div` 8 in (str, spanS + (max 0 (min d spanE))))
>                   UIEvent (Key c True) -> if (isAcceptable c)
>                                               then ((take pos str) ++ [c] ++ (takeTail (len - pos) str), pos + 1)
>                                               else (case c of
>                                                   '\b' -> if (pos > 0) then -- backspace
>                                                           ((take (pos - 1) str) ++ (takeTail (len - pos) str), pos - 1)
>                                                           else (str, pos)
>                                                   '\0177' -> if (pos < len) then -- delete
>                                                             ((take pos str) ++ (takeTail (len - pos - 1) str), pos)
>                                                             else (str, pos)
>                                                   '\0208' -> if pos > 0 then (str, pos - 1) else (str, 0) -- left arrow
>                                                   '\0214' -> if pos < len then (str, pos + 1) else (str, len) -- right arrow
>                                                   _ -> (str, pos))
>                   _ -> (str, pos)
>               len' = length str'
>               (spanS', spanE') = case (signum (len' - len)) of
>                   1 -> if (pos' - spanS > nChar) then (spanS + 1, spanE + 1)
>                           else (if (spanE - spanS < nChar) then (spanS, spanE + 1) else (spanS, spanE))
>                   0 -> if (pos' < spanS) then (spanS - 1, if (spanE - spanS < nChar) then spanE else spanE - 1)
>                           else (if (pos' > spanE) then (spanS + 1, spanE + 1) else (spanS, spanE))
>                   -1 -> if (pos' < spanS) then (spanS - 1, spanE - 1)
>                            else (if (spanE > len') then (spanS, spanE - 1) else (spanS, spanE))

> takeTail :: Int -> [a] -> [a]
> takeTail n xs = let len = length xs
>                   in if (n <= len && n >= 0) then reverse $ (take n) $ reverse xs else error "takeTail error"

> takeMid :: Int -> Int -> [a] -> [a]
> takeMid start end xs = if (0 <= start && start <= end && end <= (length xs))
>                        then takeTail (end - start) (take end xs) else error "takeMid error"

> bndKnob :: RealFloat a => (a, a) -> a -> a -> a -> UI (Signal a)
> bndKnob (min, max) scale initVal initAng = mkKnob v2a a2v initVal
>   where
>       scale' = - (abs scale) -- to use clockwise direction
>       v2a val = let val' = if val < min then min else if val > max then max else val
>                 in (val' - initVal) / scale' + initAng
>       a2v ang = let val = (ang - initAng) * scale' + initVal
>                 in if val < min then min else if val > max then max else val

> infKnob :: RealFloat a => a -> a -> a -> UI (Signal a)
> infKnob scale initVal initAng = mkKnob v2a a2v initVal
>   where
>       scale' = - (abs scale) -- to use clockwise direction
>       v2a val = (val - initVal) / scale' + initAng
>       a2v ang = (ang - initAng) * scale' + initVal

> mkKnob :: RealFloat a => (a -> a) -> (a -> a) -> a -> UI (Signal a)
> mkKnob val2ang ang2val val0 =
>   mkUI (val0, Nothing) d draw (const nullSound) (const id)
>        process proj (const ())
>   where
>       diameter = 60
>       d = Layout 0 0 diameter diameter diameter diameter
>       proj = unzipS . (lift1 temp) where temp (a, b) = (a, (a, b))
>       pt2ang (cx, cy) (x, y) = alpha
>           where (dx, dy) = (fromIntegral (x - cx), fromIntegral (y - cy))
>                 (dx', dy') = (dx, -dy) -- change to normal coordinate system
>                 r = sqrt (dx^2 + dy^2)
>                 sinA = dy' / r
>                 alpha' = asin sinA
>                 alpha = if (dx' >= 0) then alpha'
>                             else (if (dy' >= 0) then (pi - alpha') else (-alpha' - pi))
>       epsilon = pi / 18 -- angular toleration of 5 degrees
>       fixCycle refAng ang = if (ang - refAng > pi) then (fixCycle refAng (ang - pi * 2))
>                               else if (refAng - ang > pi) then (fixCycle refAng (ang + pi * 2)) else ang
>       ang2line (cx, cy) rad ang = line (cx, cy) (x2, y2)
>           where (cosA, sinA) = (cos ang, sin ang)
>                 r2 = (fromIntegral rad) * 4 / 5
>                 (x2, y2) = (cx + truncate (r2 * cosA), cy - truncate (r2 * sinA))
>       draw b@((x, y), (w, h)) (val, _) =
>           let radius = ((min w h) `div` 2) - padding * 2
>               cent@(centX, centY) = (x + radius + padding, y + radius + padding)
>               ang = val2ang val
>           in (withColor' gray3 $ ang2line cent radius ang) //
>              (withColor' gray3 $ arc (x + padding, y + padding) (x + padding + radius * 2, y + padding + radius * 2) 0 360)
>       process ctx ((val, s), (evt, sys)) = ((val', s'), markDirty sys' (val /= val'))
>           where
>               ((val', s'), sys') = case evt of
>                   UIEvent (Button pt True down) -> let captured = handleCaptured ref pt in
>                       case (captured, down) of
>                           (True, True) -> ((val, Just (angDif ref pt)),
>                                             if focused then sys else sys {nextFocus = Just myid})
>                           (_, False) -> ((val, Nothing),
>                                           if focused then sys {focus = Nothing} else sys)
>                           _ -> ((val, s), sys)
>                   UIEvent (MouseMove pt) -> case s of
>                       Just ad -> ((pt2val ref ad pt, Just ad), sys)
>                       Nothing -> ((val, s), sys)
>                   _ -> ((val, s), sys)
>               bbx@((bx, by), (bw, bh)) = computeBBX ctx d
>               rad = ((min bw bh) `div` 2) - padding * 2
>               centr@(cenX, cenY) = (bx + rad + padding, by + rad + padding)
>               ref = val2ang val
>               angDif refAng p@(x, y)= let ang = fixCycle refAng (pt2ang centr p)
>                                       in refAng - ang
>               pt2val refAng aD p@(x, y) = ang2val ((fixCycle refAng (pt2ang centr p)) + aD)
>               handleCaptured refAng p@(x, y)  = let aDiff = angDif refAng p
>                                                 in ((p `inside` bbx) && (aDiff <= epsilon) && (aDiff >= (- epsilon)))
>               myid = uid ctx
>               focused = focus sys == Just myid

