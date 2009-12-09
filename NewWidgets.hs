module NewWidgets where
import Euterpea
import Euterpea.UI.Widget
import Euterpea.UI.UIMonad
import Euterpea.UI
import Euterpea.UI.SOE

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

