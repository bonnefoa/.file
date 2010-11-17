import XMonad
import XMonad.Hooks.DynamicLog
import qualified Data.Map as M
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.FadeInactive
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.SetWMName
import Data.Ratio ((%))
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

main = do 
    xmobar <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig 
      {  modMask=mod4Mask
      , normalBorderColor="#ffffff"
      , focusedBorderColor="#9492F" 
      , borderWidth = 1
      , keys = \c -> myKeys c `M.union` keys defaultConfig c
      , logHook  = myLogHook xmobar
      , manageHook = myManageDocks <+> manageHook defaultConfig
      , startupHook = setWMName "LG3D"
      , layoutHook = smartBorders (avoidStruts  $  layoutHook defaultConfig)
      }

myLogHook :: Handle -> X ()
myLogHook xmobar = do
    dynamicLogWithPP $ xmobarPP 
                     { ppOutput = hPutStrLn xmobar
                     , ppTitle = xmobarColor "green" "" . shorten 50
                     }
    fadeInactiveLogHook fadeAmount
    where fadeAmount = 0xdddddddd

myManageDocks = manageDocks <+> (composeAll $
-- Allows focusing other monitors without killing the fullscreen
   [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ] 
   ++ [title =? t --> doFloat | t <- []]
  )

myKeys(XConfig {modMask = modm})= M.fromList $ 
     -- Apps ans tools
     [ 
       ((modm .|. shiftMask, xK_l ), spawn "xscreensaver-command --lock")
     , ((modm, xK_Left), spawn "mpc prev")
     , ((modm, xK_Down), spawn "mpc toggle")
     , ((modm, xK_Right), spawn "mpc next")
     , ((modm, xK_BackSpace), spawn "suppr.sh")
     ]

