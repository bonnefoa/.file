import Data.Ratio ((%))
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Core
import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Accordion
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

data InitProgram = InitProgram WorkspaceId String
  deriving (Read, Show)

main = do
    xmobar <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig
      {  modMask=mod4Mask
      , normalBorderColor="#ffffff"
      , focusedBorderColor="#9492F"
      , borderWidth = 1
      , keys = \c -> myKeys c `M.union` keys defaultConfig c
      , logHook = myLogHook xmobar >>= \_ -> takeTopFocus
      , manageHook = manageSpawn <+> myManageDocks <+> manageHook defaultConfig
      , terminal = "urxvt"
      , layoutHook = myLayout
      , startupHook = myStartupHook
      }

myLayout =
  smartBorders (avoidStruts  $
    (tabbed |||  Grid ||| Accordion ||| layoutHook defaultConfig )
  )
  where tabbed = named "Tabbed" $ simpleTabbed

myDmenu :: X ()
myDmenu = do
  currentWorkspace <- fmap W.currentTag (gets windowset)
  spawnOn currentWorkspace "exe=`dmenu_path | dmenu ` && eval \"exec $exe\""

myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    {-
     -listInitProgram <- liftIO readInitProgram
     -mapM_ (\(InitProgram wid prg) -> spawnOn wid prg) listInitProgram
     -}

myLogHook :: Handle -> X ()
myLogHook xmobar = do
    dynamicLogWithPP $ xmobarPP
                     { ppOutput = hPutStrLn xmobar
                     , ppTitle = xmobarColor "green" "" . shorten 50
                     , ppSep = " | "
                     , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                     }
    fadeInactiveLogHook fadeAmount
    where fadeAmount = 0xdddddddd

myManageDocks = manageDocks <+> (composeAll $
-- Allows focusing other monitors without killing the fullscreen
   [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
   ++ [title =? t --> doFloat | t <- []]
  )

myKeys (config@XConfig {modMask = modm})= M.fromList $
     -- Apps ans tools
     [
       ((modm .|. shiftMask, xK_l ), spawn "xscreensaver-command --lock")
     , ((modm, xK_Left), spawn "mpc prev")
     , ((modm, xK_Down), spawn "mpc toggle")
     , ((modm, xK_Right), spawn "mpc next")
     , ((modm, xK_BackSpace), spawn "suppr.sh")
     , ((modm, xK_p), myDmenu)
     ]

{-
 -readInitProgram :: IO([InitProgram])
 -readInitProgram = do
 -  strInitProgram <- readFile "initProgram.conf"
 -  return $ read strInitProgram
 -}

