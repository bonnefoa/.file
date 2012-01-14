{-# LANGUAGE DeriveDataTypeable #-}
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
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import Data.Maybe
import qualified XMonad.Util.ExtensibleState as XS

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
    (layoutHook defaultConfig)
  )
  where tabbed = named "Tabbed" $ simpleTabbed

myDmenu :: X ()
myDmenu = do
  currentWorkspace <- fmap W.currentTag (gets windowset)
  spawnOn currentWorkspace "exe=`IFS=: ;lsx $PATH |sort -u| dmenu ` && eval \"exec $exe\""


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

data OldWorkspace = OldWorkspace (M.Map ScreenId WorkspaceId) deriving Typeable
instance ExtensionClass OldWorkspace where
  initialValue = OldWorkspace M.empty

myChangeWorkspace :: WorkspaceId -> X()
myChangeWorkspace targetWorkspace = do
  currentWorkspace <- fmap W.currentTag (gets windowset)
  processChange currentWorkspace targetWorkspace

getCurrentScreen :: X(ScreenId)
getCurrentScreen = fmap (W.screen . W.current) (gets windowset)

saveWsState :: X()
saveWsState = do
  currentWorkspace <- fmap W.currentTag (gets windowset)
  currentScreen <- getCurrentScreen
  OldWorkspace map <- XS.get
  XS.put $ OldWorkspace $ M.insert currentScreen currentWorkspace map

fetchAndSwapOldWs :: WorkspaceId ->  X()
fetchAndSwapOldWs targetWorkspace = do
  currentScreen <- getCurrentScreen
  OldWorkspace oldMap <- XS.get
  let newWorkspace = M.findWithDefault targetWorkspace currentScreen oldMap
  windows $ W.greedyView newWorkspace

processChange :: WorkspaceId -> WorkspaceId -> X()
processChange currentWorkspace targetWorkspace
  | currentWorkspace == targetWorkspace =
    fetchAndSwapOldWs targetWorkspace
  | otherwise = do
    saveWsState
    windows $ W.greedyView targetWorkspace

myKeys conf@(XConfig {modMask = modm})= M.fromList $
     -- Apps ans tools
     [
       ((modm .|. shiftMask, xK_l ), spawn "xlock")
     , ((modm, xK_Left), spawn "mpc prev")
     , ((modm, xK_Down), spawn "mpc toggle")
     , ((modm, xK_Right), spawn "mpc next")
     , ((modm, xK_BackSpace), spawn "suppr.sh")
     , ((modm, xK_p), myDmenu)
     ]
    ++
    [(
        (modm, k), myChangeWorkspace i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    ]

{-
 -readInitProgram :: IO([InitProgram])
 -readInitProgram = do
 -  strInitProgram <- readFile "initProgram.conf"
 -  return $ read strInitProgram
 -}

