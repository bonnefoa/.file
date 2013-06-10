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
import XMonad.Hooks.UrgencyHook
import Data.Maybe
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Paste
import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Dmenu
import Control.Arrow((>>>))
import Control.Monad
import System.FilePath
import System.Posix.Files
import Data.String
import System.Directory
import Data.List

data InitProgram = InitProgram WorkspaceId String
  deriving (Read, Show)

main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
      {  modMask=mod4Mask
      , normalBorderColor="#ffffff"
      , focusedBorderColor="#9492F"
      , borderWidth = 1
      , keys = \c -> myKeys c `M.union` keys defaultConfig c
      , logHook = myLogHook xmobar
      , manageHook = myManageHook
      , terminal = "urxvt"
      , layoutHook = myLayout
      , startupHook = myStartupHook
      }

myManageHook :: ManageHook
myManageHook = do
  doF W.focusDown <+> doFullFloat
  manageSpawn <+> myManageDocks <+> manageHook defaultConfig

myLayout = smartBorders (avoidStruts  $
    (layoutHook defaultConfig)
  )

myDmenu :: X ()
myDmenu = do
  files <- liftIO getListExecutables
  currentWorkspace <- fmap W.currentTag (gets windowset)
  res <- dmenu files
  spawnOn currentWorkspace res

myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"

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
   ++ [ className =? "mplayer2"  --> doFloat ]
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
  saveWsState
  windows $ W.greedyView newWorkspace

processChange :: WorkspaceId -> WorkspaceId -> X()
processChange currentWorkspace targetWorkspace
  | currentWorkspace == targetWorkspace =
    fetchAndSwapOldWs targetWorkspace
  | otherwise = do
    saveWsState
    windows $ W.greedyView targetWorkspace


sendKey keysym = withDisplay $ \d -> do
  keycode <- io $ keysymToKeycode d keysym
  root <- asks theRoot
  let subw = none
  withFocused $ \w -> do
    io $ allocaXEvent $ \ev -> do
                        setEventType ev keyPress
                        setKeyEvent ev w root subw noModMask keycode True
                        sendEvent d w False noEventMask ev
    io $ allocaXEvent $ \ev -> do
                        setEventType ev keyRelease
                        setKeyEvent ev w root subw noModMask keycode True
                        sendEvent d w False noEventMask ev
    return ()

myKeys conf@(XConfig {modMask = modm})= M.fromList $
     -- Apps ans tools
     [
     ((modm, xK_Left), spawn "mpc prev")
     , ((modm, xK_Down), spawn "mpc toggle")
     , ((modm, xK_Right), spawn "mpc next")
     , ((modm, xK_BackSpace), spawn "suppr.sh")
     , ((modm, xK_Up), spawn "copy.sh")
     , ((noModMask, xF86XK_AudioMute), spawn "amixer set Master toggle")
     , ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5+")
     , ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 5-")
     , ((noModMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc +10")
     , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -inc -10")
     ]
    ++
    [(
        (modm, k), myChangeWorkspace i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    ]

filterUniq :: [FilePath] -> [FilePath]
filterUniq (x : y : xs)
  | x == y = filterUniq (y:xs)
  | otherwise = x : filterUniq (y:xs)
filterUniq res = res

getAbsFilesInDir :: FilePath -> IO([FilePath])
getAbsFilesInDir fp = do
  contents <- fmap (\\ [".", ".."]) (getDirectoryContents fp)
  return $ map (\a -> joinPath [fp, a]) contents

getFilesInPath :: IO([FilePath])
getFilesInPath = getSearchPath
  >>= mapM getAbsFilesInDir
  >>= (concat >>> return)

isSymlinkExecutable :: FilePath -> IO Bool
isSymlinkExecutable fp = do
  li <- readSymbolicLink fp
  let fullPath = joinPath [root, li]
  isFileExit <- fileExist fullPath
  if isFileExit
    then fileAccess fullPath False False True
    else return False
  where root = dropFileName fp

isFileExecutable :: FilePath -> IO Bool
isFileExecutable fp = do
  fs <- getSymbolicLinkStatus fp
  if isSymbolicLink fs
    then isSymlinkExecutable fp
    else fileAccess fp False False True

getListExecutables :: IO([FilePath])
getListExecutables =
  getFilesInPath
  >>= filterM isFileExecutable
  >>= return . filterUniq . sort . (map takeFileName)
