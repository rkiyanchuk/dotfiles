import Control.Monad
import Data.Ratio ((%))
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Plane
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import qualified XMonad.StackSet as W


solarizedBase03     = "#002b36"
solarizedBase02     = "#073642"
solarizedBase01     = "#586e75"
solarizedBase00     = "#657b83"
solarizedBase0      = "#839496"
solarizedBase1      = "#93a1a1"
solarizedBase2      = "#eee8d5"
solarizedBase3      = "#fdf6e3"
solarizedYellow     = "#b58900"
solarizedOrange     = "#cb4b16"
solarizedRed        = "#dc322f"
solarizedMagenta    = "#d33682"
solarizedViolet     = "#6c71c4"
solarizedBlue       = "#268bd2"
solarizedCyan       = "#2aa198"
solarizedGreen      = "#859900"

myModMask            = mod4Mask         -- Changes the mod key to "super".
myFocusedBorderColor = solarizedBlue    -- Color of focused border.
myNormalBorderColor  = solarizedBase02  -- Color of inactive border.
myBorderWidth        = 1                -- Width of border around windows.
myTerminal           = "urxvt"          -- Which terminal software to use.

myTitleLength       = 90  -- Truncate window title to this length.
-- Workspace indicator wrappers.
myCurrentWSLeft     = "["
myCurrentWSRight    = "]"
myVisibleWSLeft     = "["
myVisibleWSRight    = "]"
myUrgentWSLeft      = "{"
myUrgentWSRight     = "}"

myWorkspaces = [ "α", "β", "γ", "δ", "ε", "ζ", "η"]
startupWorkspace = "α"


defaultLayouts = smartBorders(avoidStruts(
    named "<icon=/home/zoresvit/.xmonad/images/layout_tall.xpm/>"
        (ResizableTall 1 (3/100) (1/2) [])
    ||| named "<icon=/home/zoresvit/.xmonad/images/layout_rtall.xpm/>"
        (Mirror (ResizableTall 1 (3/100) (4/5) []))
    ||| named "<icon=/home/zoresvit/.xmonad/images/layout_full.xpm/>"
        (noBorders Full)))

chatLayout = named "<icon=/home/zoresvit/.xmonad/images/layout_chat.xpm/>"
    $ avoidStruts $ reflectHoriz $ withIM (0.2) isSkype (Grid)
    ||| named "<icon=/home/zoresvit/.xmonad/images/layout_tall.xpm/>"
        (ResizableTall 1 (3/100) (1/2) [])
  where
    isSkype = (Title "zoresvit - Skype™")

myLayouts = onWorkspace "η" chatLayout $
            defaultLayouts

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "XXkb" --> doIgnore,
  className =? "Kazam" --> doFloat,
  resource =? "screenkey" --> doIgnore,
  resource =? "stalonetray" --> doIgnore,
  resource =? "xfce4-notifyd" --> doIgnore,
  className =? "rdesktop" --> doFloat,
  className =? "Skype" --> doShift "η"
  ]

myKeyBindings =
  [
    ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask .|. shiftMask, xK_l), sendMessage MirrorShrink)
    , ((myModMask .|. shiftMask, xK_h), sendMessage MirrorExpand)
    -- Shift to previous workspace
    , ((myModMask, xK_Left), prevWS)
    , ((myModMask .|. controlMask, xK_h), prevWS)
    -- Shift to next workspace
    , ((myModMask, xK_Right), nextWS)
    , ((myModMask .|. controlMask, xK_l), nextWS)
    -- Shift window to previous workspace
    , ((myModMask .|. shiftMask, xK_Left), shiftToPrev)
    , ((myModMask .|. controlMask .|. shiftMask, xK_h), shiftToPrev)
    -- Shift window to next workspace
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext)
    , ((myModMask .|. controlMask .|. shiftMask, xK_l), shiftToNext)
    , ((myModMask, xK_u), focusUrgent)
    , ((myModMask, xK_p), spawn "dmenu_run -i -nb '#002b36' -nf  '#839496' -sb '#073642' -sf '#93a1a1' -fn 'Liberation Mono-13'")
    -- Lock computer.
    , ((myModMask .|. mod1Mask, xK_l), spawn "gnome-screensaver-command -l")
    -- Volume control
    , ((myModMask .|. controlMask, xK_m), toggleMute >> return ())
    , ((0, xF86XK_AudioMute), toggleMute >> return ())
    , ((myModMask .|. controlMask, xK_Down), lowerVolume 5 >> return ())
    , ((0, xF86XK_AudioRaiseVolume), raiseVolume 5 >> return ())
    , ((myModMask .|. controlMask, xK_Up), raiseVolume 5 >> return())
    , ((0, xF86XK_AudioLowerVolume), lowerVolume 5 >> return ())
    -- Brightness control
    , ((myModMask .|. controlMask, xK_Right), spawn "xbacklight -inc 5")
    , ((myModMask .|. controlMask, xK_Left), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((myModMask, xK_Print), spawn "scrot -e 'mv $f $${HOME}/media/screenshots'")
    , ((myModMask .|. shiftMask, xK_Print), spawn "scrot -u -e 'mv $f $${HOME}/media/screenshots'")
    , ((myModMask, xK_w), onPrevNeighbour W.view)
    , ((myModMask, xK_e), onNextNeighbour W.view)
    , ((myModMask .|. shiftMask, xK_w), onPrevNeighbour W.shift)
    , ((myModMask .|. shiftMask, xK_e), onNextNeighbour W.shift)
  ]

-- LibNotify urgency hook
-- Create notification popup when some window becomes urgent.
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "Urgent window"]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig {
        borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      , handleEventHook = fullscreenEventHook
      , layoutHook = myLayouts
      , manageHook = manageHook defaultConfig
          <+> composeAll myManagementHooks
          <+> manageDocks
      , modMask = myModMask
      , normalBorderColor = myNormalBorderColor
      , startupHook = do
          spawn "~/.xmonad/autorun.sh"
          setWMName "LG3D"
      , terminal = myTerminal
      , workspaces = myWorkspaces
      , logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppCurrent = xmobarColor solarizedGreen "" . wrap myCurrentWSLeft myCurrentWSRight
          , ppHidden = xmobarColor solarizedBase0 ""
          , ppHiddenNoWindows = xmobarColor solarizedBase02 ""
          , ppLayout = xmobarColor solarizedCyan ""
          , ppTitle = xmobarColor solarizedBase1 "" . shorten myTitleLength
          , ppUrgent = xmobarColor solarizedRed "" . wrap myUrgentWSLeft myUrgentWSRight
          , ppVisible = xmobarColor solarizedBase01 "" . wrap myVisibleWSLeft myVisibleWSRight
          }
      } `additionalKeys` myKeyBindings
