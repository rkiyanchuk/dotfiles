
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop(desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName(setWMName)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Monitor
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig as EZ
import XMonad.Util.NamedWindows
import XMonad.Util.Run as Run
import XMonad.Util.SpawnOnce(spawnOnce)

import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import System.Environment
import System.IO.Unsafe


nord0   = "#2e3440"
nord1   = "#3b4252"
nord2   = "#434c5e"
nord3   = "#4c566a"
nord4   = "#d8dee9"
nord5   = "#e5e9f0"
nord6   = "#eceff4"
nord7   = "#8fbcbb"
nord8   = "#88c0d0"
nord9   = "#81a1c1"
nord10  = "#5e81ac"
nord11  = "#bf616a"
nord12  = "#d08770"
nord13  = "#ebcb8b"
nord14  = "#a3be8c"
nord15  = "#b48ead"

colorBackground     = nord0
colorBackgroundAlt  = nord2

metaMask            = mod4Mask  -- Changes Mod key to "super".
altMask             = mod1Mask  -- Map Alt to more descriptive var.
myFocusedBorderColor = nord8
myNormalBorderColor  = nord3
myBorderWidth        = 1
myTerminal           = "tilix"
iconsRoot            = unsafePerformIO (getEnv "HOME") ++ "/.xmonad/images/"
myWorkspaces         = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 "]


layoutIcons =
    [
      "<icon=" ++ iconsRoot ++ "layout_tall.xpm" ++ "/>"
    , "<icon=" ++ iconsRoot ++ "layout_mtall.xpm" ++ "/>"
    , "<icon=" ++ iconsRoot ++ "layout_full.xpm" ++ "/>"
    ]

myLayouts = named (head layoutIcons) (ResizableTall 1 (3/100) (1/2) [])
            ||| named (layoutIcons !! 1) (Mirror (ResizableTall 1 (3/100) (4/5) []))
            ||| named (layoutIcons !! 2) Full

myManageHook =
    [
      resource =? "screenkey" --> doIgnore
    , isFullscreen --> doFullFloat
    ]

myKeyBindings =
    [ ((metaMask, xK_Escape), spawn "gnome-screensaver-command -l")
    , ((metaMask, xK_b), sendMessage ToggleStruts)
--  , ((metaMask, xK_w), onPrevNeighbour W.view)
--  , ((metaMask, xK_e), onNextNeighbour W.view)
--  , ((metaMask .|. shiftMask, xK_w), onPrevNeighbour W.shift)
--  , ((metaMask .|. shiftMask, xK_e), onNextNeighbour W.shift)
    , ((0, xF86XK_Search), spawn "albert toggle")

    -- Sound control.
    , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -i 5")
    , ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 5")
    , ((0, xF86XK_AudioMute), spawn "pamixer -t")
    , ((shiftMask, xF86XK_AudioMute), spawn "pamixer -t")

    -- Brightness control
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((metaMask .|. shiftMask, xK_Up), spawn "xbacklight -inc 5")
    , ((metaMask .|. shiftMask, xK_Down), spawn "xbacklight -dec 5")

    -- Screenshot
    , ((0, xK_Print), spawn "flameshot gui -p ${HOME}/Dropbox/Screenshots")
    , ((shiftMask, xK_Print), spawn "flameshot full -p ${HOME}/Dropbox/Screenshots")
    ]


main = do
    xmproc <- Run.spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh def
        { borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = metaMask
        , workspaces         = myWorkspaces
        , terminal           = myTerminal
        , layoutHook         = avoidStruts $ smartBorders myLayouts
        , handleEventHook    = handleEventHook def
                               <+> fullscreenEventHook
                               <+> docksEventHook
        , manageHook         = manageDocks
                               <+> manageHook def
                               <+> composeAll myManageHook
        , startupHook        = do setWMName "LG3D"
                                  spawn "albert"
                                  spawnOnce "trayer --edge bottom --widthtype request --align right  --distancefrom right --distance 280 --tint 0x3B4252 --alpha 0 --transparent true --height 28"
                                  spawnOnce "multiload-ng-systray"
                                  spawnOnce "sleep 1 && nm-applet"
                                  spawnOnce "sleep 1 && blueman-applet"
                                  spawnOnce "sleep 1 && dropbox"
                                  spawnOnce "sleep 1 && pasystray --notify=all"
                                  spawnOnce "conky -d"
                                  spawnOnce "sleep 10 && kalu"
        , logHook            = dynamicLogWithPP $ xmobarPP {
                                 ppOutput = Run.hPutStrLn xmproc
                               , ppCurrent = xmobarColor nord6 colorBackgroundAlt
                               , ppHidden = xmobarColor nord7 ""
                               , ppHiddenNoWindows = xmobarColor nord3 ""
                               , ppLayout = xmobarColor nord3 ""
                               , ppTitle = xmobarStrip . shorten 75
                               , ppSep = " "
                               , ppUrgent = xmobarColor nord12 colorBackground
                               , ppVisible = xmobarColor nord6 ""
                               }
        } `EZ.additionalKeys` myKeyBindings
