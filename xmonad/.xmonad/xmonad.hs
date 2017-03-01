
import XMonad
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

colorBackground     = "#152327"
colorBackgroundAlt  = "#223034"
colorBackgroundUrg  = "#252730"

myModMask            = mod4Mask  -- Changes Mod key to "super".
myFocusedBorderColor = solarizedBlue
myNormalBorderColor  = solarizedBase02
myBorderWidth        = 1
myTerminal           = "urxvt"
iconsRoot            = unsafePerformIO (getEnv "HOME") ++ "/.xmonad/images/"
myWorkspaces         = [ " 1 ", " 2 ", " 3 ", " 4 " ]


layoutIcons =
    [
      "<icon=" ++ iconsRoot ++ "layout_tall.xpm" ++ "/>"
    , "<icon=" ++ iconsRoot ++ "layout_mtall.xpm" ++ "/>"
    , "<icon=" ++ iconsRoot ++ "layout_full.xpm" ++ "/>"
    ]

tray = monitor
    { prop = ClassName "stalonetray"
    , name = "stalonetray"
    , visible = False
    }

myLayouts = ModifiedLayout tray $
            named (layoutIcons !! 0) (ResizableTall 1 (3/100) (1/2) [])
            ||| named (layoutIcons !! 1) (Mirror (ResizableTall 1 (3/100) (4/5) []))
            ||| named (layoutIcons !! 2) Full

myManageHook =
    [
      resource =? "screenkey" --> doIgnore
    , isFullscreen --> doFullFloat
    ]

myKeyBindings =
    [ ((myModMask, xK_Escape), spawn "gnome-screensaver-command -l")
    , ((myModMask, xK_b), sendMessage ToggleStruts)

    -- Sound control.
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulseaudio-ctl up")
    , ((0, xF86XK_AudioLowerVolume), spawn "pulseaudio-ctl down")
    , ((0, xF86XK_AudioMute), spawn "pulseaudio-ctl mute")

    -- Brightness control
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")

    -- Screenshot
    , ((0, xK_Print), spawn "scrot -e 'mv $f $${HOME}/downloads'")
    , ((shiftMask, xK_Print), spawn "scrot -u -e 'mv $f $${HOME}/downloads'")
    ]


main = do
    xmproc <- Run.spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh def
        { borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = myModMask
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
                                  spawnOnce "nm-applet"
                                  spawnOnce "blueman-applet"
                                  spawnOnce "dropbox"
                                  spawnOnce "conky -d"
                                  spawnOnce "sleep 10 && kalu"
                                  spawn "albert"
        , logHook            = dynamicLogWithPP $ xmobarPP {
                                 ppOutput = Run.hPutStrLn xmproc
                               , ppCurrent = xmobarColor solarizedBlue colorBackgroundAlt
                               , ppHidden = xmobarColor solarizedBase0 ""
                               , ppHiddenNoWindows = xmobarColor solarizedBase01 colorBackground
                               , ppLayout = xmobarColor solarizedCyan ""
                               , ppTitle = xmobarStrip . shorten 80
                               , ppUrgent = xmobarColor solarizedRed colorBackgroundUrg
                               , ppVisible = xmobarColor solarizedBase01 ""
                               }
        } `EZ.additionalKeys` myKeyBindings
