
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


colorBase03     = "#002b36"
colorBase02     = "#073642"
colorBase01     = "#586e75"
colorBase00     = "#657b83"
colorBase0      = "#839496"
colorBase1      = "#93a1a1"
colorBase2      = "#eee8d5"
colorBase3      = "#fdf6e3"
colorYellow     = "#b58900"
colorOrange     = "#cb4b16"
colorRed        = "#dc322f"
colorMagenta    = "#d33682"
colorViolet     = "#6c71c4"
colorBlue       = "#268bd2"
colorCyan       = "#2aa198"
colorGreen      = "#859900"

colorBackground     = "#152327"
colorBackgroundAlt  = "#223034"
colorBackgroundUrg  = "#252730"

myModMask            = mod4Mask  -- Changes Mod key to "super".
myFocusedBorderColor = colorBlue
myNormalBorderColor  = "#084652"
myBorderWidth        = 1
myTerminal           = "urxvt"
iconsRoot            = unsafePerformIO (getEnv "HOME") ++ "/.xmonad/images/"
myWorkspaces         = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 "]


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
    , ((myModMask, xK_w), onPrevNeighbour W.view)
    , ((myModMask, xK_e), onNextNeighbour W.view)
    , ((myModMask .|. shiftMask, xK_w), onPrevNeighbour W.shift)
    , ((myModMask .|. shiftMask, xK_e), onNextNeighbour W.shift)
    , ((0, xF86XK_Search), spawn "albert toggle")

    -- Sound control.
    , ((0, xF86XK_AudioRaiseVolume), spawn "${HOME}/.xmonad/control.sh vol-inc")
    , ((0, xF86XK_AudioLowerVolume), spawn "${HOME}/.xmonad/control.sh vol-dec")
    , ((0, xF86XK_AudioMute), spawn "${HOME}/.xmonad/control.sh mute")
    , ((shiftMask, xF86XK_AudioMute), spawn "${HOME}/.xmonad/control.sh mute-input")
    , ((shiftMask .|. controlMask, xF86XK_AudioMute), spawn "${HOME}/.xmonad/control.sh unmute-input")

    -- Brightness control
    , ((0, xF86XK_MonBrightnessUp), spawn "${HOME}/.xmonad/control.sh br-inc")
    , ((0, xF86XK_MonBrightnessDown), spawn "${HOME}/.xmonad/control.sh br-dec")

    -- Screenshot
    , ((0, xK_Print), spawn "deepin-screenshot")
    , ((shiftMask, xK_Print), spawn "deepin-screenshot --fullscreen")
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
                                  spawnOnce "pasystray -m 150"
                                  spawnOnce "blueman-applet"
                                  spawnOnce "dropbox"
                                  spawnOnce "conky -d"
                                  spawnOnce "sleep 10 && kalu"
                                  spawn "albert"
        , logHook            = dynamicLogWithPP $ xmobarPP {
                                 ppOutput = Run.hPutStrLn xmproc
                               , ppCurrent = xmobarColor colorBlue colorBackgroundAlt
                               , ppHidden = xmobarColor colorBase0 ""
                               , ppHiddenNoWindows = xmobarColor colorBase01 colorBackground
                               , ppLayout = xmobarColor colorCyan ""
                               , ppTitle = xmobarStrip . shorten 75
                               , ppSep = " "
                               , ppUrgent = xmobarColor colorRed colorBackgroundUrg
                               , ppVisible = xmobarColor colorBase01 ""
                               }
        } `EZ.additionalKeys` myKeyBindings
