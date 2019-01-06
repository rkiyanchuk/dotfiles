
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

colorBackground     = "#232629"
colorBackgroundAlt  = "#31363b"

metaMask            = mod4Mask  -- Changes Mod key to "super".
altMask             = mod1Mask  -- Map Alt to more descriptive var.
myFocusedBorderColor = "#3daee9"
myNormalBorderColor  = "#2c3e50"
myBorderWidth        = 1
myTerminal           = "gnome-terminal"
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
            named (head layoutIcons) (ResizableTall 1 (3/100) (1/2) [])
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
--   , ((metaMask, xK_e), onNextNeighbour W.view)
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
                                  spawnOnce "trayer --edge bottom --widthtype request --align right  --distancefrom right --distance 335 --tint 0x232629 --alpha 0 --transparent true --height 28"
                                  spawnOnce "multiload-ng-systray"
                                  spawnOnce "sleep 1 && nm-applet"
                                  spawnOnce "sleep 1 && blueman-applet"
                                  spawnOnce "sleep 1 && dropbox"
                                  spawnOnce "sleep 1 && pasystray --notify=all"
                                  spawnOnce "conky -d"
                                  spawnOnce "sleep 10 && kalu"
        , logHook            = dynamicLogWithPP $ xmobarPP {
                                 ppOutput = Run.hPutStrLn xmproc
                               , ppCurrent = xmobarColor myFocusedBorderColor colorBackgroundAlt
                               , ppHidden = xmobarColor colorBase1 ""
                               , ppHiddenNoWindows = xmobarColor colorBase01 colorBackground
                               , ppLayout = xmobarColor colorCyan ""
                               , ppTitle = xmobarStrip . shorten 75
                               , ppSep = " "
                               , ppUrgent = xmobarColor colorRed colorBackground
                               , ppVisible = xmobarColor colorBase1 ""
                               }
        } `EZ.additionalKeys` myKeyBindings
