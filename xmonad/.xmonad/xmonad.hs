
import XMonad
import XMonad.Config.Desktop(desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName(setWMName)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig as EZ
import XMonad.Util.SpawnOnce(spawnOnce)
import Graphics.X11.ExtraTypes.XF86


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

myModMask            = mod4Mask  -- Changes Mod key to "super".
myFocusedBorderColor = solarizedBlue
myNormalBorderColor  = solarizedBase02
myBorderWidth        = 1
myTerminal           = "urxvt"
myWorkspaces = [ "1", "2", "3", "4" ]

myLayouts = ResizableTall 1 (3/100) (1/2) []
            ||| Mirror (ResizableTall 1 (3/100) (4/5) [])
            ||| Full

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

main = xmonad $ ewmh def
    { borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , terminal           = myTerminal
    , layoutHook         = avoidStruts $ smartBorders myLayouts
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook
    , manageHook         = manageDocks <+> manageHook def <+> composeAll myManageHook
    , startupHook        = do setWMName "LG3D"
                              spawnOnce "blueman-applet"
                              spawnOnce "conky -d"
                              spawnOnce "dropbox start"
                              spawnOnce "sleep 10 && kalu"
                              spawnOnce "nm-applet"
                              spawnOnce "polybar top"
                              spawn "albert"
    } `EZ.additionalKeys` myKeyBindings
