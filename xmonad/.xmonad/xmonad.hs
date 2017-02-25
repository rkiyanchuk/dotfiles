
import XMonad
import XMonad.Config.Desktop(desktopConfig)
import XMonad.Hooks.SetWMName(setWMName)
import XMonad.Util.EZConfig as EZ

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
startupWorkspace = "1"


myKeyBindings =
    [ ((myModMask, xK_Escape), spawn "gnome-screensaver-command -l")
    ]

main = xmonad $ desktopConfig
    { borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal           = myTerminal
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , startupHook        = do setWMName "LG3D"
                              spawn "${HOME}/.xmonad/autorun.sh"
                              spawn "polybar -r top"
    } `EZ.additionalKeys` myKeyBindings
