import XMonad
import System.Exit

import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import Data.List
import Data.Char
import XMonad.Layout.Spacing
import XMonad.Util.SpawnOnce
import XMonad.Config.Desktop
import XMonad.Layout.Reflect
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Tabbed
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedScratchpad


import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.DynamicLog
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows
import XMonad.Actions.SpawnOn
import System.Process



-- | Case-insensitive version of `=?`
(=?.) :: Query String -> String -> Query Bool
(=?.) q x = fmap ((== map toLower x) . map toLower) q



-- | Prefix matching version of `=?`

(=?>) :: Query String -> String -> Query Bool
(=?>) q x = fmap (isPrefixOf x) q



-- | Check if the second argument occurs in any of the first.
matches :: [String] -> String -> Bool
matches l s = any ((`isInfixOf` map toLower s) . map toLower) l

-- random comment
myTerminal = "alacritty"

myModMask = mod4Mask

myBorderWith = 1
myFocusedBorderColor = "#89ddff"
myNormalBorderColor = "#1b1d24"

myWorkspaces = ["1", "2", "3", "4", "5", "6"]

myStartupHook = do
    spawnOnce "redshift -O 4000k"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "feh --bg-center -g +-140--500 ~/.wallpapers/1.jpg"
    spawnOnce "picom -f"
--    spawnOnce "tint2 -c ~/.config/tint2/clock.tint2rc"
--    spawnOnce "tint2 -c ~/.config/tint2/workspaces.tint2rc"
    spawnOnce "dropbox"


    spawn "pulseaudio --start" -- @TODO comment out
    spawnOn "1" "discord"
    spawnOn "6" "/opt/Element/element-desktop"
    spawnOn "6" "/opt/Signal/signal-desktop"
    spawnOn "6" "app-launch WhatsApp"

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
      manageSpawn
  <+> composeAll
    [
      isFullscreen                        --> doFullFloat
    -- , className =? "Pavucontrol"        --> customFloating (W.RationalRect 0.2 0.05 0.6 0.4)
    , resource  =?. "desktop_window"      --> doIgnore
    , className =?. "platform-Emulicious" --> doFloat
    , className =?. "discord"             --> doShift "1"
    , title     =?. "Application Finder"  --> placeHook (smart (0.5, 0.5)) <+> doFloat
    , namedScratchpadManageHook scratchpads
    ]


scratchpads :: NamedScratchpads
scratchpads =
  [ NS { name  = "audio"
       , cmd   = "pavucontrol"
       , query = className  =?. "Pavucontrol"
       , hook  = customFloating (W.RationalRect 0.2 0.03 0.6 0.4)
       }
  , NS { name  = "password"
       , cmd   = "/opt/Bitwarden/bitwarden"
       , query = className =?. "Bitwarden"
       , hook  = customFloating (W.RationalRect 0.2 0.03 0.6 0.6)
       }
  , NS { name  = "terminal"
       , cmd   = "alacritty --class scratch --title scratch"
       , query = resource =?. "scratch"
       , hook  = customFloating (W.RationalRect 0.2 0.03 0.6 0.6)
       }
  ]



mySpacing = spacingRaw False
                       (Border 40 10 10 10) True
                       (Border  3  3  3  3) True

decoTheme :: Theme
decoTheme = def {         activeColor = "#1b1d24"
                ,       inactiveColor = "#1b1d24"
                ,         urgentColor = "#1b1d24"
                ,   activeBorderColor = "#1b1d24"
                , inactiveBorderColor = "#1b1d24"
                ,   urgentBorderColor = "#89ddff"
                ,     activeTextColor = "#89ddff"
                ,   inactiveTextColor = "#666666"
                ,     urgentTextColor = "#89ddff"
                ,            fontName = "xft:terminus:size=12"
                }



myLayout =
        mySpacing
         $ mkToggle (NOBORDERS ?? EOT)
         $ mkToggle (REFLECTX  ?? EOT)
         $ windowNavigation
         $ boringWindows
        --  $ subLayout [0] (tabbed shrinkText decoTheme)
         $  tiled
      --  ||| Full
       ||| tabbed shrinkText decoTheme

    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


-- Main configuration, override the defaults to your liking.
myConfig = desktopConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWith
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    , focusFollowsMouse  = True
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , startupHook        = ewmhDesktopsStartup <+> myStartupHook
    , logHook            = customLogHook <+> dynamicLog
    }

main = xmonad
     $ docks
    --  $ fullscreenSupport
       myConfig


customLogHook :: X ()
-- filter out NSP workspace that is used to hide scratchpads
customLogHook = ewmhDesktopsLogHookCustom (filter ((/= "NSP") . W.tag))


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch application menu
    , ((modm,               xK_p     ), spawn $ rofi "drun")
    -- launch run menu
    , ((modm .|. shiftMask, xK_p     ), spawn $ rofi "run")
    -- screenshot
    , ((0,                  xK_Print ), snip)
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    -- toggle horizontal reflect
    , ((modm .|. shiftMask, xK_v     ), sendMessage $ Toggle REFLECTX)
    -- toggle fullscreen
    , ((modm,               xK_Escape), toggleFullscreen)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_d     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_a     ), windows W.focusUp)
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_d     ), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_a     ), windows W.swapUp)
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm .|. shiftMask, xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN 1))
    -- , ((modm              , xK_period ), increaseLimit)
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))
    -- , ((modm              , xK_comma), decreaseLimit)


    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))


    -- media keys
    , ((0, xF86XK_AudioLowerVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, xF86XK_AudioMute          ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioPlay          ), spawn "playerctl -p playerctld play-pause")
    , ((0, xF86XK_AudioPrev          ), spawn "playerctl -p playerctld previous")
    , ((0, xF86XK_AudioNext          ), spawn "playerctl -p playerctld next")
    , ((0, xF86XK_MonBrightnessUp    ), spawn "xbacklight -inc 10")
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 10")

    -- scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction scratchpads  "audio")
    , ((modm              , xK_b     ), namedScratchpadAction scratchpads  "password")
    , ((modm              , xK_t     ), namedScratchpadAction scratchpads  "terminal")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_r     ), io exitSuccess)
    -- Restart xmonad
    , ((modm              , xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    where
      rofi :: String -> String
      rofi show = "rofi -show " ++ show ++ " -theme \"/home/hannah/.config/rofi/launcher/style\""

      snip = spawn "import png:- | xclip -selection c -t image/png -i"

      toggleFullscreen = do toggleWindowSpacingEnabled
                            toggleScreenSpacingEnabled
                            sendMessage $ Toggle NOBORDERS


help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]

