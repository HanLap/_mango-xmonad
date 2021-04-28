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
import XMonad.Layout.IndependentScreens
import Data.Monoid
import XMonad.Layout.PerWorkspace

import qualified XMonad.Actions.Commands as R


-- | Case-insensitive version of `=?`
(=?.) :: Query String -> String -> Query Bool
(=?.) q x = fmap ((== map toLower x) . map toLower) q




(=?>) :: Query String -> String -> Query Bool
(=?>) q x = fmap (isPrefixOf x) q



-- | Check if the second argument occurs in any of the first.
matches :: [String] -> String -> Bool
matches l s = any ((`isInfixOf` map toLower s) . map toLower) l

myTerminal = "alacritty"

myModMask = mod4Mask
-- | Prefix matching version of `=?`

myBorderWith = 3
myFocusedBorderColor:: [Char]
myFocusedBorderColor = "#<#{PRIMARY}#>"
myNormalBorderColor:: [Char]
myNormalBorderColor = "#<#{BACKGROUND_ALT}#>"

myWorkspaces:: [WorkspaceId]
myWorkspaces = ["www", "code", "term", "misc", "music", "chat", "mail", "zoom"]

{- wss:: [[WorkspaceId,[[Char]]]
wss = [ [ "www"   ,
            [ "firefox"       ]
        ]
      , [ "code"  ,
            [ "code"
            , "jetbrains-ide" ]
        ]
      , [ "term"  , [         ]]
      , [ "music" ,
            [ "spotify"       ]
            ]
      , [ "chat"  ,
            [ "discord"
            , "element"
            , "signal"        ]]
      , [ "mail"  ,
            [ "thunderbird"   ]]
      ]
-}
myStartupHook = do
    spawnOnce "comango hook wminit"


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
myManageHook = manageSpawn
           <+> namedScratchpadManageHook scratchpads
           <+> composeAll
               [
                 isFullscreen                        --> doFullFloat
               -- , className =? "Pavucontrol"        --> customFloating (W.RationalRect 0.2 0.05 0.6 0.4)
               , className =?. "firefox"             --> doShift "www"
               , className =?. "code"                --> doShift "code"
               , className =?. "jetbrains-idea"      --> doShift "code"
               , className =?. "spotify"             --> doShift "chat"
               , className =?. "discord"             --> doShift "chat"
               , className =?. "element"             --> doShift "chat"
               , className =?. "signal"              --> doShift "chat"
               , className =?. "thunderbird"         --> doShift "mail"
               , className =?. "zoom"                --> doShift "zoom"
               , className =?. "gcr-prompter"        --> doCenterFloat
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



-- | Naughty windows that won't stop trying to steal focus.
offenders :: [Query Bool]
offenders =
    [ className =?. "Steam"
    , className =?. "Thunderbird"
    , className =?. "Firefox"
    , className =?. "discord"
    , className =?. "signal"
    , className =?. "element"
    ]







mySpacing = spacingRaw False
                       (Border 40 10 10 10) True
                       (Border  3  3  3  3) True

decoTheme :: Theme
decoTheme = def {         activeColor = "#<#{BACKGROUND_ALT}#>"
                ,       inactiveColor = "#<#{BACKGROUND}#>"
                ,         urgentColor = "#<#{BACKGROUND}#>"
                ,   activeBorderColor = "#<#{BACKGROUND}#>"
                , inactiveBorderColor = "#<#{BACKGROUND}#>"
                ,   urgentBorderColor = "#<#{PRIMARY}#>"
                ,     activeTextColor = "#<#{PRIMARY}#>"
                ,   inactiveTextColor = "#<#{FOREGROUND_ALT}#>"
                ,     urgentTextColor = "#<#{SECONDARY}#>"
                ,            fontName = "xft:terminus:size=12"
                }



myLayout = mySpacing
         $ mkToggle (NOBORDERS ?? EOT)
         $ mkToggle (REFLECTX  ?? EOT)
         $ windowNavigation
         $ boringWindows
         $ onWorkspace (myWorkspaces !! 4) (tabs  ||| tiled)
         $ onWorkspace (myWorkspaces !! 5) (tabs  ||| tiled)
         $                                  tiled ||| tabs
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled = Tall 1 (3/100) (1/2)
      tabs  = tabbed shrinkText decoTheme



-- | Adds `guardHook` to an existing config. Easiest way to make sure that it
--   catches events for /all/ custom hooks.
guardAgainst :: [Query Bool] -> XConfig a -> XConfig a
guardAgainst list conf = conf { handleEventHook = guardHook list
                                                $ handleEventHook conf
                              }



-- | Blocks events of type @_NET_ACTIVE_WINDOW@ for windows matching any of the
--   given queries from entering the given event hook.
guardHook :: [Query Bool] -> (Event -> X All) -> Event -> X All
guardHook list hook ev@ClientMessageEvent
                            { ev_window       = window
                            , ev_message_type = mtype
                            } = do

    actWind <- getAtom "_NET_ACTIVE_WINDOW"

    if mtype /= actWind
       then hook ev
       else do
            b <- or <$> mapM (`runQuery` window) list

            if b
               then return $ All False
               else hook ev

guardHook _ hook ev = hook ev




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
     $ guardAgainst offenders
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
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf      )
    -- launch application menu
    , ((modm,               xK_p     ), rofi "drun"                       )
    -- launch run menu
    , ((modm .|. shiftMask, xK_p     ), rofi "run"                        )
    , ((modm,               xK_s     ), rofi "window"                     )
    -- dmenu actions
    , ((modm .|. shiftMask, xK_o     ), R.defaultCommands >>= R.runCommand)
    -- screenshot
    , ((0,                  xK_Print ), snip                              )
    , ((         shiftMask, xK_Print ), snipToTmp                         )
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill                              )
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout            )
    -- toggle horizontal reflect
    , ((modm .|. shiftMask, xK_v     ), sendMessage $ Toggle REFLECTX     )
    -- toggle fullscreen
    , ((modm,               xK_Escape), toggleFullscreen                  )
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh                           )
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown               )
    -- Move focus to the next window
    , ((modm,               xK_d     ), windows W.focusDown               )
    -- Move focus to the previous window
    , ((modm,               xK_a     ), windows W.focusUp                 )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster             )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster              )
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_d     ), windows W.swapDown                )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_a     ), windows W.swapUp                  )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink                )
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand                )
    -- Push window back into tiling
    , ((modm .|. shiftMask, xK_t     ), withFocused $ windows . W.sink    )
    -- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN 1)        )
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1))     )

    -- media keys
    , ((0, xF86XK_AudioLowerVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%" )
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%" )
    , ((0, xF86XK_AudioMute          ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioPlay          ), spawn "playerctl -p playerctld play-pause"       )
    , ((0, xF86XK_AudioPrev          ), spawn "playerctl -p playerctld previous"         )
    , ((0, xF86XK_AudioNext          ), spawn "playerctl -p playerctld next"             )
    , ((0, xF86XK_MonBrightnessUp    ), spawn "xbacklight -inc 5"                       )
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 5"                       )

    -- scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction scratchpads  "audio"   )
    , ((modm              , xK_b     ), namedScratchpadAction scratchpads  "password")
    , ((modm              , xK_t     ), namedScratchpadAction scratchpads  "terminal")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
--    , ((modm .|. shiftMask, xK_     ), io exitSuccess)
    -- Restart xmonad
    , ((modm              , xK_r     ), rebuild)
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
      rofi :: String -> X()
      rofi show = spawn $ "rofi -show " ++ show ++ " -theme \"/home/hannah/.config/rofi/launcher/style\""

      snip :: X()
      snip = spawn "import png:- | xclip -selection c -t image/png -i"

      snipToTmp :: X()
      snipToTmp = spawn "import png:/tmp/snip.png"

      toggleFullscreen :: X()
      toggleFullscreen = do toggleWindowSpacingEnabled
                            toggleScreenSpacingEnabled
                            sendMessage $ Toggle NOBORDERS

      rebuild :: X()
      rebuild = spawn "xmonad --recompile                  && \
                      \xmonad --restart                    && \
                      \notify-send \"rebuild xmonad\"      || \
                      \notify-send \"xmonad build failed\"    "
