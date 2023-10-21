-- pragmas
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- imports
import Control.Monad
import Data.List qualified as L
import Data.Map qualified as M
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Search qualified as S
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap qualified as SM
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Prelude (fi, isPrefixOf)
import XMonad.Prompt (XPConfig, XPrompt, historyCompletionP, mkXPrompt)
import XMonad.Prompt qualified as P
import XMonad.StackSet qualified as W
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

{-  KeyPress Codes
    KeyPress event, serial 33, synthetic NO, window 0x1800001,
        state 0x0, keycode 123 (keysym 0x1008ff13, XF86AudioRaiseVolume), same_screen YES,

    KeyPress event, serial 33, synthetic NO, window 0x1800001,
        state 0x0, keycode 122 (keysym 0x1008ff11, XF86AudioLowerVolume), same_screen YES,

    KeyPress event, serial 33, synthetic NO, window 0x1800001,
        state 0x0, keycode 121 (keysym 0x1008ff12, XF86AudioMute), same_screen YES,
-}

-- Volume Control
raiseVolume :: KeySym
raiseVolume = 0x1008ff13

lowerVolume :: KeySym
lowerVolume = 0x1008ff11

mute :: KeySym
mute = 0x1008ff12

printScreen :: KeySym
printScreen = xK_Print

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "kitty"

-- Program to use to lock the screen.
--
myLock = "xscreensaver-command -lock"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = ["1:www", "2:term", "3:dev", "4", "5", "6", "7", "8:chat", "9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#dddddd"

myFocusedBorderColor = "#ff00ff"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

newtype AppPrompt = AppPrompt String

instance XPrompt AppPrompt where
  showXPrompt (AppPrompt n) = n ++ " "

type Application = String

type Parameters = String

launch' :: MonadIO m => Application -> Parameters -> m ()
launch' app params = spawn (app ++ " " ++ params)

launchApp :: XPConfig -> Application -> X ()
launchApp config app = do
  hc <- historyCompletionP (app `isPrefixOf`)
  mkXPrompt (AppPrompt app) config hc $ launch' app

forecast = S.searchEngine "forecast" "https://forecast.weather.gov/zipcity.php?inputstring="

images = S.searchEngine "images" "https://www.google.com/images?q="

searchEngineMap method =
  M.fromList
    [ ((noModMask, xK_d), method S.dictionary),
      ((noModMask, xK_g), method S.github),
      ((noModMask, xK_f), method forecast),
      ((noModMask, xK_h), method S.hackage),
      ((noModMask, xK_i), method images),
      ((noModMask, xK_m), method S.maps),
      ((noModMask, xK_w), method S.wikipedia)
    ]

promptConfig =
  P.def
    { P.position = P.Top,
      P.promptBorderWidth = 0,
      P.font = "xft:monospace-14",
      P.bgColor = "#2f1e2e",
      P.fgColor = "#b4b4b4",
      P.height = 24,
      P.showCompletionOnTab = True
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      -- launch dmenu
      -- ((modm, xK_p), spawn "dmenu_run -fn 'xft:monospace-14' -nb '#000000' -nf '#ca8f2d'"),
      ((modm, xK_p), spawn "rofi -show combi -modes combi -combi-modes 'window,drun,run'"),
      -- close focused window
      ((modm .|. shiftMask, xK_c), kill),
      -- lock the screen
      ((modm, xK_x), spawn myLock),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      -- Jump to the Full Layout
      ((modm, xK_f), sendMessage $ JumpToLayout "Full"),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- Move focus to the next window
      ((modm, xK_Tab), windows W.focusDown),
      -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp),
      -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modm, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Go to a Window by its name
      ((modm, xK_g), gotoMenuConfig def {menuCommand = "rofi", menuArgs = ["-dmenu", "-i", "-p", "Go to"]}),
      -- Bring a Window by its name
      ((modm, xK_b), bringMenuConfig def {menuCommand = "rofi", menuArgs = ["-dmenu", "-i", "-p", "Bring"]}),
      -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_l), sendMessage Expand),
      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

      ((mod1Mask, xK_grave), spawn "dunstctl history-pop"),
      ((mod1Mask, xK_space), spawn "dunstctl close"),
      ((mod1Mask, xK_Return), spawn "dunstctl context"),
      -- volume control buttons
      ((noModMask, mute), spawn "toggle-mute"),
      ((noModMask, raiseVolume), spawn "volume-up"),
      ((noModMask, lowerVolume), spawn "volume-down"),
      -- search
      ((modm, xK_d), S.promptSearchBrowser promptConfig "google-chrome" S.duckduckgo),
      ((modm, xK_y), S.promptSearchBrowser promptConfig "google-chrome" S.youtube),
      ((modm, xK_o), launchApp promptConfig "google-chrome"),
      ((modm, xK_slash), SM.submap $ searchEngineMap $ S.promptSearchBrowser promptConfig "google-chrome"),
      -- print the screen
      ((noModMask, printScreen), spawn "flameshot launcher"),
      -- Quit xmonad
      ((modm .|. shiftMask, xK_q), io exitSuccess),
      -- Restart xmonad
      ((modm, xK_q), spawn "xmonad --recompile; pkill xmobar; xmonad --restart"),
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --

      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --

      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --

      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w
            >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- ForceFullLayout wraps the CenteredIfSingle or CenteredLayout LayoutModifiers so that they are applied only if the screen is bigger than some lower bound.
data ForceFullLayout a = ForceFull !Dimension !Dimension deriving (Show, Read)

instance LayoutModifier ForceFullLayout Window where
  pureModifier (ForceFull w h) r _ [(onlyWindow, d)] = ([(onlyWindow, force w h r d)], Nothing)
  pureModifier _ _ _ winRects = (winRects, Nothing)

force w h s@(Rectangle rx ry rw rh) d = if rw <= w && rh <= h then s else d

forceFull ::
  Dimension ->
  Dimension ->
  -- | The layout that will be used if more than one window is open
  l a ->
  ModifiedLayout ForceFullLayout l a
forceFull w h = ModifiedLayout (ForceFull w h)

-- CenteredLayout forces other Layouts to be centered
data CenteredLayout a = CenteredLayout !Dimension !Dimension !Double !Double deriving (Show, Read)

instance LayoutModifier CenteredLayout Window where
  modifyLayout (CenteredLayout widthBound heightBound w h) wksp rect@(Rectangle rx ry rw rh) =
    if rw <= widthBound && rh <= heightBound
      then runLayout wksp rect
      else runLayout wksp $ rectangleCenterPiece w h rect

rectangleCenterPiece ratioX ratioY (Rectangle rx ry rw rh) = Rectangle startX startY width height
  where
    startX = rx + left
    startY = ry + top

    width = newSize rw left
    height = newSize rh top

    left = rw `scaleBy` ratioX
    top = rh `scaleBy` ratioY

newSize dim pos = fi $ fi dim - pos * 2

scaleBy dim ratio = floor $ fi dim * (1.0 - ratio) / 2

centeredLayout ::
  Dimension ->
  Dimension ->
  Double ->
  Double ->
  l a ->
  ModifiedLayout CenteredLayout l a
centeredLayout widthBound heightBound w h = ModifiedLayout (CenteredLayout widthBound heightBound w h)

myLayout =
  avoidStruts $
    spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $
      Full ||| others
  where
    tcm = ThreeColMid 1 (3 / 100) (1 / 2)
    centered = Accordion ||| tcm
    -- others = centerMaster centered
    -- cfgForceFull = forceFull 2560 1600
    -- cfgCenteredIfSingle = centeredIfSingle 0.7 1.0
    -- others = cfgForceFull $ cfgCenteredIfSingle centered
    cfgCentered = centeredLayout 2560 1600 0.7 1.0
    others = cfgCentered centered

-- others = cfgForceFull $ cfgCentered centered

-- default tiling algorithm partitions the screen into two panes
--  tiled = Tall nmaster delta ratio
-- --
-- -- -- The default number of windows in the master pane
--  nmaster = 1
-- --
-- -- -- Default proportion of screen occupied by master pane
--  ratio = 1 / 2
-- --
-- -- -- Percent of screen to increment by when resizing panes
--  delta = 3 / 100

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
(=~) :: forall (f :: * -> *) a. (Functor f, Eq a) => f [a] -> [a] -> f Bool
(=~) query needle = (needle `L.isInfixOf`) <$> query

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      className =? "Gimp" --> doFloat,
      resource =? "stalonetray" --> doIgnore,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore,
      stringProperty "_NET_WM_NAME" =~ "Visual Studio Code" --> viewShift "3:dev",
      className =? "discord" --> doShift "8:chat",
      className =? "Slack" --> doShift "8:chat",
      manageDocks
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook :: Event -> X All
-- myEventHook = ewmhDesktopsEventHook -- mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook :: X ()
myStartupHook =
  do
    spawnOnce "nitrogen --restore"
    <+> spawnOnce "redshift"
    <+> spawnOnce "picom"
    <+> spawnOnce "stalonetray"
    <+> spawnOnce "xscreensaver -nosplash"
    <+> spawnOnce "screensaver-poller.py"
    <+> spawnOnce "dunst"
    <+> spawnOnce "blueman-applet"
    <+> spawnOnce "nm-applet"
    <+> spawnOnce "flameshot"
    <+> spawnOnOnce "1:www" "google-chrome-stable --new-window"
    <+> spawnOnOnce "2:term" myTerminal
    <+> spawnOnOnce "8:chat" "slack"
    <+> spawnOnOnce "8:chat" "Discord"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0"
  xmonad $
    ewmhFullscreen . ewmh . docks $
      defaults
        { logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput =
                    hPutStrLn
                      xmproc
                }
        }

-- A structure containing your configuration settings, overriding fields in the
-- default config. Any you don't override will use the defaults defined in
-- xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults =
  def
    { -- simple stuff
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeys,
      mouseBindings = myMouseBindings,
      -- hooks, layouts
      layoutHook = myLayout,
      manageHook = manageSpawn <+> myManageHook,
      -- handleEventHook = myEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'alt'. Default keybindings:",
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
      "mod-button3  Set the window to floating mode and resize by dragging"
    ]
