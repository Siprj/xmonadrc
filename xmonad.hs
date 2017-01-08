import Control.Concurrent (forkOS, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Monad (forever, void)
import qualified Data.Map as M
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import System.Exit
import System.IO
import System.Posix.Process
import System.Process

import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioRaiseVolume
    , xF86XK_AudioLowerVolume
    )
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Types (Direction2D(U))
import XMonad.Wallpaper
import XMonad.Wallpaper.Expand (expand)

xmonadSandbox = "$HOME/xmonadrc/"
xmonadBinDir = "$HOME/bin/"
wallpaperDirectories = ["$HOME/Dropbox/Wallpapers/"]

-- {{{ Wallpaper setup

randomWallpaper :: IO ()
randomWallpaper = do
    setRandomWallpaper wallpaperDirectories
    forever $ do
        setRandomWallpaper wallpaperDirectories
        threadDelay 3600000000

-- }}} Wallpaper setup

-- {{{ Keyboard layout switching

newKeyboardHandling :: [String] -> IO (IORef [String])
newKeyboardHandling k = newIORef k

rotateKeyboardLayouts :: [String] -> [String]
rotateKeyboardLayouts [] = []
rotateKeyboardLayouts (x:[]) = [x]
rotateKeyboardLayouts (x:xs) = reverse $ x:(reverse xs)

safeHead :: a -> [a] -> a
safeHead d (x:_) = x
safeHead d _ = d

keyboardLoop :: IORef [String] -> IO ()
keyboardLoop c = do
    k <- readIORef c
    spawn $ "setxkbmap " <> safeHead "us" k
    writeIORef c $ rotateKeyboardLayouts k

switchKeyboardLayout :: IORef [String] -> IO ()
switchKeyboardLayout c = do
    keyboardLoop c

-- }}} Keyboard layout switching

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [((modm .|. mask, key), f sc)
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
    ]

main = do
    kbl <- newKeyboardHandling ["cz", "us"]
--    setRandomWallpaper wallpaperDirectories
    forkOS $ randomWallpaper
    cfg <- statusBar xmobarCmd xmobarPP' hidStatusBarShortcut . withUrgencyHook NoUrgencyHook $ ewmh def
        { manageHook = manageDocks <> (className =? "vlc" --> doFullFloat)  <> (isFullscreen --> doFullFloat) <> manageHook def <> manageSpawn
        , layoutHook = avoidStrutsOn [U] $ layoutHook def
        , handleEventHook = fullscreenEventHook <> docksEventHook
        , modMask = mod4Mask
        , keys = myKeys <> keys def
        , startupHook = do
            spawnOn "1" "xterm"
            spawnOn "3" "firefox"
            spawnOn "8" "quassel"
        } `additionalKeys`
            [ ((mod1Mask, xK_z), spawn "slock")
            , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D default set Master 5%-")
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D default set Master 5%+")
            , ((mod4Mask, 0x1008FF11), spawn "amixer -D default set Master 1%-")
            , ((mod4Mask, 0x1008FF13), spawn "amixer -D default set Master 1%+")
            , ((0, 0x1008FF12), spawn "amixer -D defalt set Master toggle")
            , ((mod1Mask, xK_Shift_L), liftIO $ switchKeyboardLayout kbl)
            ]
    xmonad cfg
  where
    xmobarPP' = xmobarPP
        { ppTitle = xmobarColor "green" "" . shorten 50
        , ppUrgent = xmobarColor "red" ""
        }
    xmobarCmd = "xmobar ~/xmonadrc/xmobarrc.hs"
    hidStatusBarShortcut XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
