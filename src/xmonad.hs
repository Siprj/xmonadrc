module Main where

import Control.Concurrent (forkOS, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Time.Clock
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
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioRaiseVolume
    , xF86XK_AudioLowerVolume
    , xF86XK_MonBrightnessDown
    , xF86XK_MonBrightnessUp
    )
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Types (Direction2D(U))
import XMonad.Hooks.WallpaperSetter

import Expand (expand)


userHome = "$HOME"
wallpaperDirectory = "${HOME}/Dropbox/Wallpapers/"

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

handleWallpaper :: FilePath -> MVar UTCTime -> X ()
handleWallpaper wallpaperDirectory' mvar = do
    tmp <- liftIO $ tryTakeMVar mvar
    maybe (storeTime >> setWapppaper) go tmp
  where
    storeTime = liftIO $ getCurrentTime >>=
        (putMVar mvar . addUTCTime timeDiff)
    go v = do
        now <- liftIO getCurrentTime
        if v > now
            then liftIO $ putMVar mvar v
            else storeTime >> setWapppaper
    timeDiff = fromIntegral 3600
    setWapppaper = wallpaperSetter $ WallpaperConf wallpaperDirectory'
        . WallpaperList $ fmap (\w -> (w, WallpaperDir "")) $ workspaces def

main = do
    kbl <- newKeyboardHandling ["cz qwerty", "us"]
    timeMVar <- newEmptyMVar
    userHome' <- expand userHome
    wallpaperDirectory' <- expand wallpaperDirectory
    cfg <- statusBar xmobarCmd xmobarPP' hidStatusBarShortcut
        . withUrgencyHook NoUrgencyHook $ ewmh def
            { manageHook = manageDocks
                <> (isFullscreen --> doFullFloat)
                <> (className =? "vlc" --> doFullFloat)
                <> manageHook def
                <> manageSpawn
            , layoutHook = smartBorders . avoidStrutsOn [U] $ layoutHook def
            , handleEventHook = fullscreenEventHook <> docksEventHook
            , modMask = mod4Mask
            , logHook = handleWallpaper wallpaperDirectory' timeMVar
            , startupHook = do
                spawnOn "1" "konsole"
                spawnOn "3" "firefox"
                setWMName "LG3D"
            , terminal = "konsole"
            } `additionalKeys`
                [ ((mod1Mask, xK_l), spawn "slock")
                , ((0, xK_Print), spawn $ "scrot " <> userHome'
                    <> "/ScreenShots/screen_%Y-%m-%d-%H-%M-%S.png -d 1 -u"
                  )
                , ( (0, xF86XK_AudioLowerVolume)
                  , spawn "amixer -D default set Master 5%-"
                  )
                , ( (0, xF86XK_AudioRaiseVolume)
                  , spawn "amixer -D default set Master 5%+"
                  )
                , ((0, xF86XK_MonBrightnessDown) , spawn "xbacklight -10")
                , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
                , ( (mod4Mask, 0x1008FF11)
                  , spawn "amixer -D default set Master 1%-"
                  )
                , ( (mod4Mask, 0x1008FF13)
                  , spawn "amixer -D default set Master 1%+"
                  )
                , ((0, 0x1008FF12), spawn "amixer -D pulse set Master toggle")
                , ((mod1Mask, xK_Shift_L), liftIO $ switchKeyboardLayout kbl)
                , ((mod4Mask, xK_p), spawn "rofi -no-lazy-grab -show drun -modi drun")
                , ((mod4Mask .|. shiftMask, xK_p), spawn $ userHome' <> "/install/powermenu.sh")
                ]
    xmonad cfg
  where
    xmobarPP' = xmobarPP
        { ppTitle = xmobarColor "green" "" . shorten 50
        , ppUrgent = xmobarColor "red" ""
        }
    xmobarCmd = "xmobar ~/xmonadrc/xmobarrc.hs"
    hidStatusBarShortcut XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
