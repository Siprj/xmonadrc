module Main where

import Codec.Binary.UTF8.String (decodeString)
import Control.Concurrent (forkOS, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Time.Clock
import qualified DBus as D
import qualified DBus.Client as D
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

-- This part is copied from
-- https://github.com/gvolpe/nix-config/blob/master/home/programs/xmonad/config.hs
mkDbusClient :: IO D.Client
mkDbusClient = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    return dbus

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue = "#2E9AFE"
      gray = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      yellow = "#FFFF00"
      red = "#722222"
      green = "#00FF00"
      white = "#FFFFFF"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper yellow . wrap "[" "]"
          , ppVisible         = wrapper white . wrap "(" ")"
          , ppUrgent          = wrapper orange
--          , ppHidden          = wrapper gray
--          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper green . shorten 90
          }

main = do
    kbl <- newKeyboardHandling ["cz qwerty", "us"]
    timeMVar <- newEmptyMVar
    userHome' <- expand userHome
    wallpaperDirectory' <- expand wallpaperDirectory
    dbus <- mkDbusClient
    xmonad . docks . withUrgencyHook NoUrgencyHook $ ewmh def
            { manageHook = manageDocks
                <> (isFullscreen --> doFullFloat)
                <> (className =? "vlc" --> doFullFloat)
                <> manageHook def
                <> manageSpawn
            , layoutHook = smartBorders . avoidStrutsOn [U] $ layoutHook def
            , handleEventHook = ewmhDesktopsEventHook
                <> docksEventHook
                <> fullscreenEventHook
            , modMask = mod4Mask
            , logHook = dynamicLogWithPP (polybarHook dbus)
                <+> handleWallpaper wallpaperDirectory' timeMVar
            , startupHook = do
                spawnOn "1" "konsole"
                spawnOn "3" "firefox"
                setWMName "LG3D"
                ewmhDesktopsStartup
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
                , ((mod4Mask, xK_b), sendMessage ToggleStruts)
                ]
