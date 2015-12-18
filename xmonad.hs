import Control.Concurrent (forkIO, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Monad (forever, void)
import Data.Monoid ((<>))
import System.Exit
import System.IO
import System.Posix.Process
import System.Process

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.EntryHelper as EH
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioRaiseVolume
    , xF86XK_AudioLowerVolume
    )
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Wallpaper
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Wallpaper.Expand (expand)

xmonadSandbox = "$HOME/xmonadrc/"
xmonadBinDir = "$HOME/bin/"
wallpaperDirectories = ["$HOME/Dropbox/Wallpapers/"]

-- {{{ Make compile and restart options work in sandbox

main = EH.withCustomHelper conf
  where
    conf = EH.defaultConfig
        { EH.run = oldMain
        , EH.compile = compileInSandbox
        , EH.postCompile = postCompile
        }

compileInSandbox :: Bool -> IO (Either String ())
compileInSandbox _ = do
    ep <- expand xmonadSandbox
    xb <- expand xmonadBinDir
    (_, _, errHandler, procHandler) <- createProcess
        (proc "cabal" ["install", "--symlink-bindir", xb])
            { cwd = Just ep
            , std_err = CreatePipe
            }
    exitCodeToEather errHandler =<<  waitForProcess procHandler
exitCodeToEather :: Maybe Handle -> ExitCode -> IO (Either String ())
exitCodeToEather _ ExitSuccess = return $ Right ()
exitCodeToEather mh (ExitFailure _) =
    case mh of
        Nothing -> return $ Left "ERROR: Can't retrieve error message from build command."
        Just h  -> Left <$> hGetContents h

postCompile :: Either String () -> IO ()
postCompile (Right _)= return ()
postCompile (Left errMsg) = do
    putStrLn errMsg
    void . forkProcess $ executeFile "xmessage" True ["-default", "okay", errMsg] Nothing

-- }}} Make compile and restart options work in sandbox

-- {{{ Wallpaper setup

randomWallpaper :: IO ()
randomWallpaper = forever $ do
    setRandomWallpaper wallpaperDirectories
    threadDelay 3600000000

-- }}} Wallpaper setup

-- {{{ Keyboard layout switching

newKeyboardHandling :: [String] -> IO (Chan ())
newKeyboardHandling k = do
    c <- newChan
    forkIO $ keyboardLoop k c
    return c

rotateKeyboardLayouts :: [String] -> [String]
rotateKeyboardLayouts [] = []
rotateKeyboardLayouts (x:[]) = [x]
rotateKeyboardLayouts (x:xs) = reverse $ x:(reverse xs)

safeHead :: a -> [a] -> a
safeHead d (x:_) = x
safeHead d _ = d

keyboardLoop :: [String] -> Chan () -> IO ()
keyboardLoop k c = do
    readChan c
    spawn $ "setxkbmap " <> safeHead "us" k
    keyboardLoop (rotateKeyboardLayouts k) c

switchKeyboardLayout :: Chan () -> IO ()
switchKeyboardLayout c =
    writeChan c ()

-- }}} Keyboard layout switching

oldMain = do
    forkIO $ randomWallpaper
    kbl <- newKeyboardHandling ["cz", "us"]
    xmproc <- spawnPipe "xmobar ~/xmonadrc/xmobarrc.hs"

    xmonad $ ewmh defaultConfig
        { manageHook = manageDocks <+> (className =? "vlc" --> doFullFloat)  <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , handleEventHook = fullscreenEventHook
        , modMask = mod4Mask
        } `additionalKeys`
            [ ((mod1Mask, xK_z), spawn "slock")
            , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -c 1 set Master 1-")
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -c 1 set Master 1+")
            , ((mod4Mask, 0x1008FF11), spawn "amixer set Master 3-")
            , ((mod4Mask, 0x1008FF13), spawn "amixer set Master 3+")
            , ((0, 0x1008FF12), spawn "amixer -D pulse set Master toggle")
            , ((mod1Mask, xK_Shift_L), liftIO $ switchKeyboardLayout kbl)
            ]
