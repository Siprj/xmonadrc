import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Exit
import System.Posix.Process
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Wallpaper
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import Data.Monoid ((<>))

main = EH.withCustomHelper conf
  where
    conf = EH.defaultConfig
        { EH.run = oldMain
        , EH.compile = \_ -> EH.compileUsingShell "cd /home/yrid/xmonadrc/ && cabal build 2> /home/yrid/xmonadrc/build.log"
        , EH.postCompile = postCompile
        }

postCompile :: ExitCode -> IO ()
postCompile ExitSuccess = return ()
postCompile es@(ExitFailure _) = do
    errMsg <- readFile "/home/yrid/xmonadrc/build.log"
    putStrLn errMsg
    _ <- forkProcess $ executeFile "xmessage" True ["-default", "okay", errMsg] Nothing
    return ()

randomWallpaper :: IO ()
randomWallpaper = forever $ do
    setRandomWallpaper ["/home/yrid-backup/Dropbox/Wallpapers/"]
    threadDelay 3600000000

oldMain = do
    forkIO $ randomWallpaper
    xmproc <- spawnPipe "xmobar"

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
            [ --((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
              ((0, 0x1008FF11), spawn $ "amixer set Master 1-")
            , ((0, 0x1008FF13), spawn "amixer set Master 1+")
            , ((mod4Mask, 0x1008FF11), spawn "amixer set Master 3-")
            , ((mod4Mask, 0x1008FF13), spawn "amixer set Master 3+")
            , ((0, 0x1008FF12), spawn "amixer -D pulse set Master toggle")
            ]

