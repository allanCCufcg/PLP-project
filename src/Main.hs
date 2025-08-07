module Main where

import Graphics.UI.Threepenny.Core (startGUI, defaultConfig)
import qualified Graphics.UI.Threepenny.Core as UI
import Interface.MainUI (mainUI)
import EstadoGlobal (resetarDados)

main :: IO ()
main = do
    resetarDados
    startGUI defaultConfig
        { UI.jsPort = Just 8023
        , UI.jsStatic = Just "static"
        } mainUI
