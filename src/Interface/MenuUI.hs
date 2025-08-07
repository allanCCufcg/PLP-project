module Interface.MenuUI (menuUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

-- | A função principal para renderizar a interface do menu.
menuUI :: Window -> UI ()
menuUI window = do
    -- Estilo de fundo idêntico ao MainUI.hs
    body <- getBody window
    element body # set UI.style
        [ ("background-color", "#2c1f13") -- marrom escuro tipo cassino
        , ("color", "#ffd700")            -- dourado
        , ("font-family", "'Orbitron', sans-serif")
        , ("height", "100vh")
        , ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]
    
    -- Título da página do menu
    menuTitle <- UI.h1 #+ [string "Bem-vindo ao Cassino!"]
                       # set UI.style [("font-size", "3em"), ("text-shadow", "2px 2px 5px #000")]

    -- Adiciona o título ao corpo da página. A última linha do bloco `do`
    -- precisa retornar o tipo `UI ()`, que é feito pelo `return ()`.
    element body #+ [element menuTitle]
    return ()
