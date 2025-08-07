module Interface.MenuUI (menuUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Interface.CacaNiquelUI (cacaniquelUI)

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
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]
    
    -- Título da página do menu
    menuTitle <- UI.h1 #+ [string "Bem-vindo ao Cassino!"]
                       # set UI.style [("font-size", "3em"), ("text-shadow", "2px 2px 5px #000")]

    -- Botões dos jogos
    baccaratImg      <- UI.img # set UI.src "static/baccarat.png"      # set UI.style [("width", "100px")]
    blackjackImg     <- UI.img # set UI.src "static/blackjack.png"     # set UI.style [("width", "100px")]
    cacaniquelImg    <- UI.img # set UI.src "static/cacaniquel.png"    # set UI.style [("width", "100px")]
    caixasurpresaImg <- UI.img # set UI.src "static/caixasurpresa.png" # set UI.style [("width", "100px")]
    roletaImg        <- UI.img # set UI.src "static/roleta.png"        # set UI.style [("width", "100px")]

    baccaratBtn      <- UI.button #+ [element baccaratImg, string " Baccarat"]
                        # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
    blackjackBtn     <- UI.button #+ [element blackjackImg, string " Blackjack"]
                        # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
    cacaniquelBtn    <- UI.button #+ [element cacaniquelImg, string " Caça-Níquel"]
                        # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
    caixasurpresaBtn <- UI.button #+ [element caixasurpresaImg, string " Caixa Surpresa"]
                        # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
    roletaBtn        <- UI.button #+ [element roletaImg, string " Roleta"]
                        # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]

    -- Eventos de clique (substitua pelas funções corretas de navegação)
    on UI.click baccaratBtn      $ \_ -> liftIO $ putStrLn "Ir para Baccarat"
    on UI.click blackjackBtn     $ \_ -> liftIO $ putStrLn "Ir para Blackjack"
    
    on UI.click cacaniquelBtn    $ \_ -> do
                                        body <- getBody window
                                        element body # set UI.children [] -- Limpa a tela
                                        cacaniquelUI window                 -- Chama a interface do jogo

    on UI.click caixasurpresaBtn $ \_ -> liftIO $ putStrLn "Ir para Caixa Surpresa"
    on UI.click roletaBtn        $ \_ -> liftIO $ putStrLn "Ir para Roleta"

    -- Container dos botões
    btnContainer <- UI.div #+ [ element baccaratBtn
                             , element blackjackBtn
                             , element cacaniquelBtn
                             , element caixasurpresaBtn
                             , element roletaBtn
                             ]
                   # set UI.style [("display", "flex"), ("gap", "30px"), ("margin-top", "40px")]

    -- Adiciona o título e os botões ao corpo da página
    element body #+ [element menuTitle, element btnContainer]
    return ()
