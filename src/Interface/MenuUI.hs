-- File: src/Interface/MenuUI.hs

module Interface.MenuUI (menuUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Interface.CacaNiquelUI (cacaniquelUI)
import EstadoGlobal (buscarJogadorPorID, Jogador(..))
import Control.Monad (void)

-- | A função principal para renderizar a interface do menu.
-- Agora aceita o ID do jogador.
menuUI :: Window -> Int -> UI ()
menuUI window jogadorId = do
    -- Limpa a tela antes de renderizar o novo conteúdo
    body <- getBody window
    void $ element body # set UI.children []

    -- Estilo de fundo idêntico ao MainUI.hs
    void $ element body # set UI.style
        [ ("background-color", "#2c1f13") -- marrom escuro tipo cassino
        , ("color", "#ffd700")             -- dourado
        , ("font-family", "'Orbitron', sans-serif")
        , ("height", "100vh")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]
    
    -- Busca os dados do jogador para a nav-bar
    maybeJogador <- liftIO $ buscarJogadorPorID jogadorId
    case maybeJogador of
        Just jogador -> do
            -- Renderiza a nav-bar com os dados do jogador
            let nomeStr = "Nome: " ++ nome jogador
                idStr = "ID: " ++ show (playerID jogador)
                saldoStr = "Saldo: $" ++ show (saldo jogador)
            
            pNome <- UI.p #+ [string nomeStr] # set UI.style [("margin", "0")]
            pId <- UI.p #+ [string idStr] # set UI.style [("margin", "0")]
            pSaldo <- UI.p #+ [string saldoStr] # set UI.style [("margin", "0")]

            navBarElem <- UI.div #+ [element pNome, element pId, element pSaldo] # set UI.style
                [ ("background-color", "#3b2614")
                , ("padding", "10px 20px")
                , ("width", "100%")
                , ("display", "flex")
                , ("justify-content", "space-around")
                , ("align-items", "center")
                , ("position", "absolute")
                , ("top", "0")
                , ("left", "0")
                , ("box-shadow", "0 2px 5px #000")
                , ("font-size", "1.1em")
                ]

            void $ element body #+ [element navBarElem]

            -- Título da página do menu
            menuTitle <- UI.h1 #+ [string "Bem-vindo ao Cassino!"]
                               # set UI.style [("font-size", "3em"), ("text-shadow", "2px 2px 5px #000")]

            -- Botões dos jogos
            baccaratImg        <- UI.img # set UI.src "static/baccarat.png"      # set UI.style [("width", "100px")]
            blackjackImg       <- UI.img # set UI.src "static/blackjack.png"      # set UI.style [("width", "100px")]
            cacaniquelImg      <- UI.img # set UI.src "static/cacaniquel.png"      # set UI.style [("width", "100px")]
            caixasurpresaImg   <- UI.img # set UI.src "static/caixasurpresa.png" # set UI.style [("width", "100px")]
            roletaImg          <- UI.img # set UI.src "static/roleta.png"         # set UI.style [("width", "100px")]

            baccaratBtn        <- UI.button #+ [element baccaratImg, string " Baccarat"]
                                           # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
            blackjackBtn       <- UI.button #+ [element blackjackImg, string " Blackjack"]
                                           # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
            cacaniquelBtn      <- UI.button #+ [element cacaniquelImg, string " Caça-Níquel"]
                                           # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
            caixasurpresaBtn   <- UI.button #+ [element caixasurpresaImg, string " Caixa Surpresa"]
                                           # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]
            roletaBtn          <- UI.button #+ [element roletaImg, string " Roleta"]
                                           # set UI.style [("margin", "20px"), ("font-size", "1.2em"), ("display", "flex"), ("align-items", "center")]

            -- Eventos de clique
            on UI.click baccaratBtn      $ \_ -> liftIO $ putStrLn "Ir para Baccarat"
            on UI.click blackjackBtn     $ \_ -> liftIO $ putStrLn "Ir para Blackjack"
            on UI.click cacaniquelBtn    $ \_ -> do
                void $ element body # set UI.children []
                cacaniquelUI window
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
            void $ element body #+ [element menuTitle, element btnContainer]
        Nothing -> do
            -- Cria o elemento de parágrafo com a mensagem de erro
            erroElem <- UI.p #+ [string "Erro: Jogador não encontrado. Por favor, volte à tela inicial."]
            -- Limpa o corpo e adiciona o novo elemento como a única criança
            void $ element body # set UI.children [erroElem]