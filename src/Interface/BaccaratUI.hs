module Interface.BaccaratUI (baccaratUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (PlayerID, buscarJogadorPorID, Jogador(..), registrarJogada, adicionarSaldo)
import Jogos.Baccarat (Aposta(..), calcularVencedor, multiplicadorPremio, custoBaccarat)
import System.Random (randomRIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

-- Sorteia uma carta (0 a 9)
sorteiaCarta :: IO Int
sorteiaCarta = randomRIO (0, 9)

-- Sorteia as mãos do jogador e banco
sortearMaos :: IO ([Int], [Int])
sortearMaos = do
    j1 <- sorteiaCarta
    j2 <- sorteiaCarta
    b1 <- sorteiaCarta
    b2 <- sorteiaCarta
    return ([j1, j2], [b1, b2])

-- Função para criar visual de cartas
cartaView :: Int -> UI Element
cartaView n = UI.div #+ [string (show n)]
    # set UI.style
        [ ("display", "inline-block")
        , ("width", "40px")
        , ("height", "60px")
        , ("margin", "0 6px")
        , ("background", "#fffbe6")
        , ("border", "2px solid #ffd700")
        , ("border-radius", "8px")
        , ("font-size", "2em")
        , ("font-weight", "bold")
        , ("color", "#b8860b")
        , ("text-align", "center")
        , ("line-height", "60px")
        , ("box-shadow", "2px 2px 8px rgba(0,0,0,0.15)")
        ]

baccaratUI :: Window -> PlayerID -> UI () -> UI ()
baccaratUI window playerId voltarAoMenu = do
    body <- getBody window
    void $ element body # set UI.children []

    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
            saldoDisplay <- UI.p # set UI.text ("R$ " ++ show (saldo jogador))
                # set UI.style
                    [ ("font-size", "1.1em")
                    , ("color", "#ffd700")
                    , ("font-weight", "bold")
                    , ("background", "rgba(0,0,0,0.3)")
                    , ("padding", "8px 16px")
                    , ("border-radius", "6px")
                    , ("border", "2px solid #ffd700")
                    , ("margin", "5px 0")
                    ]

            btnVoltarMenu <- UI.button #+ [string "🏠 MENU"]
                # set UI.style
                    [ ("padding", "5px 12px")
                    , ("font-size", "12px")
                    , ("font-weight", "bold")
                    , ("background", "linear-gradient(145deg, #5a2a2a, #4a1a1a)")
                    , ("color", "#ff9090")
                    , ("border", "2px solid #8a4a4a")
                    , ("border-radius", "6px")
                    , ("cursor", "pointer")
                    , ("margin", "0 5px")
                    ]

            title <- UI.h1 #+ [string "🃏 BACCARAT 🃏"]
                # set UI.style
                    [ ("color", "#ffd700")
                    , ("font-size", "2em")
                    , ("margin", "0 0 10px 0")
                    , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                    ]

            headerSection <- UI.div #+ [element title, element btnVoltarMenu]
                # set UI.style
                    [ ("display", "flex")
                    , ("flex-direction", "column")
                    , ("align-items", "center")
                    , ("margin", "10px 0")
                    ]

            resultado <- UI.div # set UI.text ""
                # set UI.style
                    [ ("font-size", "1em")
                    , ("font-weight", "bold")
                    , ("margin", "5px 0")
                    , ("min-height", "25px")
                    , ("color", "#ffd700")
                    ]

            -- Inputs de aposta e valor (criados mas só exibidos após rodada)
            apostaLabel <- UI.p #+ [string "Escolha sua aposta:"]
            apostaSelect <- UI.select #+ [UI.option # set text "Banco"
                                         ,UI.option # set text "Jogador"
                                         ,UI.option # set text "Empate"]
                # set UI.style [("margin", "0 10px")]

            valorLabel <- UI.p #+ [string "Valor da aposta (mínimo 10):"]
            valorInput <- UI.input # set (attr "type") "number"
                                  # set (attr "placeholder") "Valor"
                                  # set (attr "min") "10"
                                  # set UI.style [("margin", "0 10px")]

            btnJogar <- UI.button #+ [string "Jogar"]
                # set UI.style
                    [ ("font-size", "1.2em")
                    , ("font-weight", "bold")
                    , ("padding", "8px 20px")
                    , ("background", "linear-gradient(145deg, #ffd700, #b8860b)")
                    , ("color", "#222")
                    , ("border", "2px solid #ffd700")
                    , ("border-radius", "8px")
                    , ("cursor", "pointer")
                    , ("margin", "8px 0")
                    ]

            -- Seção do jogo
            gameSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                , ("min-height", "0")
                ]
            void $ element gameSection #+
                [ element saldoDisplay
                , element resultado
                , element apostaLabel
                , element apostaSelect
                , element valorLabel
                , element valorInput
                , element btnJogar
                ]

            contentWrapper <- UI.div # set UI.style
                [ ("padding-top", "60px")
                , ("height", "calc(100vh - 60px)")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("text-align", "center")
                , ("overflow", "hidden")
                ]
            void $ element contentWrapper #+ [element headerSection, element gameSection]
            void $ element body #+ [element contentWrapper]

            -- Evento voltar ao menu
            void $ on UI.click btnVoltarMenu $ \_ -> voltarAoMenu

            -- Funções para mostrar/esconder inputs
            let mostrarInputs = do
                    void $ element apostaLabel   # set UI.style [("display", "block")]
                    void $ element apostaSelect  # set UI.style [("display", "block")]
                    void $ element valorLabel    # set UI.style [("display", "block")]
                    void $ element valorInput    # set UI.style [("display", "block")]
                    void $ element btnJogar      # set UI.style [("display", "block")]

                esconderInputs = do
                    void $ element apostaLabel   # set UI.style [("display", "none")]
                    void $ element apostaSelect  # set UI.style [("display", "none")]
                    void $ element valorLabel    # set UI.style [("display", "none")]
                    void $ element valorInput    # set UI.style [("display", "none")]
                    void $ element btnJogar      # set UI.style [("display", "none")]

            mostrarInputs

            void $ on UI.click btnJogar $ \_ -> do
                valorStr <- get value valorInput
                apostaIdx <- get UI.selection apostaSelect
                let valorRaw = if null valorStr then 0 else read valorStr :: Float
                let valor = abs valorRaw
                let apostaEscolhida = case apostaIdx of
                        Just 0 -> Banco
                        Just 1 -> Player
                        Just 2 -> Empate
                        _      -> Banco
                saldoAtual <- liftIO $ pure (saldo jogador)
                if valor < 10
                  then void $ element resultado # set UI.text "Aposta mínima é 10!"
                                               # set UI.style [("color", "#ff6b6b")]
                else if saldoAtual < (custoBaccarat + valor)
                  then void $ element resultado # set UI.text "Saldo insuficiente!"
                                               # set UI.style [("color", "#ff6b6b")]
                else do
                    esconderInputs
                    -- Sorteia cartas e calcula resultado
                    (maoJogador, maoBanco) <- liftIO sortearMaos
                    let pontosJ = sum maoJogador `mod` 10
                        pontosB = sum maoBanco `mod` 10
                        vencedor = calcularVencedor maoJogador maoBanco
                        ganhou = apostaEscolhida == vencedor
                        premio = if ganhou
                                 then valor * multiplicadorPremio vencedor
                                 else 0

                    -- Atualiza stats e saldo
                    liftIO $ registrarJogada playerId "Baccarat" (round valor) premio
                    when ganhou $ liftIO $ adicionarSaldo playerId premio

                    -- Atualiza saldo na tela
                    maybeJogadorNovo <- liftIO $ buscarJogadorPorID playerId
                    case maybeJogadorNovo of
                        Just jNovo -> void $ element saldoDisplay # set UI.text ("R$ " ++ show (saldo jNovo))
                        Nothing    -> pure ()

                    -- Visual das cartas lado a lado
                    maoJogadorView <- UI.div #+ map cartaView maoJogador
                        # set UI.style [("display", "flex"), ("flex-direction", "row"), ("justify-content", "center"), ("margin-bottom", "8px")]
                    maoBancoView   <- UI.div #+ map cartaView maoBanco
                        # set UI.style [("display", "flex"), ("flex-direction", "row"), ("justify-content", "center"), ("margin-bottom", "8px")]

                    -- Exibe tudo na interface
                    void $ element resultado # set UI.children []
                    void $ element resultado #+
                        [ UI.div # set UI.style [("display", "flex"), ("flex-direction", "row"), ("justify-content", "center"), ("gap", "60px")] #+
                            [ UI.div #+ [UI.h3 #+ [string "Cartas do Jogador"], element maoJogadorView, UI.p #+ [string $ "Pontos: " ++ show pontosJ]]
                            , UI.div #+ [UI.h3 #+ [string "Cartas do Banco"], element maoBancoView, UI.p #+ [string $ "Pontos: " ++ show pontosB]]
                            ]
                        , UI.p #+ [string $ "Vencedor: " ++ show vencedor]
                        , UI.p #+ [string $ "Sua aposta: " ++ show apostaEscolhida]
                        , if ganhou
                            then UI.p #+ [string $ "Você ganhou! Prêmio: R$ " ++ show premio] # set UI.style [("color", "#00ff00")]
                            else UI.p #+ [string "Você perdeu!"] # set UI.style [("color", "#ff6b6b")]
                        ]

                    -- Botão Nova Rodada
                    novaRodadaBtn <- UI.button #+ [string "Nova Rodada"]
                        # set UI.style
                            [ ("margin-top", "18px")
                            , ("padding", "8px 20px")
                            , ("font-size", "1em")
                            , ("background", "linear-gradient(145deg, #ffd700, #b8860b)")
                            , ("color", "#222")
                            , ("border", "2px solid #ffd700")
                            , ("border-radius", "8px")
                            , ("cursor", "pointer")
                            ]
                    void $ element resultado #+ [element novaRodadaBtn]
                    void $ on UI.click novaRodadaBtn $ \_ -> do
                        void $ element resultado # set UI.children []
                        mostrarInputs

        Nothing -> do
            erro <- UI.div #+ [string "Erro: Jogador não encontrado. Por favor, volte à tela inicial."]
                # set UI.style
                    [ ("background", "rgba(139, 0, 0, 0.8)")
                    , ("color", "#ffaaaa")
                    , ("padding", "30px")
                    , ("border-radius", "10px")
                    , ("border", "2px solid #ff6b6b")
                    , ("font-size", "1.3em")
                    , ("margin", "50px auto")
                    , ("max-width", "500px")
                    ]
            void $ element body #+ [element erro]
