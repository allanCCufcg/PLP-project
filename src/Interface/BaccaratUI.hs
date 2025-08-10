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

-- Função para criar visual de cartas MELHORADO
cartaView :: Int -> UI Element
cartaView n = UI.div #+ [string (show n)]
    # set UI.style
        [ ("display", "inline-block")
        , ("width", "50px")
        , ("height", "75px")
        , ("margin", "0 8px")
        , ("background", "linear-gradient(145deg, #ffffff, #f8f8f8)")
        , ("border", "3px solid #ffd700")
        , ("border-radius", "12px")
        , ("font-size", "2.2em")
        , ("font-weight", "bold")
        , ("color", "#8b0000")
        , ("text-align", "center")
        , ("line-height", "75px")
        , ("box-shadow", "0 4px 15px rgba(0,0,0,0.3)")
        , ("position", "relative")
        ]

baccaratUI :: Window -> PlayerID -> UI () -> UI ()
baccaratUI window playerId voltarAoMenu = do
    body <- getBody window
    void $ element body # set UI.children []
    
    -- Estilo de fundo luxuoso
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #0f1419, #1a2332, #2d3748)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("overflow", "hidden")
        ]

    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
            -- Nav-bar elegante
            let nomeStr = "👤 " ++ nome jogador
                idStr = "🎫 ID: " ++ show (playerID jogador)
                saldoStr = "💰 $" ++ show (saldo jogador)
            
            pNome <- UI.span #+ [string nomeStr] 
                            # set UI.style [("margin", "0"), ("color", "#ffd700")]
            pId <- UI.span #+ [string idStr] 
                          # set UI.style [("margin", "0"), ("color", "#ffd700")]
            pSaldo <- UI.span #+ [string saldoStr] 
                             # set UI.style [("margin", "0"), ("color", "#ffd700")]

            navBarElem <- UI.div #+ [element pNome, element pId, element pSaldo] 
                                # set UI.style 
                                    [ ("background", "rgba(0,0,0,0.5)")
                                    , ("padding", "12px 30px")
                                    , ("width", "100%")
                                    , ("height", "40px")
                                    , ("display", "flex")
                                    , ("justify-content", "space-around")
                                    , ("align-items", "center")
                                    , ("position", "fixed")
                                    , ("top", "0")
                                    , ("left", "0")
                                    , ("z-index", "1000")
                                    , ("box-shadow", "0 4px 20px rgba(0, 0, 0, 0.8)")
                                    , ("border-bottom", "3px solid #ffd700")
                                    ]

            -- Container principal
            contentWrapper <- UI.div # set UI.style 
                [ ("padding-top", "65px")
                , ("height", "calc(100vh - 65px)")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("text-align", "center")
                , ("overflow-y", "auto")
                ]

            -- Título elegante
            title <- UI.h1 #+ [string "🃏 BACCARAT 🃏"]
                # set UI.style
                    [ ("color", "#ffd700")
                    , ("font-size", "2.5em")
                    , ("margin", "0 0 20px 0")
                    , ("text-shadow", "3px 3px 6px rgba(0,0,0,0.8)")
                    , ("font-weight", "bold")
                    ]

            -- Botão voltar elegante
            btnVoltarMenu <- UI.button #+ [string "🏠 VOLTAR AO MENU"]
                # set UI.style
                    [ ("padding", "8px 16px")
                    , ("font-size", "14px")
                    , ("font-weight", "bold")
                    , ("background", "linear-gradient(145deg, #8b0000, #660000)")
                    , ("color", "#ffffff")
                    , ("border", "2px solid #ff6b6b")
                    , ("border-radius", "8px")
                    , ("cursor", "pointer")
                    , ("margin", "10px")
                    , ("box-shadow", "0 4px 10px rgba(0,0,0,0.3)")
                    ]

            -- Display do saldo elegante
            saldoDisplay <- UI.p # set UI.text ("💰 Saldo: R$ " ++ show (saldo jogador))
                # set UI.style
                    [ ("font-size", "1.4em")
                    , ("color", "#ffd700")
                    , ("font-weight", "bold")
                    , ("background", "rgba(0,0,0,0.6)")
                    , ("padding", "12px 24px")
                    , ("border-radius", "12px")
                    , ("border", "3px solid #ffd700")
                    , ("margin", "15px 0")
                    , ("box-shadow", "0 4px 15px rgba(255,215,0,0.3)")
                    ]

            -- Container do jogo principal
            gameContainer <- UI.div # set UI.style
                [ ("background", "rgba(0,0,0,0.4)")
                , ("border", "3px solid #ffd700")
                , ("border-radius", "20px")
                , ("padding", "25px")
                , ("margin", "10px")
                , ("min-width", "600px")
                , ("box-shadow", "0 8px 30px rgba(0,0,0,0.6)")
                ]

            -- Área de resultado
            resultado <- UI.div # set UI.text ""
                # set UI.style
                    [ ("font-size", "1.1em")
                    , ("font-weight", "bold")
                    , ("margin", "15px 0")
                    , ("min-height", "30px")
                    , ("color", "#ffd700")
                    ]

            -- Inputs elegantes
            apostaLabel <- UI.p #+ [string "🎯 Escolha sua aposta:"]
                # set UI.style
                    [ ("font-size", "1.2em")
                    , ("color", "#ffffff")
                    , ("margin", "15px 0 8px 0")
                    , ("font-weight", "bold")
                    ]

            apostaSelect <- UI.select #+ [UI.option # set text "Banco" # set value "banco"
                                         ,UI.option # set text "Jogador" # set value "jogador"  
                                         ,UI.option # set text "Empate" # set value "empate"]
                # set UI.style 
                    [ ("margin", "0 10px")
                    , ("padding", "8px 12px")
                    , ("font-size", "1.1em")
                    , ("background", "#2d3748")
                    , ("color", "#ffffff")
                    , ("border", "2px solid #ffd700")
                    , ("border-radius", "8px")
                    , ("min-width", "150px")
                    ]

            valorLabel <- UI.p #+ [string "💎 Valor da aposta (mínimo R$ 10):"]
                # set UI.style
                    [ ("font-size", "1.2em")
                    , ("color", "#ffffff")
                    , ("margin", "15px 0 8px 0")
                    , ("font-weight", "bold")
                    ]

            valorInput <- UI.input # set (attr "type") "number"
                                  # set (attr "placeholder") "Digite o valor"
                                  # set (attr "min") "10"
                                  # set UI.style 
                                      [ ("margin", "0 10px")
                                      , ("padding", "10px 15px")
                                      , ("font-size", "1.1em")
                                      , ("background", "#2d3748")
                                      , ("color", "#ffffff")
                                      , ("border", "2px solid #ffd700")
                                      , ("border-radius", "8px")
                                      , ("min-width", "200px")
                                      , ("text-align", "center")
                                      ]

            -- Botão jogar elegante
            btnJogar <- UI.button #+ [string "🎰 JOGAR AGORA!"]
                # set UI.style
                    [ ("font-size", "1.4em")
                    , ("font-weight", "bold")
                    , ("padding", "12px 30px")
                    , ("background", "linear-gradient(145deg, #ffd700, #b8860b)")
                    , ("color", "#000000")
                    , ("border", "3px solid #ffd700")
                    , ("border-radius", "12px")
                    , ("cursor", "pointer")
                    , ("margin", "20px 0")
                    , ("box-shadow", "0 6px 20px rgba(255,215,0,0.4)")
                    ]

            -- Seção do jogo
            gameSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                ]

            void $ element gameSection #+
                [ element resultado
                , element apostaLabel
                , element apostaSelect
                , element valorLabel
                , element valorInput
                , element btnJogar
                ]

            void $ element gameContainer #+ [element gameSection]
            
            headerSection <- UI.div #+ [element title, element btnVoltarMenu]
                # set UI.style
                    [ ("display", "flex")
                    , ("flex-direction", "column")
                    , ("align-items", "center")
                    , ("margin", "10px 0")
                    ]

            void $ element contentWrapper #+ [element headerSection, element saldoDisplay, element gameContainer]
            void $ element body #+ [element navBarElem, element contentWrapper]

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

            -- Click effects (Threepenny não suporta mouseenter/mouseleave)
            void $ on UI.mousedown btnJogar $ \_ -> element btnJogar # set UI.style [("transform", "scale(0.95)")]
            void $ on UI.mouseup btnJogar $ \_ -> element btnJogar # set UI.style [("transform", "scale(1)")]

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
                  then void $ element resultado # set UI.text "⚠️ Aposta mínima é R$ 10!"
                                               # set UI.style [("color", "#ff6b6b"), ("font-size", "1.2em")]
                else if saldoAtual < (custoBaccarat + valor)
                  then void $ element resultado # set UI.text "💸 Saldo insuficiente!"
                                               # set UI.style [("color", "#ff6b6b"), ("font-size", "1.2em")]
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
                        Just jNovo -> do
                            void $ element saldoDisplay # set UI.text ("💰 Saldo: R$ " ++ show (saldo jNovo))
                            void $ element pSaldo # set UI.text ("💰 $" ++ show (saldo jNovo))
                        Nothing    -> pure ()

                    -- Visual das cartas lado a lado MELHORADO
                    maoJogadorView <- UI.div #+ map cartaView maoJogador
                        # set UI.style 
                            [ ("display", "flex")
                            , ("flex-direction", "row")
                            , ("justify-content", "center")
                            , ("margin-bottom", "12px")
                            , ("padding", "10px")
                            , ("background", "rgba(0,100,0,0.2)")
                            , ("border-radius", "10px")
                            ]
                    
                    maoBancoView <- UI.div #+ map cartaView maoBanco
                        # set UI.style 
                            [ ("display", "flex")
                            , ("flex-direction", "row")
                            , ("justify-content", "center")
                            , ("margin-bottom", "12px")
                            , ("padding", "10px")
                            , ("background", "rgba(100,0,0,0.2)")
                            , ("border-radius", "10px")
                            ]

                    -- Títulos das seções
                    jogadorTitle <- UI.h3 #+ [string "🎮 JOGADOR"]
                        # set UI.style [("color", "#90ee90"), ("margin", "5px 0")]
                    bancoTitle <- UI.h3 #+ [string "🏦 BANCO"] 
                        # set UI.style [("color", "#ff9090"), ("margin", "5px 0")]
                    
                    jogadorPontos <- UI.p #+ [string $ "Pontos: " ++ show pontosJ]
                        # set UI.style [("font-size", "1.2em"), ("font-weight", "bold"), ("color", "#90ee90")]
                    bancoPontos <- UI.p #+ [string $ "Pontos: " ++ show pontosB]
                        # set UI.style [("font-size", "1.2em"), ("font-weight", "bold"), ("color", "#ff9090")]

                    -- Container das cartas
                    cartasContainer <- UI.div # set UI.style 
                        [ ("display", "flex")
                        , ("flex-direction", "row")
                        , ("justify-content", "center")
                        , ("gap", "40px")
                        , ("margin", "20px 0")
                        ]

                    jogadorSection <- UI.div #+ [element jogadorTitle, element maoJogadorView, element jogadorPontos]
                        # set UI.style [("text-align", "center")]
                    bancoSection <- UI.div #+ [element bancoTitle, element maoBancoView, element bancoPontos]
                        # set UI.style [("text-align", "center")]
                    
                    void $ element cartasContainer #+ [element jogadorSection, element bancoSection]

                    -- Resultado final
                    vencedorText <- UI.p #+ [string $ "🏆 Vencedor: " ++ show vencedor]
                        # set UI.style 
                            [ ("font-size", "1.4em")
                            , ("font-weight", "bold")
                            , ("color", "#ffd700")
                            , ("margin", "10px 0")
                            ]
                    
                    apostaText <- UI.p #+ [string $ "🎯 Sua aposta: " ++ show apostaEscolhida]
                        # set UI.style 
                            [ ("font-size", "1.2em")
                            , ("color", "#ffffff")
                            , ("margin", "8px 0")
                            ]

                    resultadoFinal <- if ganhou
                        then UI.p #+ [string $ "🎉 VOCÊ GANHOU! Prêmio: R$ " ++ show premio] 
                             # set UI.style 
                                 [ ("color", "#00ff00")
                                 , ("font-size", "1.5em")
                                 , ("font-weight", "bold")
                                 , ("margin", "15px 0")
                                 , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.8)")
                                 ]
                        else UI.p #+ [string "😔 Você perdeu! Tente novamente."] 
                             # set UI.style 
                                 [ ("color", "#ff6b6b")
                                 , ("font-size", "1.3em")
                                 , ("font-weight", "bold")
                                 , ("margin", "15px 0")
                                 ]

                    -- Exibe tudo na interface
                    void $ element resultado # set UI.children []
                    void $ element resultado #+ 
                        [ element cartasContainer
                        , element vencedorText
                        , element apostaText  
                        , element resultadoFinal
                        ]

                    -- Botão Nova Rodada elegante
                    novaRodadaBtn <- UI.button #+ [string "🔄 NOVA RODADA"]
                        # set UI.style
                            [ ("margin-top", "20px")
                            , ("padding", "10px 25px")
                            , ("font-size", "1.2em")
                            , ("font-weight", "bold")
                            , ("background", "linear-gradient(145deg, #32cd32, #228b22)")
                            , ("color", "#ffffff")
                            , ("border", "3px solid #32cd32")
                            , ("border-radius", "10px")
                            , ("cursor", "pointer")
                            , ("box-shadow", "0 4px 15px rgba(50,205,50,0.3)")
                            ]
                    
                    void $ element resultado #+ [element novaRodadaBtn]
                    void $ on UI.click novaRodadaBtn $ \_ -> do
                        void $ element resultado # set UI.children []
                        mostrarInputs

        Nothing -> do
            erro <- UI.div #+ [string "⚠️ Erro: Jogador não encontrado. Por favor, volte à tela inicial."]
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