module Interface.BaccaratUI (baccaratUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (PlayerID, buscarJogadorPorID, Jogador(..), registrarJogada, adicionarSaldo)
import Jogos.Baccarat (Aposta(..), calcularVencedor, multiplicadorPremio, custoBaccarat)
import System.Random (randomRIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

-- Tipos de valor das cartas
data ValorCarta = As | Numero Int | Valete | Dama | Rei
data Carta = Carta { valor :: ValorCarta, naipe :: String }

-- Lista de naipes
naipes :: [String]
naipes = ["♠", "♥", "♦", "♣"]

-- Gera uma carta aleatória
gerarCarta :: IO Carta
gerarCarta = do
    valorIdx <- randomRIO (0, 12)
    naipeIdx <- randomRIO (0, 3)
    let valorCarta = case valorIdx of
            0 -> As
            n | n >= 1 && n <= 9 -> Numero n
            10 -> Valete
            11 -> Dama
            12 -> Rei
            _ -> As
    let naipeCarta = naipes !! naipeIdx
    return $ Carta valorCarta naipeCarta

-- Converte uma carta para seu valor numérico no Baccarat (0-9)
valorBaccarat :: Carta -> Int
valorBaccarat carta = case valor carta of
    As -> 1
    Numero n -> n
    Valete -> 0
    Dama -> 0
    Rei -> 0

-- Sorteia duas cartas para uma mão
sortearMao :: IO ([Carta], Int)
sortearMao = do
    c1 <- gerarCarta
    c2 <- gerarCarta
    let cartas = [c1, c2]
    let pontos = sum (map valorBaccarat cartas) `mod` 10
    return (cartas, pontos)

-- Função para criar visual de cartas usando o mesmo estilo do Blackjack
cartaVisual :: Carta -> UI Element
cartaVisual carta = do
    let v = valor carta
        (valorStr, _) = case v of
            As         -> ("A", 0)
            Numero n   -> (show n, n)
            Valete     -> ("J", 1)
            Dama       -> ("Q", 2)
            Rei        -> ("K", 3)
        naipeCarta = naipe carta
        cor = if naipeCarta == "♥" || naipeCarta == "♦" then "#e53935" else "#222"
    UI.div # set UI.style
        [ ("display", "inline-block")
        , ("width", "50px")
        , ("height", "75px")
        , ("margin", "0 8px")
        , ("background", "white")
        , ("border", "2px solid #333")
        , ("border-radius", "8px")
        , ("box-shadow", "0 4px 15px rgba(0,0,0,0.3)")
        , ("font-size", "1.1em")
        , ("color", cor)
        , ("position", "relative")
        ]
        # set UI.class_ "baccarat-carta"
        #+ [ UI.span # set UI.text naipeCarta
                     # set UI.style [ ("position","absolute")
                                   , ("top","4px")
                                   , ("left","6px")
                                   , ("font-size","1.3em")
                                   , ("font-weight","bold")
                                   ]
           , UI.span # set UI.text valorStr
                     # set UI.style [ ("position","absolute")
                                   , ("top","50%")
                                   , ("left","50%")
                                   , ("transform","translate(-50%,-50%)")
                                   , ("font-size","1.8em")
                                   , ("font-weight","bold")
                                   ]
           ]

-- Função para criar visual de uma mão de cartas
cartasVisuais :: [Carta] -> UI Element
cartasVisuais cs = UI.div #+ map cartaVisual cs
    # set UI.style [("display", "flex"), ("justify-content", "center"), ("flex-wrap", "wrap")]

baccaratUI :: Window -> PlayerID -> UI () -> UI ()
baccaratUI window playerId voltarAoMenu = do
    body <- getBody window
    void $ element body # set UI.children []

    -- Adicionar CSS para responsividade
    void $ UI.mkElement "style" # set UI.html 
        "@media (max-width: 768px) { \
        \  .baccarat-nav { flex-direction: column !important; gap: 8px !important; padding: 12px 15px !important; min-height: 80px !important; } \
        \  .baccarat-info { flex-wrap: wrap !important; gap: 8px !important; font-size: 0.85em !important; justify-content: center !important; } \
        \  .baccarat-container { min-width: 90% !important; padding: 15px !important; margin: 10px 15px !important; } \
        \  .baccarat-inputs { flex-direction: column !important; align-items: center !important; gap: 15px !important; } \
        \  .baccarat-inputs input, .baccarat-inputs select { min-width: 250px !important; margin: 0 !important; } \
        \  .baccarat-cartas { flex-direction: column !important; gap: 20px !important; } \
        \  .baccarat-carta { width: 40px !important; height: 60px !important; margin: 0 4px !important; } \
        \  .baccarat-titulo { font-size: 2em !important; } \
        \} \
        \@media (max-width: 480px) { \
        \  .baccarat-info span { display: block !important; margin: 2px 0 !important; text-align: center !important; } \
        \  .baccarat-container { margin: 8px 10px !important; padding: 12px !important; } \
        \  .baccarat-carta { width: 35px !important; height: 52px !important; font-size: 0.9em !important; } \
        \  .baccarat-titulo { font-size: 1.8em !important; } \
        \  .baccarat-inputs input, .baccarat-inputs select { min-width: 200px !important; } \
        \}"
    
    -- Estilo de fundo luxuoso
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #0f1419, #1a2332, #2d3748)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("overflow-x", "hidden")
        , ("overflow-y", "auto")
        ]

    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
            -- Nav-bar elegante
            let nomeStr = "👤 " ++ nome jogador
                idStr = "🎫 ID: " ++ show (playerID jogador)
                saldoStr = "💰 $" ++ show (saldo jogador)
            
            pNome <- UI.span #+ [string nomeStr] 
                            # set UI.style [("margin", "0"), ("color", "#ffd700"), ("font-weight", "bold")]
            pId <- UI.span #+ [string idStr] 
                          # set UI.style [("margin", "0"), ("color", "#ffd700"), ("font-weight", "bold")]
            pSaldo <- UI.span #+ [string saldoStr] 
                             # set UI.style [("margin", "0"), ("color", "#ffd700"), ("font-weight", "bold")]

            navBarElem <- UI.div #+ [UI.div #+ [element pNome, element pId, element pSaldo] # set UI.class_ "baccarat-info"] 
                                # set UI.style 
                                    [ ("background", "rgba(0,0,0,0.5)")
                                    , ("padding", "12px 20px")
                                    , ("width", "100%")
                                    , ("min-height", "50px")
                                    , ("display", "flex")
                                    , ("justify-content", "space-around")
                                    , ("align-items", "center")
                                    , ("position", "fixed")
                                    , ("top", "0")
                                    , ("left", "0")
                                    , ("z-index", "1000")
                                    , ("box-shadow", "0 4px 20px rgba(0, 0, 0, 0.8)")
                                    , ("border-bottom", "3px solid #ffd700")
                                    , ("box-sizing", "border-box")
                                    ]
                                # set UI.class_ "baccarat-nav"

            -- Container principal
            contentWrapper <- UI.div # set UI.style 
                [ ("padding-top", "70px")
                , ("min-height", "calc(100vh - 70px)")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "flex-start")
                , ("text-align", "center")
                , ("padding-bottom", "20px")
                , ("box-sizing", "border-box")
                ]

            -- Título elegante
            title <- UI.h1 #+ [string "🃏 BACCARAT 🃏"]
                # set UI.style
                    [ ("color", "#ffd700")
                    , ("font-size", "clamp(2em, 5vw, 2.5em)")
                    , ("margin", "10px 0 20px 0")
                    , ("text-shadow", "3px 3px 6px rgba(0,0,0,0.8)")
                    , ("font-weight", "bold")
                    ]
                # set UI.class_ "baccarat-titulo"

            -- Botão voltar elegante
            btnVoltarMenu <- UI.button #+ [string "🏠 VOLTAR AO MENU"]
                # set UI.style
                    [ ("padding", "8px 16px")
                    , ("font-size", "clamp(0.9em, 2.2vw, 1.1em)")
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
                    [ ("font-size", "clamp(1.1em, 3vw, 1.4em)")
                    , ("color", "#ffd700")
                    , ("font-weight", "bold")
                    , ("background", "rgba(0,0,0,0.6)")
                    , ("padding", "12px 20px")
                    , ("border-radius", "12px")
                    , ("border", "3px solid #ffd700")
                    , ("margin", "15px 20px")
                    , ("box-shadow", "0 4px 15px rgba(255,215,0,0.3)")
                    , ("max-width", "400px")
                    , ("width", "90%")
                    ]

            -- Container do jogo principal
            gameContainer <- UI.div # set UI.style
                [ ("background", "rgba(0,0,0,0.4)")
                , ("border", "3px solid #ffd700")
                , ("border-radius", "20px")
                , ("padding", "25px")
                , ("margin", "10px 20px")
                , ("width", "90%")
                , ("max-width", "800px")
                , ("min-width", "300px")
                , ("box-shadow", "0 8px 30px rgba(0,0,0,0.6)")
                , ("box-sizing", "border-box")
                ]
                # set UI.class_ "baccarat-container"

            -- Área de resultado
            resultado <- UI.div # set UI.text ""
                # set UI.style
                    [ ("font-size", "clamp(1em, 2.5vw, 1.2em)")
                    , ("font-weight", "bold")
                    , ("margin", "15px 0")
                    , ("min-height", "30px")
                    , ("color", "#ffd700")
                    ]

            -- Inputs elegantes
            apostaLabel <- UI.p #+ [string "🎯 Escolha sua aposta:"]
                # set UI.style
                    [ ("font-size", "clamp(1em, 2.8vw, 1.2em)")
                    , ("color", "#ffffff")
                    , ("margin", "15px 0 8px 0")
                    , ("font-weight", "bold")
                    ]

            apostaSelect <- UI.select #+ [UI.option # set text "Banco" # set value "banco"
                                         ,UI.option # set text "Jogador" # set value "jogador"  
                                         ,UI.option # set text "Empate" # set value "empate"]
                # set UI.style 
                    [ ("margin", "0 10px")
                    , ("padding", "10px 15px")
                    , ("font-size", "clamp(1em, 2.5vw, 1.1em)")
                    , ("background", "#2d3748")
                    , ("color", "#ffffff")
                    , ("border", "2px solid #ffd700")
                    , ("border-radius", "8px")
                    , ("min-width", "150px")
                    ]

            valorLabel <- UI.p #+ [string "💎 Valor da aposta (mínimo R$ 10):"]
                # set UI.style
                    [ ("font-size", "clamp(1em, 2.8vw, 1.2em)")
                    , ("color", "#ffffff")
                    , ("margin", "15px 0 8px 0")
                    , ("font-weight", "bold")
                    ]

            valorInput <- UI.input # set (attr "type") "number"
                                  # set (attr "placeholder") "Digite o valor"
                                  # set (attr "min") "10"
                                  # set UI.style 
                                      [ ("margin", "0 10px")
                                      , ("padding", "12px 15px")
                                      , ("font-size", "clamp(1em, 2.5vw, 1.1em)")
                                      , ("background", "#2d3748")
                                      , ("color", "#ffffff")
                                      , ("border", "2px solid #ffd700")
                                      , ("border-radius", "8px")
                                      , ("min-width", "200px")
                                      , ("text-align", "center")
                                      , ("width", "90%")
                                      , ("max-width", "300px")
                                      ]

            -- Botão jogar elegante
            btnJogar <- UI.button #+ [string "🎰 JOGAR AGORA!"]
                # set UI.style
                    [ ("font-size", "clamp(1.1em, 3vw, 1.4em)")
                    , ("font-weight", "bold")
                    , ("padding", "12px 25px")
                    , ("background", "linear-gradient(145deg, #ffd700, #b8860b)")
                    , ("color", "#000000")
                    , ("border", "3px solid #ffd700")
                    , ("border-radius", "12px")
                    , ("cursor", "pointer")
                    , ("margin", "20px 0")
                    , ("box-shadow", "0 6px 20px rgba(255,215,0,0.4)")
                    , ("width", "90%")
                    , ("max-width", "300px")
                    ]

            -- Seção do jogo
            gameSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                ]

            inputsContainer <- UI.div #+ 
                [ element apostaLabel
                , element apostaSelect
                , element valorLabel
                , element valorInput
                , element btnJogar
                ]
                # set UI.style
                    [ ("display", "flex")
                    , ("flex-direction", "column")
                    , ("align-items", "center")
                    , ("gap", "10px")
                    ]
                # set UI.class_ "baccarat-inputs"

            void $ element gameSection #+
                [ element resultado
                , element inputsContainer
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
                    void $ element inputsContainer # set UI.style [("display", "flex"), ("flex-direction", "column"), ("align-items", "center"), ("gap", "10px")]

                esconderInputs = do
                    void $ element inputsContainer # set UI.style [("display", "none")]

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
                                               # set UI.style [("color", "#ff6b6b"), ("font-size", "clamp(1em, 2.8vw, 1.2em)")]
                else if saldoAtual < (custoBaccarat + valor)
                  then void $ element resultado # set UI.text "💸 Saldo insuficiente!"
                                               # set UI.style [("color", "#ff6b6b"), ("font-size", "clamp(1em, 2.8vw, 1.2em)")]
                else do
                    esconderInputs
                    -- Sorteia cartas e calcula resultado
                    (cartasJogador, pontosJ) <- liftIO sortearMao
                    (cartasBanco, pontosB) <- liftIO sortearMao
                    
                    -- Converte as cartas para o formato esperado pela lógica do Baccarat
                    let maoJogadorInt = map valorBaccarat cartasJogador
                        maoBancoInt = map valorBaccarat cartasBanco
                        vencedor = calcularVencedor maoJogadorInt maoBancoInt
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
                    maoJogadorView <- cartasVisuais cartasJogador
                        # set UI.style 
                            [ ("margin-bottom", "12px")
                            , ("padding", "15px")
                            , ("background", "rgba(0,100,0,0.2)")
                            , ("border-radius", "10px")
                            , ("border", "2px solid #90ee90")
                            ]
                    
                    maoBancoView <- cartasVisuais cartasBanco
                        # set UI.style 
                            [ ("margin-bottom", "12px")
                            , ("padding", "15px")
                            , ("background", "rgba(100,0,0,0.2)")
                            , ("border-radius", "10px")
                            , ("border", "2px solid #ff9090")
                            ]

                    -- Títulos das seções
                    jogadorTitle <- UI.h3 #+ [string "🎮 JOGADOR"]
                        # set UI.style [("color", "#90ee90"), ("margin", "5px 0"), ("font-size", "clamp(1em, 2.5vw, 1.2em)")]
                    bancoTitle <- UI.h3 #+ [string "🏦 BANCO"] 
                        # set UI.style [("color", "#ff9090"), ("margin", "5px 0"), ("font-size", "clamp(1em, 2.5vw, 1.2em)")]
                    
                    jogadorPontos <- UI.p #+ [string $ "Pontos: " ++ show pontosJ]
                        # set UI.style [("font-size", "clamp(1em, 2.5vw, 1.2em)"), ("font-weight", "bold"), ("color", "#90ee90")]
                    bancoPontos <- UI.p #+ [string $ "Pontos: " ++ show pontosB]
                        # set UI.style [("font-size", "clamp(1em, 2.5vw, 1.2em)"), ("font-weight", "bold"), ("color", "#ff9090")]

                    -- Container das cartas
                    cartasContainer <- UI.div # set UI.style 
                        [ ("display", "flex")
                        , ("flex-direction", "row")
                        , ("justify-content", "center")
                        , ("gap", "30px")
                        , ("margin", "20px 0")
                        , ("flex-wrap", "wrap")
                        ]
                        # set UI.class_ "baccarat-cartas"

                    jogadorSection <- UI.div #+ [element jogadorTitle, element maoJogadorView, element jogadorPontos]
                        # set UI.style [("text-align", "center")]
                    bancoSection <- UI.div #+ [element bancoTitle, element maoBancoView, element bancoPontos]
                        # set UI.style [("text-align", "center")]
                    
                    void $ element cartasContainer #+ [element jogadorSection, element bancoSection]

                    -- Resultado final
                    vencedorText <- UI.p #+ [string $ "🏆 Vencedor: " ++ show vencedor]
                        # set UI.style 
                            [ ("font-size", "clamp(1.1em, 3vw, 1.4em)")
                            , ("font-weight", "bold")
                            , ("color", "#ffd700")
                            , ("margin", "10px 0")
                            ]
                    
                    apostaText <- UI.p #+ [string $ "🎯 Sua aposta: " ++ show apostaEscolhida]
                        # set UI.style 
                            [ ("font-size", "clamp(1em, 2.8vw, 1.2em)")
                            , ("color", "#ffffff")
                            , ("margin", "8px 0")
                            ]

                    resultadoFinal <- if ganhou
                        then UI.p #+ [string $ "🎉 VOCÊ GANHOU! Prêmio: R$ " ++ show premio] 
                             # set UI.style 
                                 [ ("color", "#00ff00")
                                 , ("font-size", "clamp(1.2em, 3.5vw, 1.5em)")
                                 , ("font-weight", "bold")
                                 , ("margin", "15px 0")
                                 , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.8)")
                                 ]
                        else UI.p #+ [string "😔 Você perdeu! Tente novamente."] 
                             # set UI.style 
                                 [ ("color", "#ff6b6b")
                                 , ("font-size", "clamp(1.1em, 3vw, 1.3em)")
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
                            , ("font-size", "clamp(1em, 2.8vw, 1.2em)")
                            , ("font-weight", "bold")
                            , ("background", "linear-gradient(145deg, #32cd32, #228b22)")
                            , ("color", "#ffffff")
                            , ("border", "3px solid #32cd32")
                            , ("border-radius", "10px")
                            , ("cursor", "pointer")
                            , ("box-shadow", "0 4px 15px rgba(50,205,50,0.3)")
                            , ("width", "90%")
                            , ("max-width", "250px")
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
                    , ("font-size", "clamp(1.1em, 3vw, 1.3em)")
                    , ("margin", "50px auto")
                    , ("max-width", "90%")
                    , ("width", "500px")
                    , ("text-align", "center")
                    ]
            void $ element body #+ [element erro]