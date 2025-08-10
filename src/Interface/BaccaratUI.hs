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

            -- Botão para explicar as regras
            btnRegras <- UI.button #+ [string "❓ COMO JOGAR"]
                # set UI.style
                    [ ("padding", "8px 16px")
                    , ("font-size", "14px")
                    , ("font-weight", "bold")
                    , ("background", "linear-gradient(145deg, #4169e1, #1e3a8a)")
                    , ("color", "#ffffff")
                    , ("border", "2px solid #6495ed")
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

            -- Card explicativo das regras (inicialmente oculto)
            regrasCard <- UI.div # set UI.style
                [ ("position", "fixed")
                , ("top", "50%")
                , ("left", "50%")
                , ("transform", "translate(-50%, -50%)")
                , ("background", "linear-gradient(145deg, #2d3748, #1a202c)")
                , ("border", "3px solid #ffd700")
                , ("border-radius", "15px")
                , ("padding", "25px")
                , ("max-width", "700px")
                , ("max-height", "80vh")
                , ("overflow-y", "auto")
                , ("z-index", "2000")
                , ("color", "#ffffff")
                , ("box-shadow", "0 10px 40px rgba(0,0,0,0.8)")
                , ("display", "none")
                ]

            -- Overlay de fundo escuro (inicialmente oculto)
            overlay <- UI.div # set UI.style
                [ ("position", "fixed")
                , ("top", "0")
                , ("left", "0")
                , ("width", "100%")
                , ("height", "100%")
                , ("background", "rgba(0,0,0,0.7)")
                , ("z-index", "1500")
                , ("display", "none")
                ]

            -- Conteúdo do card de regras
            regrasTitle <- UI.h2 #+ [string "🃏 COMO JOGAR BACCARAT 🃏"]
                # set UI.style
                    [ ("color", "#ffd700")
                    , ("text-align", "center")
                    , ("margin-bottom", "20px")
                    , ("font-size", "1.8em")
                    ]

            regrasTexto <- UI.div #+ 
                [ UI.h3 #+ [string "🎯 OBJETIVO DO JOGO"]
                    # set UI.style [("color", "#90ee90"), ("margin", "15px 0 8px 0")]
                , UI.p #+ [string "O objetivo é apostar na mão que ficará mais próxima de 9 pontos: Jogador, Banco ou Empate."]
                    # set UI.style [("margin-bottom", "15px"), ("line-height", "1.4")]

                , UI.h3 #+ [string "🃏 VALOR DAS CARTAS"]
                    # set UI.style [("color", "#90ee90"), ("margin", "15px 0 8px 0")]
                , UI.ul #+ 
                    [ UI.li #+ [string "Ás = 1 ponto"]
                    , UI.li #+ [string "Cartas 2-9 = valor nominal"]
                    , UI.li #+ [string "10, J, Q, K = 0 pontos"]
                    ] # set UI.style [("margin-bottom", "15px"), ("line-height", "1.4")]

                , UI.h3 #+ [string "🎲 COMO FUNCIONA"]
                    # set UI.style [("color", "#90ee90"), ("margin", "15px 0 8px 0")]
                , UI.ul #+ 
                    [ UI.li #+ [string "Cada mão recebe 2 cartas inicialmente"]
                    , UI.li #+ [string "A pontuação é a soma das cartas módulo 10 (só o último dígito conta)"]
                    , UI.li #+ [string "Exemplo: 7 + 8 = 15, mas vale 5 pontos"]
                    , UI.li #+ [string "A mão mais próxima de 9 vence"]
                    ] # set UI.style [("margin-bottom", "15px"), ("line-height", "1.4")]

                , UI.h3 #+ [string "💰 TIPOS DE APOSTA"]
                    # set UI.style [("color", "#90ee90"), ("margin", "15px 0 8px 0")]
                , UI.ul #+ 
                    [ UI.li #+ [string "🎮 JOGADOR: Paga 1:1 (dobra sua aposta)"]
                    , UI.li #+ [string "🏦 BANCO: Paga 1:1 (dobra sua aposta)"]
                    , UI.li #+ [string "🤝 EMPATE: Paga 8:1 (multiplica por 8)"]
                    ] # set UI.style [("margin-bottom", "15px"), ("line-height", "1.4")]

                , UI.h3 #+ [string "💡 DICAS"]
                    # set UI.style [("color", "#90ee90"), ("margin", "15px 0 8px 0")]
                , UI.ul #+ 
                    [ UI.li #+ [string "Banco tem ligeira vantagem estatística"]
                    , UI.li #+ [string "Empate paga muito mais, mas é mais difícil"]
                    , UI.li #+ [string "Aposta mínima: R$ 10"]
                    ] # set UI.style [("margin-bottom", "20px"), ("line-height", "1.4")]
                ]

            btnFecharRegras <- UI.button #+ [string "❌ FECHAR"]
                # set UI.style
                    [ ("padding", "10px 20px")
                    , ("font-size", "1.1em")
                    , ("font-weight", "bold")
                    , ("background", "linear-gradient(145deg, #dc3545, #c82333)")
                    , ("color", "#ffffff")
                    , ("border", "2px solid #dc3545")
                    , ("border-radius", "8px")
                    , ("cursor", "pointer")
                    , ("margin-top", "15px")
                    , ("display", "block")
                    , ("margin-left", "auto")
                    , ("margin-right", "auto")
                    ]

            void $ element regrasCard #+ [element regrasTitle, element regrasTexto, element btnFecharRegras]

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
            
            headerSection <- UI.div #+ [element title, 
                                            UI.div #+ [element btnVoltarMenu, element btnRegras]
                                                # set UI.style [("display", "flex"), ("gap", "10px"), ("justify-content", "center")]
                                            ]
                # set UI.style
                    [ ("display", "flex")
                    , ("flex-direction", "column")
                    , ("align-items", "center")
                    , ("margin", "10px 0")
                    ]

            void $ element contentWrapper #+ [element headerSection, element saldoDisplay, element gameContainer]
            void $ element body #+ [element navBarElem, element contentWrapper, element overlay, element regrasCard]

            -- Evento voltar ao menu
            void $ on UI.click btnVoltarMenu $ \_ -> voltarAoMenu

            -- Eventos para o card de regras
            let mostrarRegras = do
                    void $ element overlay # set UI.style [("display", "block")]
                    void $ element regrasCard # set UI.style [("display", "block")]
                
                esconderRegras = do
                    void $ element overlay # set UI.style [("display", "none")]
                    void $ element regrasCard # set UI.style [("display", "none")]

            void $ on UI.click btnRegras $ \_ -> mostrarRegras
            void $ on UI.click btnFecharRegras $ \_ -> esconderRegras
            void $ on UI.click overlay $ \_ -> esconderRegras

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
                        , ("flex-wrap", "wrap")
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