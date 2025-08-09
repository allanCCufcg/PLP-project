module Interface.CacaNiquelUI (cacaniquelUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.CacaNiquel as Logica
import qualified Data.Map as Map
import EstadoGlobal (PlayerID, registrarJogada, buscarJogadorPorID, Jogador(..))
import Data.IORef

-- Função para mapear símbolo para GIF
simboloParaGif :: String -> String
simboloParaGif s = case s of
    "TIGRE"   -> "static/CacaNiquel/tigre.gif"
    "CEREJA"  -> "static/CacaNiquel/cereja.gif"
    "OURO"    -> "static/CacaNiquel/ouro.gif"
    "LIMAO"   -> "static/CacaNiquel/limao.gif"
    "FLOR"    -> "static/CacaNiquel/flor.gif"
    "ESTRELA" -> "static/CacaNiquel/estrela.gif"
    _         -> "static/CacaNiquel/interrogacao.gif"

-- Função auxiliar para atualizar o display do saldo
atualizarSaldo :: Element -> PlayerID -> UI Element
atualizarSaldo saldoDisplay pid = do
    saldo <- liftIO $ Logica.mostraSaldo pid
    element saldoDisplay # set UI.text ("💰 Saldo: R$ " ++ show saldo)

-- Função auxiliar para atualizar informações do jogador na nav-bar
atualizarNavBar :: Element -> PlayerID -> UI ()
atualizarNavBar navBarElem pid = do
    maybeJogador <- liftIO $ buscarJogadorPorID pid
    case maybeJogador of
        Just jogador -> do
            let nomeStr = "Nome: " ++ nome jogador
                idStr = "ID: " ++ show (playerID jogador)
                saldoStr = "Saldo: $" ++ show (saldo jogador)
            
            pNome <- UI.p #+ [string nomeStr] # set UI.style [("margin", "0")]
            pId <- UI.p #+ [string idStr] # set UI.style [("margin", "0")]
            pSaldo <- UI.p #+ [string saldoStr] # set UI.style [("margin", "0")]

            element navBarElem # set UI.children [pNome, pId, pSaldo]
            return ()
        Nothing -> return ()

-- | Interface do Caça-Níquel com callback para navegação
cacaniquelUI :: Window -> PlayerID -> UI () -> UI ()
cacaniquelUI window playerId voltarAoMenu = do
    -- Estado para controlar se a música está mutada
    musicMuted <- liftIO $ newIORef False
    
    body <- getBody window
    element body # set UI.style
        [ ("background-color", "#1a1a1a")
        , ("color", "#ffd700")
        , ("font-family", "'Orbitron', sans-serif")
        , ("height", "100vh")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]

    -- Nav-bar similar ao MenuUI
    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
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

            title <- UI.h1 #+ [string "🎰 Caça-Níquel 🎰"]
                          # set UI.style [("font-size", "2.8em"), ("margin-bottom", "10px"), ("text-shadow", "2px 2px 8px #000"), ("margin-top", "80px")]
                           
            -- Elemento áudio para música de fundo
            audioMusica <- UI.audio
              # set UI.src "static/CacaNiquel/musica.mp3"
              # set (attr "autoplay") "true"
              # set (attr "loop") "true"
              # set (attr "controls") "false"
              # set (attr "id") "audioMusica"
              # set UI.style [("display", "none")] -- Esconde o player de áudio

            -- Container para botões de controle
            controlesContainer <- UI.div # set UI.style 
                [ ("display", "flex")
                , ("gap", "10px")
                , ("margin-bottom", "10px")
                , ("align-items", "center")
                ]

            -- Botão para mutar/desmutar música
            btnMuteMusic <- UI.button #+ [string "🔊 Som"]
                                     # set UI.style
                                         [ ("font-size", "0.9em")
                                         , ("padding", "5px 15px")
                                         , ("background", "linear-gradient(90deg, #2a5a2a, #1a4a1a)")
                                         , ("color", "#90ee90")
                                         , ("border", "1px solid #4a8a4a")
                                         , ("border-radius", "6px")
                                         , ("cursor", "pointer")
                                         ]

            -- Botão para voltar ao menu
            btnVoltarMenu <- UI.button #+ [string "Voltar ao Menu"]
                                      # set UI.style
                                          [ ("font-size", "0.9em")
                                          , ("padding", "5px 15px")
                                          , ("background", "linear-gradient(90deg, #5a2a2a, #4a1a1a)")
                                          , ("color", "#ff9090")
                                          , ("border", "1px solid #8a4a4a")
                                          , ("border-radius", "6px")
                                          , ("cursor", "pointer")
                                          ]

            -- Display do saldo do jogador
            saldoDisplay <- UI.p # set UI.text ("💰 Saldo: R$ " ++ show (saldo jogador))
                                # set UI.style 
                                    [ ("font-size", "1.4em")
                                    , ("margin", "5px 0 15px 0")
                                    , ("text-align", "center")
                                    , ("background", "linear-gradient(90deg, #2a2a2a, #1a1a1a)")
                                    , ("padding", "8px 20px")
                                    , ("border-radius", "10px")
                                    , ("border", "2px solid #444")
                                    , ("box-shadow", "0 2px 8px rgba(0,0,0,0.5)")
                                    , ("font-weight", "bold")
                                    ]

            -- Moldura da máquina com tigrinhos dançando
            tigrinhoEsquerda <- UI.img # set UI.src "static/CacaNiquel/tigrinho.gif"
                                      # set UI.style 
                                          [ ("height", "250px")
                                          , ("margin-right", "100px")
                                          , ("filter", "drop-shadow(0 0 10px #ffd700)")
                                          ]

            tigrinhoDireita <- UI.img # set UI.src "static/CacaNiquel/tigrinho.gif"
                                     # set UI.style 
                                         [ ("height", "250px")
                                         , ("margin-left", "100px")
                                         , ("filter", "drop-shadow(0 0 10px #ffd700)")
                                         , ("transform", "scaleX(-1)") -- Espelha horizontalmente
                                         ]

            maquina <- UI.div
                # set UI.style
                    [ ("background", "linear-gradient(180deg, #333 60%, #b8860b 100%)")
                    , ("border", "8px solid #ffd700")
                    , ("border-radius", "30px")
                    , ("box-shadow", "0 0 30px #000")
                    , ("padding", "40px 50px 30px 50px")
                    , ("display", "flex")
                    , ("flex-direction", "column")
                    , ("align-items", "center")
                    ]

            -- Container para a máquina com os tigrinhos
            maquinaCompleta <- UI.div # set UI.style
                [ ("display", "flex")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("margin", "20px 0")
                ]

            rolo1 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                             # set UI.style [("margin", "0 15px")]
            rolo2 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                             # set UI.style [("margin", "0 15px")]
            rolo3 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                             # set UI.style [("margin", "0 15px")]

            rolos <- UI.div #+ [element rolo1, element rolo2, element rolo3]
                           # set UI.style [("display", "flex"), ("justify-content", "center"), ("margin-bottom", "25px"), ("background", "#222"), ("border-radius", "15px"), ("padding", "15px 30px"), ("box-shadow", "0 0 10px #000 inset")]

            resultado <- UI.p # set UI.text "" # set UI.style [("font-size", "1.3em"), ("margin", "15px 0 0 0"), ("height", "2em"), ("text-align", "center")]

            -- Informação da aposta
            infoAposta <- UI.p # set UI.text ("💎 Valor da Aposta: R$ " ++ show Logica.valorAposta)
                              # set UI.style 
                                  [ ("font-size", "1.1em")
                                  , ("margin", "10px 0")
                                  , ("text-align", "center")
                                  , ("color", "#ccc")
                                  ]

            btnGirar <- UI.button #+ [string "GIRAR!"]
                        # set UI.style
                            [ ("font-size", "1.5em")
                            , ("padding", "12px 40px")
                            , ("background", "linear-gradient(90deg, #ffd700 60%, #b8860b 100%)")
                            , ("color", "#222")
                            , ("border", "none")
                            , ("border-radius", "12px")
                            , ("box-shadow", "0 4px 12px #000")
                            , ("cursor", "pointer")
                            , ("font-weight", "bold")
                            , ("margin-top", "10px")
                            , ("transition", "transform 0.1s")
                            ]

            -- Eventos do botão girar
            on UI.mousedown btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(0.95)")]
            on UI.mouseup btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(1)")]

            -- Evento para mutar/desmutar música
            on UI.click btnMuteMusic $ \_ -> do
                isMuted <- liftIO $ readIORef musicMuted
                if not isMuted
                  then do
                    -- Mutar música
                    liftIO $ writeIORef musicMuted True
                    element btnMuteMusic # set UI.text "🔇 Mudo"
                    element btnMuteMusic # set UI.style
                        [ ("font-size", "0.9em")
                        , ("padding", "5px 15px")
                        , ("background", "linear-gradient(90deg, #5a2a2a, #4a1a1a)")
                        , ("color", "#ff9090")
                        , ("border", "1px solid #8a4a4a")
                        , ("border-radius", "6px")
                        , ("cursor", "pointer")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusica').muted = true"
                  else do
                    -- Desmutar música
                    liftIO $ writeIORef musicMuted False
                    element btnMuteMusic # set UI.text "🔊 Som"
                    element btnMuteMusic # set UI.style
                        [ ("font-size", "0.9em")
                        , ("padding", "5px 15px")
                        , ("background", "linear-gradient(90deg, #2a5a2a, #1a4a1a)")
                        , ("color", "#90ee90")
                        , ("border", "1px solid #4a8a4a")
                        , ("border-radius", "6px")
                        , ("cursor", "pointer")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusica').muted = false"
                return ()

            -- Evento para voltar ao menu - SOLUÇÃO ELEGANTE!
            on UI.click btnVoltarMenu $ \_ -> voltarAoMenu

            -- Evento principal do jogo
            on UI.click btnGirar $ \_ -> do
                saldoAtual <- liftIO $ Logica.mostraSaldo playerId
                if saldoAtual < Logica.valorAposta
                  then element resultado # set UI.text "Saldo insuficiente para jogar!" >> return ()
                  else do
                    resultadoJogo <- liftIO $ Logica.girar
                    let vitoria = Logica.verificarVitoria resultadoJogo
                    element rolo1 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 0) ++ "' style='height:60px'/>")
                    element rolo2 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 1) ++ "' style='height:60px'/>")
                    element rolo3 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 2) ++ "' style='height:60px'/>")
                    if vitoria
                      then do
                        let simbolo = head resultadoJogo
                        let multiplicador = Logica.multiplicadores Map.! simbolo
                        let ganho = multiplicador * Logica.valorAposta
                        liftIO $ registrarJogada playerId "Caca-Niquel" (round Logica.valorAposta) ganho
                        element resultado # set UI.text ("🎉 Parabéns! Você ganhou R$ " ++ show ganho ++ " com três " ++ simbolo ++ "!")
                      else do
                        liftIO $ registrarJogada playerId "Caca-Niquel" (round Logica.valorAposta) 0
                        element resultado # set UI.text "Tente novamente!"
                    -- Atualiza o saldo após cada jogada
                    atualizarSaldo saldoDisplay playerId >> return ()
                    -- Atualiza a nav-bar com o novo saldo
                    atualizarNavBar navBarElem playerId

            element maquina #+ [element rolos, element infoAposta, element btnGirar, element resultado]

            element maquinaCompleta #+ [element tigrinhoEsquerda, element maquina, element tigrinhoDireita]

            element controlesContainer #+ [element btnMuteMusic, element btnVoltarMenu]

            element body #+ [element navBarElem, element title, element controlesContainer, element saldoDisplay, element maquinaCompleta, element audioMusica]

            return ()
        Nothing -> do
            -- Cria o elemento de parágrafo com a mensagem de erro
            erroElem <- UI.p #+ [string "Erro: Jogador não encontrado. Por favor, volte à tela inicial."]
            -- Limpa o corpo e adiciona o novo elemento como a única criança
            element body # set UI.children [erroElem]
            return ()