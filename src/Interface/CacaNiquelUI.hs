module Interface.CacaNiquelUI (cacaniquelUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.CacaNiquel as Logica
import qualified Data.Map as Map
import EstadoGlobal (PlayerID, registrarJogada, buscarJogadorPorID, Jogador(..))
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

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

-- | Interface do Caça-Níquel com nav-bar estilizada igual ao MenuUI
cacaniquelUI :: Window -> PlayerID -> UI () -> UI ()
cacaniquelUI window playerId voltarAoMenu = do
    -- Estado para controlar se a música está mutada
    musicMuted <- liftIO $ newIORef False
    
    body <- getBody window
    void $ element body # set UI.children []
    
    -- Estilo de fundo do body
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("overflow", "hidden")
        ]

    -- Busca os dados do jogador para a nav-bar
    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
            -- Nav-bar FIXA NO TOPO (altura 60px)
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
                                    [ ("background", "rgba(0,0,0,0.3)")
                                    , ("padding", "10px 30px")
                                    , ("width", "100%")
                                    , ("height", "40px")
                                    , ("display", "flex")
                                    , ("justify-content", "space-around")
                                    , ("align-items", "center")
                                    , ("position", "fixed")
                                    , ("top", "0")
                                    , ("left", "0")
                                    , ("z-index", "1000")
                                    , ("box-shadow", "0 2px 10px rgba(0, 0, 0, 0.7)")
                                    , ("font-size", "1em")
                                    , ("border-bottom", "2px solid #ffd700")
                                    ]

            -- Container principal do conteúdo (altura calculada: 100vh - 60px nav)
            contentWrapper <- UI.div # set UI.style 
                [ ("padding-top", "60px")
                , ("height", "calc(100vh - 60px)")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "space-between")
                , ("text-align", "center")
                , ("overflow", "hidden")
                ]

            -- Header compacto com título e controles
            headerSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("margin", "10px 0")
                ]

            -- Título menor
            title <- UI.h1 #+ [string "🎰 CAÇA-NÍQUEL 🎰"]
                          # set UI.style 
                              [ ("color", "#ffd700")
                              , ("font-size", "1.8em")
                              , ("margin", "0 0 10px 0")
                              , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                              ]

            -- Botões de controle compactos
            btnMuteMusic <- UI.button #+ [string "🔊"]
                                     # set UI.style
                                         [ ("padding", "5px 10px")
                                         , ("font-size", "14px")
                                         , ("background", "linear-gradient(145deg, #2a5a2a, #1a4a1a)")
                                         , ("color", "#90ee90")
                                         , ("border", "2px solid #4a8a4a")
                                         , ("border-radius", "6px")
                                         , ("cursor", "pointer")
                                         , ("margin", "0 5px")
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

            controlesContainer <- UI.div #+ [element btnMuteMusic, element btnVoltarMenu]
                                        # set UI.style 
                                            [ ("display", "flex")
                                            , ("justify-content", "center")
                                            , ("margin", "5px 0")
                                            ]

            -- Adicionar título e controles ao header
            void $ element headerSection #+ [element title, element controlesContainer]

            -- Seção do jogo (flex-grow para ocupar o espaço disponível)
            gameSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                , ("min-height", "0")
                ]

            -- Display do saldo compacto
            saldoDisplay <- UI.p # set UI.text ("💰 R$ " ++ show (saldo jogador))
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

            -- Tigrinhos menores
            tigrinhoEsquerda <- UI.img # set UI.src "static/CacaNiquel/tigrinho.gif"
                                      # set UI.style 
                                          [ ("height", "120px")
                                          , ("margin-right", "20px")
                                          ]

            tigrinhoDireita <- UI.img # set UI.src "static/CacaNiquel/tigrinho.gif"
                                     # set UI.style 
                                         [ ("height", "120px")
                                         , ("margin-left", "20px")
                                         , ("transform", "scaleX(-1)")
                                         ]

            -- Rolos da máquina menores
            rolo1 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:50px; border-radius:6px;'/>"
                             # set UI.style [("margin", "0 5px")]
            rolo2 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:50px; border-radius:6px;'/>"
                             # set UI.style [("margin", "0 5px")]
            rolo3 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:50px; border-radius:6px;'/>"
                             # set UI.style [("margin", "0 5px")]

            rolos <- UI.div #+ [element rolo1, element rolo2, element rolo3]
                           # set UI.style 
                               [ ("display", "flex")
                               , ("justify-content", "center")
                               , ("align-items", "center")
                               , ("background", "rgba(0,0,0,0.5)")
                               , ("border-radius", "10px")
                               , ("padding", "10px")
                               , ("margin", "5px 0")
                               , ("border", "2px solid #ffd700")
                               ]

            -- Informação da aposta
            infoAposta <- UI.p # set UI.text ("💎 Aposta: R$ " ++ show Logica.valorAposta)
                              # set UI.style 
                                  [ ("font-size", "0.9em")
                                  , ("color", "#ffffff")
                                  , ("margin", "5px 0")
                                  ]

            btnGirar <- UI.button #+ [string "🎰 GIRAR!"]
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

            resultado <- UI.p # set UI.text "" 
                             # set UI.style 
                                 [ ("font-size", "1em")
                                 , ("font-weight", "bold")
                                 , ("margin", "5px 0")
                                 , ("min-height", "25px")
                                 , ("color", "#ffd700")
                                 ]

            -- Container da máquina mais compacto
            maquina <- UI.div #+ [ element rolos
                                 , element infoAposta
                                 , element btnGirar
                                 , element resultado
                                 ]
                             # set UI.style
                                 [ ("background", "rgba(0,0,0,0.3)")
                                 , ("border", "3px solid #ffd700")
                                 , ("border-radius", "15px")
                                 , ("padding", "15px")
                                 , ("backdrop-filter", "blur(10px)")
                                 ]

            -- Container completo com tigrinhos
            maquinaCompleta <- UI.div #+ [element tigrinhoEsquerda, element maquina, element tigrinhoDireita]
                                     # set UI.style
                                         [ ("display", "flex")
                                         , ("align-items", "center")
                                         , ("justify-content", "center")
                                         , ("margin", "10px 0")
                                         ]

            -- Adicionar elementos à seção do jogo
            void $ element gameSection #+ [element saldoDisplay, element maquinaCompleta]

            -- Elemento áudio para música de fundo
            audioMusica <- UI.audio
              # set UI.src "static/CacaNiquel/musica.mp3"
              # set (attr "autoplay") "true"
              # set (attr "loop") "true"
              # set (attr "controls") "false"
              # set (attr "id") "audioMusica"
              # set UI.style [("display", "none")]

            -- Adicionar seções ao container principal
            void $ element contentWrapper #+ [element headerSection, element gameSection]

            -- Eventos
            void $ on UI.mousedown btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(0.95)")]
            void $ on UI.mouseup btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(1)")]

            -- Evento para mutar música
            void $ on UI.click btnMuteMusic $ \_ -> do
                isMuted <- liftIO $ readIORef musicMuted
                if not isMuted
                  then do
                    liftIO $ writeIORef musicMuted True
                    void $ element btnMuteMusic # set UI.text "🔇"
                    void $ element btnMuteMusic # set UI.style
                        [ ("background", "linear-gradient(145deg, #5a2a2a, #4a1a1a)")
                        , ("color", "#ff9090")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusica').muted = true"
                  else do
                    liftIO $ writeIORef musicMuted False
                    void $ element btnMuteMusic # set UI.text "🔊"
                    void $ element btnMuteMusic # set UI.style
                        [ ("background", "linear-gradient(145deg, #2a5a2a, #1a4a1a)")
                        , ("color", "#90ee90")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusica').muted = false"

            -- Evento voltar ao menu
            void $ on UI.click btnVoltarMenu $ \_ -> voltarAoMenu

            -- Evento principal do jogo
            void $ on UI.click btnGirar $ \_ -> do
                saldoAtual <- liftIO $ Logica.mostraSaldo playerId
                if saldoAtual < Logica.valorAposta
                  then void $ element resultado # set UI.text "💸 Saldo insuficiente!" 
                                               # set UI.style [("color", "#ff6b6b")]
                  else do
                    resultadoJogo <- liftIO $ Logica.girar
                    let vitoria = Logica.verificarVitoria resultadoJogo
                    void $ element rolo1 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 0) ++ "' style='height:50px; border-radius:6px;'/>")
                    void $ element rolo2 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 1) ++ "' style='height:50px; border-radius:6px;'/>")
                    void $ element rolo3 # set UI.html ("<img src='" ++ simboloParaGif (resultadoJogo !! 2) ++ "' style='height:50px; border-radius:6px;'/>")
                    if vitoria
                      then do
                        let simbolo = head resultadoJogo
                        let multiplicador = Logica.multiplicadores Map.! simbolo
                        let ganho = multiplicador * Logica.valorAposta
                        liftIO $ registrarJogada playerId "Caca-Niquel" (round Logica.valorAposta) ganho
                        void $ element resultado # set UI.text ("🎉 Ganhou R$ " ++ show ganho ++ "!")
                                                # set UI.style [("color", "#90ee90")]
                      else do
                        liftIO $ registrarJogada playerId "Caca-Niquel" (round Logica.valorAposta) 0
                        void $ element resultado # set UI.text "😔 Tente novamente!"
                                                # set UI.style [("color", "#ff9090")]
                    -- Atualiza saldo
                    void $ atualizarSaldo saldoDisplay playerId
                    -- Atualiza nav-bar
                    novoSaldo <- liftIO $ Logica.mostraSaldo playerId
                    void $ element pSaldo # set UI.text ("💰 $" ++ show novoSaldo)

            -- Adiciona tudo ao body
            void $ element body #+ [ element navBarElem
                                   , element contentWrapper
                                   , element audioMusica
                                   ]
            
        Nothing -> do
            -- Mensagem de erro estilizada
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