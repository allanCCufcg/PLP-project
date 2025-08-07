module Interface.CacaNiquelUI (cacaniquelUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.CacaNiquel as Logica
import qualified Data.Map as Map
import EstadoGlobal (PlayerID, registrarJogada)
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

-- | Interface do Caça-Níquel integrando com a lógica do jogo
cacaniquelUI :: Window -> UI ()
cacaniquelUI window = do
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

    title <- UI.h1 #+ [string "🎰 Caça-Níquel 🎰"]
                  # set UI.style [("font-size", "2.8em"), ("margin-bottom", "10px"), ("text-shadow", "2px 2px 8px #000")]
                   
    -- Elemento áudio para música de fundo
    audioMusica <- UI.audio
      # set UI.src "static/CacaNiquel/musica.mp3"
      # set (attr "autoplay") "true"
      # set (attr "loop") "true"
      # set (attr "controls") "false"
      # set (attr "id") "audioMusica"
      # set UI.style [("display", "none")] -- Esconde o player de áudio

    -- Input do Player ID
    playerIdInput <- UI.input # set (attr "placeholder") "ID do Jogador"
                             # set UI.style [("margin-bottom", "10px"), ("font-size", "1.1em"), ("border-radius", "8px"), ("padding", "6px 12px"), ("border", "2px solid #ffd700"), ("background", "#111"), ("color", "#ffd700")]

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

    -- Display do saldo do jogador
    saldoDisplay <- UI.p # set UI.text "💰 Saldo: R$ 0.0"
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

    -- Botão para atualizar/mostrar saldo
    btnMostrarSaldo <- UI.button #+ [string "Mostrar Saldo"]
                                # set UI.style
                                    [ ("font-size", "0.9em")
                                    , ("padding", "5px 15px")
                                    , ("background", "linear-gradient(90deg, #444, #333)")
                                    , ("color", "#ffd700")
                                    , ("border", "1px solid #666")
                                    , ("border-radius", "6px")
                                    , ("cursor", "pointer")
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

    -- Evento para mostrar saldo
    on UI.click btnMostrarSaldo $ \_ -> do
        pidStr <- get value playerIdInput
        if null pidStr
          then element resultado # set UI.text "Por favor, insira um ID de jogador!" >> return ()
          else do
            let pid = read pidStr :: PlayerID
            atualizarSaldo saldoDisplay pid >> return ()

    -- Evento principal do jogo
    on UI.click btnGirar $ \_ -> do
        pidStr <- get value playerIdInput
        if null pidStr
          then element resultado # set UI.text "Por favor, insira um ID de jogador!" >> return ()
          else do
            let pid = read pidStr :: PlayerID
            saldo <- liftIO $ Logica.mostraSaldo pid
            if saldo < Logica.valorAposta
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
                    liftIO $ registrarJogada pid "Caca-Niquel" (round Logica.valorAposta) ganho
                    element resultado # set UI.text ("🎉 Parabéns! Você ganhou R$ " ++ show ganho ++ " com três " ++ simbolo ++ "!")
                  else do
                    liftIO $ registrarJogada pid "Caca-Niquel" (round Logica.valorAposta) 0
                    element resultado # set UI.text "Tente novamente!"
                -- Atualiza o saldo após cada jogada
                atualizarSaldo saldoDisplay pid >> return ()

    element maquina #+ [element rolos, element infoAposta, element btnGirar, element resultado]

    element maquinaCompleta #+ [element tigrinhoEsquerda, element maquina, element tigrinhoDireita]

    element controlesContainer #+ [element btnMuteMusic, element btnMostrarSaldo]

    element body #+ [element title, element playerIdInput, element controlesContainer, element saldoDisplay, element maquinaCompleta, element audioMusica]

    return ()