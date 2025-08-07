module Interface.CacaNiquelUI (cacaniquelUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.CacaNiquel as Logica
import qualified Data.Map as Map
import EstadoGlobal (PlayerID, registrarJogada)

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

-- | Interface do Caça-Níquel integrando com a lógica do jogo
cacaniquelUI :: Window -> UI ()
cacaniquelUI window = do
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

    -- Moldura da máquina
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

    -- Exibição dos rolos (agora usando imagens)
    rolo1 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                     # set UI.style [("margin", "0 15px")]
    rolo2 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                     # set UI.style [("margin", "0 15px")]
    rolo3 <- UI.span # set UI.html "<img src='static/CacaNiquel/interrogacao.gif' style='height:60px'/>"
                     # set UI.style [("margin", "0 15px")]
    rolos <- UI.div #+ [element rolo1, element rolo2, element rolo3]
                   # set UI.style [("display", "flex"), ("justify-content", "center"), ("margin-bottom", "25px"), ("background", "#222"), ("border-radius", "15px"), ("padding", "15px 30px"), ("box-shadow", "0 0 10px #000 inset")]

    -- Painel de resultado
    resultado <- UI.p # set UI.text "" # set UI.style [("font-size", "1.3em"), ("margin", "15px 0 0 0"), ("height", "2em"), ("text-align", "center")]

    -- Campo para PlayerID
    playerIdInput <- UI.input # set (attr "placeholder") "ID do Jogador"
                             # set UI.style [("margin-bottom", "18px"), ("font-size", "1.1em"), ("border-radius", "8px"), ("padding", "6px 12px"), ("border", "2px solid #ffd700"), ("background", "#111"), ("color", "#ffd700")]

    -- Botão de girar estilizado
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
    -- Efeito de clique no botão
    on UI.mousedown btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(0.95)")]
    on UI.mouseup btnGirar $ \_ -> element btnGirar # set UI.style [("transform", "scale(1)")]

    -- Evento do botão
    on UI.click btnGirar $ \_ -> do
        pidStr <- get value playerIdInput
        let pid = read pidStr :: PlayerID
        saldo <- liftIO $ Logica.mostraSaldo pid
        if saldo < Logica.valorAposta
          then element resultado # set UI.text "Saldo insuficiente para jogar!"
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
                element resultado # set UI.text ("🎉 Parabéns! Você ganhou " ++ show ganho ++ " com três " ++ simbolo ++ "!")
              else do
                liftIO $ registrarJogada pid "Caca-Niquel" (round Logica.valorAposta) 0
                element resultado # set UI.text "Tente novamente!"

    -- Monta a máquina
    element maquina #+ [element rolos, element btnGirar, element resultado]

    -- Adiciona tudo ao body
    element body #+ [element title, element playerIdInput, element maquina]
    return ()