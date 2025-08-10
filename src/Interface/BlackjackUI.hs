module Interface.BlackjackUI (blackjackUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.Blackjack as Logica
import Data.IORef
import EstadoGlobal (PlayerID, buscarJogadorPorID, Jogador(..), adicionarSaldo, removerSaldo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)

naipes :: [String]
naipes = ["♠", "♥", "♦", "♣"]

cartaVisual :: Logica.Carta -> UI Element
cartaVisual carta = do
    let v = Logica.valor carta
        (valorStr, idx) = case v of
            Logica.As         -> ("A", 0)
            Logica.Numero n   -> (show n, n `mod` 4)
            Logica.Valete     -> ("J", 1)
            Logica.Dama       -> ("Q", 2)
            Logica.Rei        -> ("K", 3)
        naipes = ["♠", "♥", "♦", "♣"]
        naipe = naipes !! idx
        cor = if naipe == "♥" || naipe == "♦" then "#e53935" else "#222"
    UI.div # set UI.style
        [ ("display", "inline-block")
        , ("width", "40px")
        , ("height", "60px")
        , ("margin", "2px")
        , ("background", "white")
        , ("border", "1.5px solid #333")
        , ("border-radius", "6px")
        , ("box-shadow", "1px 1px 4px #0003")
        , ("font-size", "1.1em")
        , ("color", cor)
        , ("position", "relative")
        ]
        #+ [ UI.span # set UI.text naipe
                     # set UI.style [ ("position","absolute")
                                   , ("top","2px")
                                   , ("left","4px")
                                   , ("font-size","1.1em")
                                   , ("font-weight","bold")
                                   ]
           , UI.span # set UI.text valorStr
                     # set UI.style [ ("position","absolute")
                                   , ("top","50%")
                                   , ("left","50%")
                                   , ("transform","translate(-50%,-50%)")
                                   , ("font-size","1.5em")
                                   , ("font-weight","bold")
                                   ]
           ]

cartasVisuais :: [Logica.Carta] -> UI Element
cartasVisuais cs = UI.div #+ map cartaVisual cs
    # set UI.style [("display", "flex"), ("justify-content", "center")]

data EstadoBJ = EstadoBJ
    { playerId   :: PlayerID
    , maoJogador :: [Logica.Carta]
    , maoBanca   :: [Logica.Carta]
    , baralho    :: [Logica.Carta]
    , status     :: String
    , fim        :: Bool
    }

novaPartida :: PlayerID -> IO EstadoBJ
novaPartida pid = do
    let bar = Logica.baralho
    (j1, bar1) <- Logica.sortCarta bar
    (b1, bar2) <- Logica.sortCarta bar1
    (j2, bar3) <- Logica.sortCarta bar2
    (b2, bar4) <- Logica.sortCarta bar3
    let mj = [j1, j2]
    let mb = [b1, b2]
    let pontBanca = Logica.pontuacao mb
    let st
          | pontBanca == 21 = "O Oponente Venceu"
          | otherwise       = ""
    let terminou = pontBanca == 21
    when terminou $ do
        removerSaldo pid 10 -- banca já ganha e jogador perde 10
    return $ EstadoBJ pid mj mb bar4 st terminou

-- Atualiza saldo com base no resultado fixo
atualizarSaldoPorResultado :: PlayerID -> String -> IO ()
atualizarSaldoPorResultado pid resultado = do
    case resultado of
      "Você Venceu"       -> adicionarSaldo pid 10
      "O Oponente Venceu" -> removerSaldo pid 10
      _                   -> return ()

pedirCarta :: PlayerID -> EstadoBJ -> IO EstadoBJ
pedirCarta pid est
    | fim est = return est
    | otherwise = do
        maybeJogador <- buscarJogadorPorID pid
        case maybeJogador of
          Nothing -> return est
          Just jog ->
            if saldo jog <= 0 then return est else do
              (nova, bar1) <- Logica.sortCarta (baralho est)
              let mj = maoJogador est ++ [nova]
              let somaJ = Logica.pontuacao mj
              if somaJ > 21
                then do
                  (mb, bar2) <- loopBanca (maoBanca est) bar1
                  let somaB = Logica.pontuacao mb
                  let resultado
                        | somaJ > 21 && somaB > 21 =
                            if somaJ == somaB then "Empate"
                            else if somaJ < somaB then "Você Venceu"
                            else "O Oponente Venceu"
                        | somaB > 21 = "Você Venceu"
                        | otherwise = "O Oponente Venceu"

                  atualizarSaldoPorResultado pid resultado

                  return est { playerId = pid, maoJogador = mj, maoBanca = mb, baralho = bar2, status = resultado, fim = True }
                else return est { playerId = pid, maoJogador = mj, baralho = bar1 }

parar :: PlayerID -> EstadoBJ -> IO EstadoBJ
parar pid est
    | fim est = return est
    | otherwise = do
        maybeJogador <- buscarJogadorPorID pid
        case maybeJogador of
          Nothing -> return est
          Just jog ->
            if saldo jog <= 0 then return est else do
              (mb, bar1) <- loopBanca (maoBanca est) (baralho est)
              let mj = maoJogador est
              let somaJ = Logica.pontuacao mj
              let somaB = Logica.pontuacao mb
              let resultado
                    | somaJ > 21 && somaB > 21 =
                        if somaJ == somaB then "Empate"
                        else if somaJ < somaB then "Você Venceu"
                        else "O Oponente Venceu"
                    | somaJ > 21 = "O Oponente Venceu"
                    | somaB > 21 = "Você Venceu"
                    | somaJ > somaB = "Você Venceu"
                    | somaB > somaJ = "O Oponente Venceu"
                    | otherwise = "Empate"

              atualizarSaldoPorResultado pid resultado

              return est { playerId = pid, maoBanca = mb, baralho = bar1, status = resultado, fim = True }

loopBanca :: [Logica.Carta] -> [Logica.Carta] -> IO ([Logica.Carta], [Logica.Carta])
loopBanca pb baralho = do
    let somaB = Logica.pontuacao pb
    if somaB < 17
        then do
            (novaBan, baralho1) <- Logica.sortCarta baralho
            loopBanca (pb ++ [novaBan]) baralho1
        else return (pb, baralho)

blackjackUI :: Window -> PlayerID -> UI () -> UI ()
blackjackUI window playerId voltarAoMenu = do
    body <- getBody window
    void $ element body # set UI.children []

    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg,#0b3d0b 60%,#1b5e20 100%)")
        , ("color", "white")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("overflow", "hidden")
        ]

    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
      Just jogador -> do
        let nomeStr = "👤 " ++ nome jogador
            idStr = "🎫 ID: " ++ show (playerID jogador)
            saldoStr = "💰 Saldo: R$ " ++ show (saldo jogador)

        pNome <- UI.span # set UI.text nomeStr
                         # set UI.style [("color", "#ffd700"), ("margin-right", "15px"), ("font-weight", "bold")]
        pId   <- UI.span # set UI.text idStr
                         # set UI.style [("color", "#ffd700"), ("margin-right", "15px"), ("font-weight", "bold")]
        pSaldo <- UI.span # set UI.text saldoStr
                          # set UI.style [("color", "#ffd700"), ("font-weight", "bold")]

        btnVoltarMenu <- UI.button #+ [string "🏠 MENU"]
            # set UI.style
                [ ("font-size", "1.1em")
                , ("padding", "6px 14px")
                , ("background", "linear-gradient(90deg, #185a18 60%, #0e2e0e 100%)")
                , ("color", "#c2ffc2")
                , ("border", "none")
                , ("border-radius", "8px")
                , ("box-shadow", "0 2px 8px #000")
                , ("cursor", "pointer")
                , ("font-weight", "bold")
                ]

        navBarElem <- UI.div #+ [UI.div #+ [element pNome, element pId, element pSaldo] # set UI.style [("display","flex"),("gap","10px"),("align-items","center")], element btnVoltarMenu]
                            # set UI.style
                              [ ("background", "rgba(0,0,0,0.4)")
                              , ("padding", "10px 30px")
                              , ("width", "100%")
                              , ("height", "40px")
                              , ("display", "flex")
                              , ("justify-content", "space-between")
                              , ("align-items", "center")
                              , ("position", "fixed")
                              , ("top", "0")
                              , ("left", "0")
                              , ("z-index", "1000")
                              , ("box-shadow", "0 2px 10px rgba(0,0,0,0.7)")
                              , ("font-size", "1em")
                              , ("border-bottom", "2px solid #ffd700")
                              , ("overflow", "hidden")
                              , ("box-sizing", "border-box")
                              ]

        contentWrapper <- UI.div # set UI.style
          [ ("padding-top", "60px")
          , ("height", "calc(100vh - 60px)")
          , ("display", "flex")
          , ("flex-direction", "column")
          , ("align-items", "center")
          , ("justify-content", "flex-start")
          , ("text-align", "center")
          , ("overflow", "auto")
          ]

        title <- UI.h1 #+ [string "♠️ Blackjack ♥️"]
                      # set UI.style
                          [ ("font-size", "2.2em")
                          , ("margin-bottom", "10px")
                          , ("color", "#ffd700")
                          , ("text-shadow", "1px 1px 3px #000")
                          ]

        resultado <- UI.h3 # set UI.style [("margin-bottom", "18px"), ("min-height", "28px")]

        mesa <- UI.div # set UI.style
            [ ("background", "#145a32")
            , ("border-radius", "16px")
            , ("padding", "18px 20px")
            , ("box-shadow", "0 0 18px #0008")
            , ("margin-bottom", "20px")
            , ("min-width", "260px")
            ]

        jogadorLabel <- UI.h2 #+ [string "Suas cartas"]
                             # set UI.style [("font-size","1.1em"),("margin-bottom","2px")]
        bancaLabel   <- UI.h2 #+ [string "Cartas visíveis da banca"]
                             # set UI.style [("font-size","1.1em"),("margin-bottom","2px")]

        jogadorCartasDiv <- UI.div
        bancaCartasDiv   <- UI.div

        jogadorPontuacao <- UI.p # set UI.style [("font-size", "1em"), ("margin", "6px 0")]
        bancaPontuacao   <- UI.p # set UI.style [("font-size", "1em"), ("margin", "6px 0")]

        let botaoVerde txt = UI.button #+ [string txt]
                # set UI.style
                    [ ("font-size", "1.1em")
                    , ("padding", "8px 22px")
                    , ("background", "linear-gradient(90deg, #185a18 60%, #0e2e0e 100%)")
                    , ("color", "#c2ffc2")
                    , ("border", "none")
                    , ("border-radius", "8px")
                    , ("box-shadow", "0 2px 8px #000")
                    , ("cursor", "pointer")
                    , ("font-weight", "bold")
                    , ("transition", "filter 0.2s, opacity 0.2s")
                    , ("opacity", "1")
                    ]

        btnNovaPartida <- botaoVerde "Nova Partida"
        btnPedirCarta  <- botaoVerde "Pedir Carta"
        btnParar       <- botaoVerde "Parar"

        on UI.click btnVoltarMenu $ \_ -> voltarAoMenu

        botoesContainer <- UI.div #+ [element btnNovaPartida, element btnPedirCarta, element btnParar]
            # set UI.style [("display", "flex"), ("gap", "10px"), ("margin-top", "10px")]

        estadoRef <- liftIO $ novaPartida playerId >>= newIORef

        let setBtnEnabled btn enabled = do
                element btn # set UI.enabled enabled
                element btn # set UI.style
                    [ ("font-size", "1.1em")
                    , ("padding", "8px 22px")
                    , ("background", "linear-gradient(90deg, #185a18 60%, #0e2e0e 100%)")
                    , ("color", if enabled then "#c2ffc2" else "#888")
                    , ("border", "none")
                    , ("border-radius", "8px")
                    , ("box-shadow", "0 2px 8px #000")
                    , ("cursor", if enabled then "pointer" else "not-allowed")
                    , ("font-weight", "bold")
                    , ("transition", "filter 0.2s, opacity 0.2s")
                    , ("opacity", if enabled then "1" else "0.6")
                    ]

        let atualizarUI :: UI ()
            atualizarUI = do
                estado <- liftIO $ readIORef estadoRef
                jogadorCartasElem <- cartasVisuais (maoJogador estado)
                element jogadorCartasDiv # set children [jogadorCartasElem]
                if fim estado
                  then do
                    bancaCartasElem <- cartasVisuais (maoBanca estado)
                    element bancaCartasDiv # set children [bancaCartasElem]
                    element bancaPontuacao # set UI.text ("Total Banca: " ++ show (Logica.pontuacao (maoBanca estado)))
                  else do
                    let cartasBanca = case maoBanca estado of
                                        []    -> []
                                        (x:_) -> [x]
                    bancaCartasElem <- cartasVisuais cartasBanca
                    element bancaCartasDiv # set children [bancaCartasElem]
                    element bancaPontuacao # set UI.text "Total Banca: ?"
                element jogadorPontuacao # set UI.text ("Total Jogador: " ++ show (Logica.pontuacao (maoJogador estado)))
                if status estado /= ""
                    then element resultado # set UI.text (status estado)
                    else element resultado # set UI.text ""
                let fimjogo = fim estado
                setBtnEnabled btnPedirCarta (not fimjogo)
                setBtnEnabled btnParar      (not fimjogo)
                setBtnEnabled btnNovaPartida fimjogo
                maybeJogador' <- liftIO $ buscarJogadorPorID playerId
                case maybeJogador' of
                  Just jog' -> do
                    let novoSaldoStr = "💰 Saldo: R$ " ++ show (saldo jog')
                    element pSaldo # set UI.text novoSaldoStr
                    return ()
                  Nothing -> return ()

        on UI.click btnNovaPartida $ \_ -> do
            novo <- liftIO $ novaPartida playerId
            liftIO $ writeIORef estadoRef novo
            atualizarUI

        on UI.click btnPedirCarta $ \_ -> do
            estado <- liftIO $ readIORef estadoRef
            novo <- liftIO $ pedirCarta playerId estado
            liftIO $ writeIORef estadoRef novo
            atualizarUI

        on UI.click btnParar $ \_ -> do
            estado <- liftIO $ readIORef estadoRef
            novo <- liftIO $ parar playerId estado
            liftIO $ writeIORef estadoRef novo
            atualizarUI

        element mesa #+
            [ element bancaLabel
            , element bancaCartasDiv
            , element bancaPontuacao
            , UI.hr
            , element jogadorLabel
            , element jogadorCartasDiv
            , element jogadorPontuacao
            , element botoesContainer
            ]

        void $ element contentWrapper #+ [element title, element mesa, element resultado]

        void $ element body #+ [element navBarElem, element contentWrapper]

        atualizarUI

      Nothing -> do
        erro <- UI.div # set UI.text "Jogador não encontrado."
                      # set UI.style [("color", "red"), ("font-weight", "bold"), ("font-size", "1.2em")]
        void $ element body #+ [element erro]
