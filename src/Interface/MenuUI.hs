-- File: src/Interface/MenuUI.hs

module Interface.MenuUI (menuUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Interface.CacaNiquelUI (cacaniquelUI)
import Interface.BlackjackUI (blackjackUI)
import Interface.BaccaratUI (baccaratUI)
import Interface.RankingUI (rankingUI)
import Interface.CaixaSurpresaUI (caixaSurpresaUI)
import Interface.RoletaUI (roletaUI)  -- NOVO IMPORT
import EstadoGlobal (buscarJogadorPorID, Jogador(..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

menuUI :: Window -> Int -> UI ()
menuUI window jogadorId = do
    body <- getBody window
    void $ element body # set UI.children []

    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "20px")
        , ("text-align", "center")
        ]
    
    maybeJogador <- liftIO $ buscarJogadorPorID jogadorId
    case maybeJogador of
        Just jogador -> do
            let nomeStr = "👤 " ++ nome jogador
                idStr = "🎫 ID: " ++ show (playerID jogador)
                saldoStr = "💰 $" ++ show (saldo jogador)
            
            navBar <- UI.div #+ [ UI.span #+ [string nomeStr] # set UI.style [("margin", "0 20px"), ("color", "#ffd700")]
                                , UI.span #+ [string idStr] # set UI.style [("margin", "0 20px"), ("color", "#ffd700")]
                                , UI.span #+ [string saldoStr] # set UI.style [("margin", "0 20px"), ("color", "#ffd700")]
                                ]
                            # set UI.style 
                                [ ("background", "rgba(0,0,0,0.3)")
                                , ("padding", "15px")
                                , ("border-radius", "10px")
                                , ("margin-bottom", "30px")
                                , ("border", "2px solid #ffd700")
                                ]

            titulo <- UI.h1 #+ [string "🎰 EL CASSINO 🎰"]
                           # set UI.style 
                               [ ("color", "#ffd700")
                               , ("font-size", "3em")
                               , ("margin", "30px 0")
                               , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                               ]

            let criarCard nome imagemSrc acao = do
                    imagem <- UI.img # set UI.src imagemSrc
                                    # set UI.style 
                                        [ ("width", "160px")
                                        , ("height", "160px")
                                        , ("object-fit", "cover")
                                        , ("border-radius", "10px")
                                        ]
                    
                    card <- UI.div #+ [element imagem]
                                  # set UI.style 
                                      [ ("background", "linear-gradient(145deg, #2a2a5a, #1a1a3a)")
                                      , ("border", "2px solid #ffd700")
                                      , ("border-radius", "15px")
                                      , ("padding", "20px")
                                      , ("cursor", "pointer")
                                      , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                      , ("width", "160px")
                                      , ("height", "160px")
                                      , ("display", "flex")
                                      , ("align-items", "center")
                                      , ("justify-content", "center")
                                      ]
                    
                    texto <- UI.div #+ [string nome]
                                   # set UI.style 
                                       [ ("font-size", "14px")
                                       , ("font-weight", "300")
                                       , ("color", "#ffffff")
                                       , ("margin-top", "15px")
                                       , ("text-align", "center")
                                       , ("font-family", "Arial, sans-serif")
                                       , ("letter-spacing", "1px")
                                       ]
                    
                    container <- UI.div #+ [element card, element texto]
                                       # set UI.style [("display", "flex"), ("flex-direction", "column"), ("align-items", "center")]
                    
                    void $ on UI.click card acao
                    return container

            cardBaccarat <- criarCard "BACCARAT" "static/baccarat.png" $ \_ -> do
                void $ element body # set UI.children []
                baccaratUI window jogadorId (menuUI window jogadorId)
            
            cardBlackjack <- criarCard "BLACKJACK" "static/blackjack.png" $ \_ -> do
                void $ element body # set UI.children []
                blackjackUI window jogadorId (menuUI window jogadorId)

            cardCacaniquel <- criarCard "CAÇA-NÍQUEL" "static/cacaniquel.png" $ \_ -> do
                void $ element body # set UI.children []
                cacaniquelUI window jogadorId (menuUI window jogadorId)

            cardCaixaSurpresa <- criarCard "CAIXA SURPRESA" "static/caixasurpresa.png" $ \_ -> do
                void $ element body # set UI.children []
                caixaSurpresaUI window jogadorId (menuUI window jogadorId)

            -- ALTERAÇÃO AQUI: Agora abre a roletaUI ao invés de só imprimir
            cardRoleta <- criarCard "ROLETA" "static/roleta.png" $ \_ -> do
                void $ element body # set UI.children []
                roletaUI window jogadorId (menuUI window jogadorId)

            containerJogos <- UI.div #+ [ element cardBaccarat
                                        , element cardBlackjack
                                        , element cardCacaniquel
                                        , element cardCaixaSurpresa
                                        , element cardRoleta
                                        ]
                                    # set UI.style 
                                        [ ("display", "flex")
                                        , ("justify-content", "center")
                                        , ("align-items", "flex-start")
                                        , ("gap", "30px")
                                        , ("margin", "30px auto")
                                        , ("flex-wrap", "nowrap")
                                        ]

            btnRanking <- UI.button #+ [string "🏆 RANKING"]
                                  # set UI.style 
                                      [ ("background", "linear-gradient(145deg, #ffd700, #ffb700)")
                                      , ("color", "#1a1a2e")
                                      , ("border", "none")
                                      , ("border-radius", "25px")
                                      , ("padding", "15px 40px")
                                      , ("cursor", "pointer")
                                      , ("font-size", "18px")
                                      , ("font-weight", "bold")
                                      , ("margin", "30px auto")
                                      , ("box-shadow", "0 6px 12px rgba(255, 215, 0, 0.3)")
                                      , ("transition", "all 0.3s ease")
                                      , ("text-shadow", "1px 1px 2px rgba(0,0,0,0.2)")
                                      ]

            void $ on UI.click btnRanking $ \_ -> do
                void $ element body # set UI.children []
                rankingUI window jogadorId (menuUI window jogadorId)

            containerBotao <- UI.div #+ [element btnRanking]
                                    # set UI.style 
                                        [ ("display", "flex")
                                        , ("justify-content", "center")
                                        , ("margin", "20px 0")
                                        ]

            void $ element body #+ [ element navBar
                                   , element titulo
                                   , element containerJogos
                                   , element containerBotao
                                   ]
                                   