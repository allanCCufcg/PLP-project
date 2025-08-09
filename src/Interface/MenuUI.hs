-- File: src/Interface/MenuUI.hs

module Interface.MenuUI (menuUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Interface.CacaNiquelUI (cacaniquelUI)
import Interface.BlackjackUI (blackjackUI)
import EstadoGlobal (buscarJogadorPorID, Jogador(..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

-- | A função principal para renderizar a interface do menu.
-- Agora aceita o ID do jogador.
menuUI :: Window -> Int -> UI ()
menuUI window jogadorId = do
    -- Limpa a tela antes de renderizar o novo conteúdo
    body <- getBody window
    void $ element body # set UI.children []

    -- Estilo de fundo simples e bonito
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "20px")
        , ("text-align", "center")
        ]
    
    -- Busca os dados do jogador
    maybeJogador <- liftIO $ buscarJogadorPorID jogadorId
    case maybeJogador of
        Just jogador -> do
            -- Barra superior com informações do jogador
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

            -- Título principal
            titulo <- UI.h1 #+ [string "🎰 EL CASSINO 🎰"]
                           # set UI.style 
                               [ ("color", "#ffd700")
                               , ("font-size", "3em")
                               , ("margin", "30px 0")
                               , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                               ]

            -- Função para criar cards dos jogos
            let criarCard nome imagemSrc acao = do
                    -- Imagem do jogo
                    imagem <- UI.img # set UI.src imagemSrc
                                    # set UI.style 
                                        [ ("width", "160px")
                                        , ("height", "160px")
                                        , ("object-fit", "cover")
                                        , ("border-radius", "10px")
                                        ]
                    
                    -- Card principal (só a imagem)
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
                    
                    -- Texto abaixo do card
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
                    
                    -- Container do card + texto
                    container <- UI.div #+ [element card, element texto]
                                       # set UI.style [("display", "flex"), ("flex-direction", "column"), ("align-items", "center")]
                    
                    void $ on UI.click card acao
                    return container

            -- Criação dos cards dos jogos
            cardBaccarat <- criarCard "BACCARAT" "static/baccarat.png" $ \_ -> liftIO $ putStrLn "Ir para Baccarat"
            cardBlackjack <- criarCard "BLACKJACK" "static/blackjack.png" $ \_ -> do
                void $ element body # set UI.children []
                blackjackUI window
            cardCacaniquel <- criarCard "CAÇA-NÍQUEL" "static/cacaniquel.png" $ \_ -> do
                void $ element body # set UI.children []
                cacaniquelUI window jogadorId (menuUI window jogadorId)
            cardCaixaSurpresa <- criarCard "CAIXA SURPRESA" "static/caixasurpresa.png" $ \_ -> liftIO $ putStrLn "Ir para Caixa Surpresa"
            cardRoleta <- criarCard "ROLETA" "static/roleta.png" $ \_ -> liftIO $ putStrLn "Ir para Roleta"

            -- Container dos jogos (todos na mesma linha)
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

            -- Adiciona tudo ao body
            void $ element body #+ [ element navBar
                                   , element titulo
                                   , element containerJogos
                                   ]
            
        Nothing -> do
            -- Mensagem de erro simples e bonita
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