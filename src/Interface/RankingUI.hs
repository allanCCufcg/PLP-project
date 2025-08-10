-- File: src/Interface/RankingUI.hs

module Interface.RankingUI (rankingUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (getGlobalData, Jogador(..), DadosGlobais(..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy, take)
import Data.Ord (comparing)
import Text.Printf (printf)

-- | Função principal para renderizar a interface do ranking
rankingUI :: Window -> Int -> UI () -> UI ()
rankingUI window jogadorId voltarMenu = do
    -- Limpa a tela antes de renderizar o novo conteúdo
    body <- getBody window
    void $ element body # set UI.children []

    -- Estilo de fundo responsivo
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("min-height", "100vh")
        , ("margin", "0")
        , ("padding", "10px")
        , ("box-sizing", "border-box")
        , ("overflow-x", "auto")
        ]

    -- Busca os dados globais
    dados <- liftIO getGlobalData
    
    -- Ordena jogadores por total ganho (decrescente) e pega os 10 primeiros
    let ranking = take 10 $ sortBy (comparing (negate . totalGanho)) (jogadores dados)
    
    -- Container principal sem forçar scroll
    containerPrincipal <- UI.div # set UI.style 
        [ ("max-width", "1200px")
        , ("margin", "0 auto")
        , ("width", "100%")
        , ("box-sizing", "border-box")
        ]

    -- Cabeçalho com título
    titulo <- UI.h1 #+ [string "🏆 RANKING DOS CAMPEÕES 🏆"]
                   # set UI.style 
                       [ ("color", "#ffd700")
                       , ("font-size", "clamp(1.5rem, 4vw, 2.5rem)")
                       , ("margin", "10px 0 20px 0")
                       , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                       , ("text-align", "center")
                       ]

    -- Botão de voltar - fixo no topo
    btnVoltar <- UI.button #+ [string "← Voltar ao Menu"]
                          # set UI.style 
                              [ ("background", "linear-gradient(145deg, #4a4a6a, #3a3a5a)")
                              , ("color", "#ffffff")
                              , ("border", "2px solid #ffd700")
                              , ("border-radius", "8px")
                              , ("padding", "10px 20px")
                              , ("cursor", "pointer")
                              , ("font-size", "clamp(14px, 2vw, 16px)")
                              , ("margin", "0 0 20px 0")
                              , ("box-shadow", "0 2px 4px rgba(0,0,0,0.3)")
                              , ("width", "fit-content")
                              , ("min-width", "150px")
                              ]

    void $ on UI.click btnVoltar $ \_ -> voltarMenu

    -- Container do ranking sem scroll forçado
    containerRanking <- UI.div # set UI.style 
                                   [ ("background", "rgba(0,0,0,0.3)")
                                   , ("border-radius", "15px")
                                   , ("padding", "15px")
                                   , ("border", "2px solid #ffd700")
                                   , ("width", "100%")
                                   , ("box-sizing", "border-box")
                                   , ("overflow-x", "auto")
                                   ]

    -- Função para criar uma linha do ranking
    let criarLinhaRanking posicao jogador = do
            let medalha = case posicao of
                    1 -> "🥇"
                    2 -> "🥈" 
                    3 -> "🥉"
                    _ -> "🏅"
            
            let isJogadorAtual = playerID jogador == jogadorId
                corLinha = if isJogadorAtual then "rgba(255, 215, 0, 0.2)" else "rgba(255,255,255,0.05)"
                corBorda = if isJogadorAtual then "#ffd700" else "transparent"
            
            -- Posição e medalha
            colPosicao <- UI.div #+ [string $ medalha ++ " " ++ show posicao]
                                # set UI.style 
                                    [ ("font-size", "clamp(14px, 2.5vw, 18px)")
                                    , ("font-weight", "bold")
                                    , ("width", "10%")
                                    , ("min-width", "50px")
                                    , ("text-align", "center")
                                    , ("flex-shrink", "1")
                                    ]

            -- Nome do jogador
            let nomeDisplay = nome jogador ++ if isJogadorAtual then " (VOCÊ)" else ""
            colNome <- UI.div #+ [string nomeDisplay]
                             # set UI.style 
                                 [ ("font-size", "clamp(14px, 2.5vw, 16px)")
                                 , ("font-weight", if isJogadorAtual then "bold" else "normal")
                                 , ("color", if isJogadorAtual then "#ffd700" else "#ffffff")
                                 , ("flex", "1")
                                 , ("text-align", "left")
                                 , ("padding", "0 5px")
                                 , ("min-width", "80px")
                                 , ("overflow", "hidden")
                                 , ("text-overflow", "ellipsis")
                                 , ("white-space", "nowrap")
                                 ]

            -- ID do jogador
            colID <- UI.div #+ [string $ "#" ++ show (playerID jogador)]
                           # set UI.style 
                               [ ("font-size", "clamp(11px, 1.8vw, 13px)")
                               , ("color", "#cccccc")
                               , ("width", "8%")
                               , ("min-width", "35px")
                               , ("text-align", "center")
                               , ("flex-shrink", "1")
                               ]

            -- Total ganho
            let ganhoFormatado = printf "%.0f" (totalGanho jogador)
            colGanho <- UI.div #+ [string $ "R$" ++ ganhoFormatado]
                              # set UI.style 
                                  [ ("font-size", "clamp(12px, 2.2vw, 16px)")
                                  , ("font-weight", "bold")
                                  , ("color", "#90EE90")
                                  , ("width", "20%")
                                  , ("min-width", "60px")
                                  , ("text-align", "right")
                                  , ("flex-shrink", "1")
                                  ]

            -- Total de apostas
            colApostas <- UI.div #+ [string $ show (totalApostas jogador)]
                                # set UI.style 
                                    [ ("font-size", "clamp(11px, 1.8vw, 13px)")
                                    , ("color", "#cccccc")
                                    , ("width", "8%")
                                    , ("min-width", "35px")
                                    , ("text-align", "center")
                                    , ("flex-shrink", "1")
                                    ]

            -- Saldo atual
            let saldoFormatado = printf "%.0f" (saldo jogador)
            colSaldo <- UI.div #+ [string $ "R$" ++ saldoFormatado]
                              # set UI.style 
                                  [ ("font-size", "clamp(11px, 2vw, 14px)")
                                  , ("color", "#87CEEB")
                                  , ("width", "18%")
                                  , ("min-width", "55px")
                                  , ("text-align", "right")
                                  , ("flex-shrink", "1")
                                  ]

            -- Linha completa
            linha <- UI.div #+ [ element colPosicao
                               , element colNome
                               , element colID
                               , element colGanho
                               , element colApostas
                               , element colSaldo
                               ]
                           # set UI.style 
                               [ ("display", "flex")
                               , ("align-items", "center")
                               , ("padding", "8px 10px")
                               , ("margin", "3px 0")
                               , ("background", corLinha)
                               , ("border", "2px solid " ++ corBorda)
                               , ("border-radius", "8px")
                               , ("transition", "all 0.3s ease")
                               , ("width", "100%")
                               , ("box-sizing", "border-box")
                               ]

            return linha

    -- Cabeçalho da tabela
    headerTabela <- UI.div #+ [
        UI.div #+ [string "POS"] # set UI.style [("width", "10%"), ("min-width", "50px"), ("text-align", "center"), ("font-weight", "bold"), ("flex-shrink", "1")],
        UI.div #+ [string "JOGADOR"] # set UI.style [("flex", "1"), ("text-align", "left"), ("padding", "0 5px"), ("font-weight", "bold"), ("min-width", "80px")],
        UI.div #+ [string "ID"] # set UI.style [("width", "8%"), ("min-width", "35px"), ("text-align", "center"), ("font-weight", "bold"), ("flex-shrink", "1")],
        UI.div #+ [string "GANHO"] # set UI.style [("width", "20%"), ("min-width", "60px"), ("text-align", "right"), ("font-weight", "bold"), ("flex-shrink", "1")],
        UI.div #+ [string "JOGS"] # set UI.style [("width", "8%"), ("min-width", "35px"), ("text-align", "center"), ("font-weight", "bold"), ("flex-shrink", "1")],
        UI.div #+ [string "SALDO"] # set UI.style [("width", "18%"), ("min-width", "55px"), ("text-align", "right"), ("font-weight", "bold"), ("flex-shrink", "1")]
        ] # set UI.style 
        [ ("display", "flex")
        , ("align-items", "center")
        , ("padding", "8px 10px")
        , ("background", "rgba(255, 215, 0, 0.2)")
        , ("border-radius", "8px")
        , ("margin-bottom", "15px")
        , ("color", "#ffd700")
        , ("border", "2px solid #ffd700")
        , ("width", "100%")
        , ("box-sizing", "border-box")
        , ("font-size", "clamp(11px, 1.8vw, 13px)")
        ]

    -- Cria as linhas do ranking
    linhasRanking <- mapM (uncurry criarLinhaRanking) (zip [1..] ranking)

    -- Mensagem se não houver jogadores suficientes
    conteudoRanking <- if null ranking
        then do
            msgVazio <- UI.div #+ [string "🎮 Ainda não há jogadores no ranking. Seja o primeiro!"]
                              # set UI.style 
                                  [ ("text-align", "center")
                                  , ("font-size", "clamp(16px, 3vw, 18px)")
                                  , ("color", "#cccccc")
                                  , ("padding", "40px 20px")
                                  , ("font-style", "italic")
                                  ]
            return [msgVazio]
        else return (headerTabela : linhasRanking)

    -- Adiciona tudo ao container
    void $ element containerRanking #+ (map element conteudoRanking)

    -- Adiciona tudo ao container principal
    void $ element containerPrincipal #+ [ element titulo
                                         , element btnVoltar
                                         , element containerRanking
                                         ]

    -- Adiciona o container principal ao body
    void $ element body #+ [element containerPrincipal]

    -- Dica de scroll horizontal apenas para telas muito pequenas
    dicaScroll <- UI.div #+ [string "💡 Em telas pequenas, deslize horizontalmente"]
                        # set UI.style 
                            [ ("text-align", "center")
                            , ("font-size", "11px")
                            , ("color", "#666")
                            , ("margin-top", "10px")
                            , ("font-style", "italic")
                            , ("display", "none")
                            ]

    void $ element body #+ [element dicaScroll]