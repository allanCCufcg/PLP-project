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

    -- Estilo de fundo igual ao menu
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "20px")
        , ("overflow-y", "auto")
        ]

    -- Busca os dados globais
    dados <- liftIO getGlobalData
    
    -- Ordena jogadores por total ganho (decrescente) e pega os 10 primeiros
    let ranking = take 10 $ sortBy (comparing (negate . totalGanho)) (jogadores dados)
    
    -- Cabeçalho com título
    titulo <- UI.h1 #+ [string "🏆 RANKING DOS CAMPEÕES 🏆"]
                   # set UI.style 
                       [ ("color", "#ffd700")
                       , ("font-size", "2.5em")
                       , ("margin", "20px 0")
                       , ("text-shadow", "2px 2px 4px rgba(0,0,0,0.5)")
                       , ("text-align", "center")
                       ]

    -- Botão de voltar
    btnVoltar <- UI.button #+ [string "← Voltar ao Menu"]
                          # set UI.style 
                              [ ("background", "linear-gradient(145deg, #4a4a6a, #3a3a5a)")
                              , ("color", "#ffffff")
                              , ("border", "2px solid #ffd700")
                              , ("border-radius", "8px")
                              , ("padding", "12px 25px")
                              , ("cursor", "pointer")
                              , ("font-size", "16px")
                              , ("margin", "0 0 30px 0")
                              , ("box-shadow", "0 2px 4px rgba(0,0,0,0.3)")
                              ]

    void $ on UI.click btnVoltar $ \_ -> voltarMenu

    -- Container principal do ranking
    containerRanking <- UI.div # set UI.style 
                                   [ ("max-width", "800px")
                                   , ("margin", "0 auto")
                                   , ("background", "rgba(0,0,0,0.3)")
                                   , ("border-radius", "15px")
                                   , ("padding", "30px")
                                   , ("border", "2px solid #ffd700")
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
                                    [ ("font-size", "20px")
                                    , ("font-weight", "bold")
                                    , ("width", "80px")
                                    , ("text-align", "center")
                                    ]

            -- Nome do jogador
            let nomeDisplay = nome jogador ++ if isJogadorAtual then " (VOCÊ)" else ""
            colNome <- UI.div #+ [string nomeDisplay]
                             # set UI.style 
                                 [ ("font-size", "18px")
                                 , ("font-weight", if isJogadorAtual then "bold" else "normal")
                                 , ("color", if isJogadorAtual then "#ffd700" else "#ffffff")
                                 , ("flex", "1")
                                 , ("text-align", "left")
                                 , ("padding-left", "20px")
                                 ]

            -- ID do jogador
            colID <- UI.div #+ [string $ "#" ++ show (playerID jogador)]
                           # set UI.style 
                               [ ("font-size", "14px")
                               , ("color", "#cccccc")
                               , ("width", "80px")
                               , ("text-align", "center")
                               ]

            -- Total ganho
            let ganhoFormatado = printf "R$ %.2f" (totalGanho jogador)
            colGanho <- UI.div #+ [string ganhoFormatado]
                              # set UI.style 
                                  [ ("font-size", "18px")
                                  , ("font-weight", "bold")
                                  , ("color", "#90EE90")
                                  , ("width", "120px")
                                  , ("text-align", "right")
                                  ]

            -- Total de apostas
            colApostas <- UI.div #+ [string $ show (totalApostas jogador) ++ " jogadas"]
                                # set UI.style 
                                    [ ("font-size", "14px")
                                    , ("color", "#cccccc")
                                    , ("width", "100px")
                                    , ("text-align", "center")
                                    ]

            -- Saldo atual
            let saldoFormatado = printf "R$ %.2f" (saldo jogador)
            colSaldo <- UI.div #+ [string saldoFormatado]
                              # set UI.style 
                                  [ ("font-size", "16px")
                                  , ("color", "#87CEEB")
                                  , ("width", "100px")
                                  , ("text-align", "right")
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
                               , ("padding", "15px 20px")
                               , ("margin", "5px 0")
                               , ("background", corLinha)
                               , ("border", "2px solid " ++ corBorda)
                               , ("border-radius", "10px")
                               , ("transition", "all 0.3s ease")
                               ]

            return linha

    -- Cabeçalho da tabela
    headerTabela <- UI.div #+ [
        UI.div #+ [string "POS"] # set UI.style [("width", "80px"), ("text-align", "center"), ("font-weight", "bold")],
        UI.div #+ [string "JOGADOR"] # set UI.style [("flex", "1"), ("text-align", "left"), ("padding-left", "20px"), ("font-weight", "bold")],
        UI.div #+ [string "ID"] # set UI.style [("width", "80px"), ("text-align", "center"), ("font-weight", "bold")],
        UI.div #+ [string "TOTAL GANHO"] # set UI.style [("width", "120px"), ("text-align", "right"), ("font-weight", "bold")],
        UI.div #+ [string "JOGADAS"] # set UI.style [("width", "100px"), ("text-align", "center"), ("font-weight", "bold")],
        UI.div #+ [string "SALDO"] # set UI.style [("width", "100px"), ("text-align", "right"), ("font-weight", "bold")]
        ] # set UI.style 
        [ ("display", "flex")
        , ("align-items", "center")
        , ("padding", "15px 20px")
        , ("background", "rgba(255, 215, 0, 0.2)")
        , ("border-radius", "10px")
        , ("margin-bottom", "20px")
        , ("color", "#ffd700")
        , ("border", "2px solid #ffd700")
        ]

    -- Cria as linhas do ranking
    linhasRanking <- mapM (uncurry criarLinhaRanking) (zip [1..] ranking)

    -- Mensagem se não houver jogadores suficientes
    conteudoRanking <- if null ranking
        then do
            msgVazio <- UI.div #+ [string "🎮 Ainda não há jogadores no ranking. Seja o primeiro!"]
                              # set UI.style 
                                  [ ("text-align", "center")
                                  , ("font-size", "18px")
                                  , ("color", "#cccccc")
                                  , ("padding", "40px")
                                  , ("font-style", "italic")
                                  ]
            return [msgVazio]
        else return (headerTabela : linhasRanking)

    -- Adiciona tudo ao container
    void $ element containerRanking #+ (map element conteudoRanking)

    -- Adiciona tudo ao body
    void $ element body #+ [ element titulo
                           , element btnVoltar
                           , element containerRanking
                           ]