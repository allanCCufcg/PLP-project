{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Interface.CaixaSurpresaUI (caixaSurpresaUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Jogos.CaixaSurpresa (abrirCaixa)
import Text.Printf (printf)
import EstadoGlobal (buscarJogadorPorID, Jogador(..))


caixaSurpresaUI :: Window -> Int -> UI () -> UI ()
caixaSurpresaUI window jogadorId voltarMenu = do
    body <- getBody window
    void $ element body # set UI.children []

    -- Fundo gradiente
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #03001B, #2E016F)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "20px")
        , ("text-align", "center")
        ]

    -- Título
    titulo <- UI.h1 #+ [string "🎁 Caixa Surpresa 🎁"]
          # set UI.style
            [ ("font-size", "3em")
            , ("color", "#FFD700")
            , ("text-shadow", "2px 2px 5px rgba(0,0,0,0.6)")
            , ("margin-bottom", "18px")
            , ("margin-top", "-40px")
            ]

    -- Botão voltar no topo, abaixo do título
    btnVoltar <- UI.button #+ [UI.img # set UI.src "static/CaixaSupresa/casa.png"
                                        # set UI.style [("width","24px"),("vertical-align","middle")]]
            # set UI.style
              [ ("margin-bottom", "30px")
              , ("padding", "10px 20px")
              , ("border", "none")
              , ("border-radius", "8px")
              , ("background", "#FFD700")
              , ("color", "#000")
              , ("font-size", "1em")
              , ("cursor", "pointer")
              ]
    on UI.click btnVoltar $ \_ -> voltarMenu

    -- Saldo inicial
    saldoLabel <- UI.div # set UI.style
          [ ("font-size", "1.2em")
          , ("margin-bottom", "24px")
          , ("color", "#FFD700")
          ]
          # set text "Saldo: carregando..."
    

    -- Caixa inicialmente fechada
    caixaImg <- UI.img # set UI.src "static/CaixaSupresa/CaixaFechada.PNG"
                     # set UI.style
                        [ ("width", "200px"),
                          ("margin-top", "-10px")   
                        , ("cursor", "pointer")
                        , ("transition", "transform 0.6s ease")
                        ]

    apostaMsg <- UI.div # set text "💎 Aposta: R$ 20,00"
                        # set UI.style
                            [ ("font-size", "0.8em")
                            , ("color", "#ccc")
                            , ("margin-top", "8px")
                            , ("font-style", "italic")
                            ]

    -- Área do resultado
    resultado <- UI.div # set UI.style
                [ ("margin-top", "30px")
                , ("font-size", "1.5em")
                , ("font-weight", "bold")
                , ("color", "#fff")
                ]

    -- Botão jogar novamente, inicialmente invisível
    btnJogarNovamente <- UI.button # set text "Jogar Novamente"
                                  # set UI.style
                                      [ ("margin-top", "20px")
                                      , ("padding", "10px 20px")
                                      , ("border", "none")
                                      , ("border-radius", "8px")
                                      , ("background", "#4CAF50")
                                      , ("color", "white")
                                      , ("font-size", "1em")
                                      , ("cursor", "pointer")
                                      , ("display", "none")  
                                      ]

    -- Função para fechar a caixa, esconder botão jogar novamente e habilitar clique
    let fecharCaixa = do
            void $ element caixaImg # set UI.src "static/CaixaSupresa/CaixaFechada.PNG"
            void $ element caixaImg # set UI.style [("width", "200px"), ("cursor", "pointer")]
            void $ element resultado # set text ""
            void $ element btnJogarNovamente # set UI.style [("display", "none")]
            -- Habilita clique na caixa
            void $ element caixaImg # set UI.enabled True

    -- Atualiza saldo inicial sem jogar nada
    void $ liftIO (buscarJogadorPorID jogadorId) >>= \case
        Nothing -> element saldoLabel # set text "Jogador não encontrado."
        Just jogador -> element saldoLabel # set text ("Saldo: R$ " ++ printf "%.2f" (saldo jogador))

    -- Função para lidar com o clique na caixa
    let clickCaixa = do
            -- Desabilita clique para evitar múltiplos cliques
            void $ element caixaImg # set UI.enabled False
            (premio, novoSaldo) <- liftIO $ abrirCaixa jogadorId
            void $ element caixaImg # set UI.style
                [ ("width", "230px")
                , ("cursor", "default")  -- cursor padrão pq está desabilitado
                , ("transition", "transform 0.6s ease, width 0.6s ease")
                ]
            void $ element caixaImg # set UI.src "static/CaixaSupresa/CaixaAberta.PNG"
            void $ element resultado # set text ("Você ganhou: R$ " ++ show premio)
            void $ element saldoLabel # set text ("Saldo: R$ " ++ printf "%.2f" novoSaldo)
            -- Mostrar botão jogar novamente
            void $ element btnJogarNovamente # set UI.style [("display", "inline-block")]

    -- Associa o clique na caixa à função clickCaixa
    on UI.click caixaImg $ \_ -> clickCaixa

    -- Associa o clique no botão jogar novamente à função fecharCaixa
    on UI.click btnJogarNovamente $ \_ -> fecharCaixa

    -- Inicializa com a caixa fechada e clicável
    fecharCaixa

    -- Adiciona todos os elementos ao corpo da página
    void $ element body #+ [ element titulo
                          , element btnVoltar
                          , element saldoLabel
                          , element caixaImg
                          , element apostaMsg     
                          , element resultado
                          , element btnJogarNovamente
                          ]
