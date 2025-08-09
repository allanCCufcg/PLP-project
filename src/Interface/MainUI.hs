module Interface.MainUI (mainUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (adicionarJogador, buscarJogadorPorID)
import Interface.MenuUI (menuUI)
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)

mainUI :: Window -> UI ()
mainUI window = do
    return window # set UI.title "Cassino PLP"

    body <- getBody window
    element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "Arial, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]

    -- Variável de estado para guardar o ID do jogador atual
    jogadorIdRef <- liftIO $ newIORef Nothing

    -- Título principal
    titulo <- UI.h1 #+ [string "🎰 EL CASSINO 🎰"]
                   # set UI.style 
                       [ ("color", "#ffd700")
                       , ("font-size", "3.5em")
                       , ("margin-bottom", "40px")
                       , ("text-shadow", "2px 2px 8px rgba(0,0,0,0.5)")
                       , ("text-align", "center")
                       ]

    -- Campo de entrada
    campoInput <- UI.input # set (attr "placeholder") "Digite seu nome ou ID"
                          # set UI.style 
                              [ ("padding", "15px 20px")
                              , ("font-size", "16px")
                              , ("border", "2px solid #ffd700")
                              , ("border-radius", "10px")
                              , ("background", "rgba(255,255,255,0.1)")
                              , ("color", "#ffffff")
                              , ("width", "300px")
                              , ("text-align", "center")
                              , ("margin-bottom", "25px")
                              ]

    -- Botões principais
    btnCadastrar <- UI.button #+ [string "📝 CADASTRAR"]
                             # set UI.style 
                                 [ ("padding", "15px 30px")
                                 , ("font-size", "16px")
                                 , ("font-weight", "bold")
                                 , ("background", "linear-gradient(145deg, #2a5a2a, #1a4a1a)")
                                 , ("color", "#90ee90")
                                 , ("border", "2px solid #4a8a4a")
                                 , ("border-radius", "10px")
                                 , ("cursor", "pointer")
                                 , ("margin", "0 10px")
                                 , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                 ]

    btnEntrar <- UI.button #+ [string "🚪 ENTRAR"]
                          # set UI.style 
                              [ ("padding", "15px 30px")
                              , ("font-size", "16px")
                              , ("font-weight", "bold")
                              , ("background", "linear-gradient(145deg, #2a2a5a, #1a1a3a)")
                              , ("color", "#ffd700")
                              , ("border", "2px solid #ffd700")
                              , ("border-radius", "10px")
                              , ("cursor", "pointer")
                              , ("margin", "0 10px")
                              , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                              ]

    -- Botão "Ir para o Menu" (inicialmente oculto)
    btnMenu <- UI.button #+ [string "🎮 IR PARA O MENU"]
                        # set UI.style 
                            [ ("padding", "15px 30px")
                            , ("font-size", "16px")
                            , ("font-weight", "bold")
                            , ("background", "linear-gradient(145deg, #5a2a2a, #3a1a1a)")
                            , ("color", "#ff9090")
                            , ("border", "2px solid #ff6b6b")
                            , ("border-radius", "10px")
                            , ("cursor", "pointer")
                            , ("margin-top", "15px")
                            , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                            , ("opacity", "0")
                            , ("transition", "opacity 0.5s ease")
                            , ("pointer-events", "none")
                            ]

    -- Mensagem de feedback
    aviso <- UI.p # set UI.text ""
                 # set UI.style 
                     [ ("margin-top", "25px")
                     , ("text-align", "center")
                     , ("font-size", "18px")
                     , ("font-weight", "bold")
                     , ("min-height", "25px")
                     ]

    -- Container dos botões
    containerBotoes <- UI.div #+ [element btnCadastrar, element btnEntrar]
                             # set UI.style 
                                 [ ("display", "flex")
                                 , ("gap", "20px")
                                 , ("justify-content", "center")
                                 ]

    -- Container principal
    container <- UI.div #+ [ element titulo
                           , element campoInput
                           , element containerBotoes
                           , element btnMenu
                           , element aviso
                           ]
                       # set UI.style
                           [ ("background", "rgba(0,0,0,0.3)")
                           , ("padding", "50px")
                           , ("border-radius", "20px")
                           , ("border", "2px solid #ffd700")
                           , ("box-shadow", "0 10px 30px rgba(0,0,0,0.5)")
                           , ("display", "flex")
                           , ("flex-direction", "column")
                           , ("align-items", "center")
                           , ("backdrop-filter", "blur(10px)")
                           ]

    element body #+ [element container]

    -- Evento Cadastrar
    on UI.click btnCadastrar $ \_ -> do
        nome <- get UI.value campoInput
        if null nome
            then void $ element aviso # set UI.text "⚠️ O nome não pode ser vazio!"
                                     # set UI.style [("color", "#ff6b6b")]
            else do
                novoId <- liftIO $ adicionarJogador nome
                void $ element aviso # set UI.text ("✅ Usuário cadastrado com ID: " ++ show novoId)
                                    # set UI.style [("color", "#90ee90")]
                -- Salva o novo ID
                liftIO $ writeIORef jogadorIdRef (Just novoId)
                -- Mostra o botão menu
                void $ element btnMenu # set UI.style 
                    [ ("padding", "15px 30px")
                    , ("font-size", "16px")
                    , ("font-weight", "bold")
                    , ("background", "linear-gradient(145deg, #5a2a2a, #3a1a1a)")
                    , ("color", "#ff9090")
                    , ("border", "2px solid #ff6b6b")
                    , ("border-radius", "10px")
                    , ("cursor", "pointer")
                    , ("margin-top", "15px")
                    , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                    , ("opacity", "1")
                    , ("transition", "opacity 0.5s ease")
                    , ("pointer-events", "auto")
                    ]

    -- Evento Entrar
    on UI.click btnEntrar $ \_ -> do
        valor <- get UI.value campoInput
        case readMaybe valor of
            Nothing -> void $ element aviso # set UI.text "⚠️ Digite um ID válido!"
                                           # set UI.style [("color", "#ff6b6b")]
            Just idNum -> do
                maybeJogador <- liftIO $ buscarJogadorPorID idNum
                case maybeJogador of
                    Nothing -> void $ element aviso # set UI.text "❌ ID não encontrado!"
                                                   # set UI.style [("color", "#ff6b6b")]
                    Just _  -> do
                        void $ element aviso # set UI.text "🎮 Entrando no cassino..."
                                            # set UI.style [("color", "#90ee90")]
                        void $ element body # set UI.children []
                        liftIO $ writeIORef jogadorIdRef (Just idNum)
                        menuUI window idNum

    -- Evento "Ir para o Menu"
    on UI.click btnMenu $ \_ -> do
        maybeId <- liftIO $ readIORef jogadorIdRef
        case maybeId of
            Nothing -> void $ element aviso # set UI.text "❌ Erro: Nenhum jogador logado."
                                           # set UI.style [("color", "#ff6b6b")]
            Just idNum -> do
                void $ element body # set UI.children []
                menuUI window idNum