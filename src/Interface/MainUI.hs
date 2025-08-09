module Interface.MainUI (mainUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (adicionarJogador, buscarJogadorPorID)
import Interface.MenuUI (menuUI)
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

mainUI :: Window -> UI ()
mainUI window = do
    return window # set UI.title "Cassino PLP"
    UI.addStyleSheet window "https://fonts.googleapis.com/css2?family=Orbitron&display=swap"

    body <- getBody window
    element body # set UI.style
        [ ("background-color", "#2c1f13")
        , ("color", "#ffd700")
        , ("font-family", "'Orbitron', sans-serif")
        , ("height", "100vh")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]

    -- Variável de estado para guardar o ID do jogador atual
    jogadorIdRef <- liftIO $ newIORef Nothing

    -- Título
    titulo <- UI.h1 #+ [string "Cassino"]
                     # set UI.style [("margin-bottom", "20px")]

    -- Campo para nome ou ID
    campoInput <- UI.input # set (attr "placeholder") "Digite seu nome ou ID"
                           # set UI.style [("padding", "10px"), ("font-size", "16px"), ("margin-right", "10px"), ("border-radius", "8px")]

    -- Botões principais
    btnCadastrar <- UI.button #+ [string "Cadastrar"]
                               # set UI.style [("padding", "8px 20px"), ("font-size", "16px"), ("cursor", "pointer"), ("border-radius", "8px")]

    btnEntrar <- UI.button #+ [string "Entrar"]
                           # set UI.style [("padding", "8px 20px"), ("font-size", "16px"), ("cursor", "pointer"), ("border-radius", "8px")]

    -- Botão "Ir para o Menu" (inicialmente oculto)
    btnMenu <- UI.button #+ [string "Ir para o Menu"]
                         # set UI.style [("padding", "8px 20px"), ("font-size", "16px"), ("cursor", "pointer")
                                       , ("border-radius", "8px"), ("opacity", "0"), ("transition", "opacity 0.5s ease")
                                       , ("pointer-events", "none")]

    -- Mensagem de aviso / sucesso
    aviso <- UI.p # set UI.text ""
                  # set UI.style [("margin-top", "25px"), ("text-align", "center"), ("font-size", "18px")]

    -- Container principal
    container <- UI.div #+ [ element titulo
                           , element campoInput
                           , UI.div #+ [element btnCadastrar, element btnEntrar]
                                    # set UI.style [("display", "flex"), ("gap", "10px")]
                           , element btnMenu
                           ]
                         # set UI.style
                             [ ("background", "#3b2614")
                             , ("padding", "40px")
                             , ("border-radius", "10px")
                             , ("box-shadow", "0 0 15px #ffd700")
                             , ("display", "flex")
                             , ("flex-direction", "column")
                             , ("align-items", "center")
                             , ("gap", "20px")
                             ]

    element body #+ [element container, element aviso]

    -- Botão Cadastrar
    on UI.click btnCadastrar $ \_ -> do
        nome <- get UI.value campoInput
        if null nome
            then void $ element aviso # set UI.text "O nome não pode ser vazio!"
            else do
                novoId <- liftIO $ adicionarJogador nome
                void $ element aviso # set UI.text ("Usuário cadastrado com ID: " ++ show novoId)
                -- Salva o novo ID na IORef
                liftIO $ writeIORef jogadorIdRef (Just novoId)
                -- Mostra o botão "Ir para o Menu"
                void $ element btnMenu # set UI.style [("opacity", "1"), ("transition", "opacity 0.5s ease"), ("pointer-events", "auto")]

    -- Botão Entrar
    on UI.click btnEntrar $ \_ -> do
        valor <- get UI.value campoInput
        case readMaybe valor of
            Nothing -> void $ element aviso # set UI.text "Digite um ID válido!"
            Just idNum -> do
                maybeJogador <- liftIO $ buscarJogadorPorID idNum
                case maybeJogador of
                    Nothing -> void $ element aviso # set UI.text "ID não encontrado!"
                    Just _  -> do
                        void $ element aviso # set UI.text "Entrando..."
                        void $ element body # set UI.children []
                        -- Salva o ID do jogador logado na IORef
                        liftIO $ writeIORef jogadorIdRef (Just idNum)
                        menuUI window idNum

    -- Botão "Ir para o Menu" (após cadastro ou login)
    on UI.click btnMenu $ \_ -> do
        maybeId <- liftIO $ readIORef jogadorIdRef
        case maybeId of
            Nothing -> void $ element aviso # set UI.text "Erro: Nenhum jogador logado."
            Just idNum -> do
                void $ element body # set UI.children []
                menuUI window idNum