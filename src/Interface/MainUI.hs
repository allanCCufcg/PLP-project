module Interface.MainUI (mainUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import EstadoGlobal (adicionarJogador)
import Interface.MenuUI (menuUI)

-- | A função principal para renderizar a interface do usuário.
mainUI :: Window -> UI ()
mainUI window = do
    -- Configura o título e a folha de estilo
    return window # set UI.title "Cassino PLP"
    UI.addStyleSheet window "https://fonts.googleapis.com/css2?family=Orbitron&display=swap"

    -- Estilo do corpo da página com tema de cassino
    body <- getBody window
    element body # set UI.style
        [ ("background-color", "#2c1f13") -- marrom escuro tipo cassino
        , ("color", "#ffd700")            -- dourado
        , ("font-family", "'Orbitron', sans-serif")
        , ("height", "100vh")
        , ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]
        
    -- Cria o título da página
    titulo <- UI.h1 #+ [string "Cassino"] 
                     # set UI.style [("margin-bottom", "20px")]

    -- Cria o input de texto e o botão
    nomeInput <- UI.input # set (attr "placeholder") "Digite seu nome"
                          # set UI.style [("padding", "10px"), ("font-size", "16px"), ("margin-right", "10px"), ("border-radius", "8px")]

    btn <- UI.button #+ [string "Entrar"]
                     # set UI.style [("padding", "8px 20px"), ("font-size", "16px"), ("cursor", "pointer"), ("border-radius", "8px")]

    -- Cria um parágrafo para exibir mensagens de aviso
    aviso <- UI.p # set UI.text "" 
                  # set UI.style [("margin-top", "10px")]

    -- Container principal com estilo de card
    container <- UI.div #+ [element titulo, element nomeInput, element btn]
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

    -- Adiciona o container e a mensagem de aviso ao corpo da página
    element body #+ [element container, element aviso]

    -- Evento de clique do botão
    on UI.click btn $ \_ -> do
        nome <- get UI.value nomeInput
        if null nome
            then element aviso # set UI.text ("O nome não pode ser vazio!") >> return ()
            else do
                liftIO $ adicionarJogador nome
                element body # set UI.children []
                menuUI window
