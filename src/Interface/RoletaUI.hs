module Interface.RoletaUI (roletaUI) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Jogos.Roleta as Logica
import EstadoGlobal (PlayerID, registrarJogada, buscarJogadorPorID, Jogador(..))
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

-- Função para mapear cor para cor CSS
corParaCSS :: Logica.CorRoleta -> String
corParaCSS cor = case cor of
    Logica.Vermelho -> "#ff4444"
    Logica.Preto    -> "#333333"
    Logica.Verde    -> "#44ff44"

-- Função auxiliar para atualizar o display do saldo
atualizarSaldo :: Element -> PlayerID -> UI Element
atualizarSaldo saldoDisplay pid = do
    saldo <- liftIO $ Logica.mostraSaldoRoleta pid
    element saldoDisplay # set UI.text ("💰 Saldo: R$ " ++ show saldo)

-- Função para adicionar estilos CSS e JavaScript para animações
adicionarEstilosEAnimacoes :: Window -> UI ()
adicionarEstilosEAnimacoes window = do
    -- CSS para animações
    let cssAnimacoes = unlines
            [ "<style>"
            , "/* ==================== RESPONSIVIDADE APRIMORADA ==================== */"
            , "/* Correção para evitar sobreposição do saldo */"
            , "@media screen and (max-width: 1400px) {"
            , "  .roleta-visual { width: 200px !important; height: 200px !important; }"
            , "  .resultado-centro { width: 80px !important; height: 80px !important; font-size: 2.2em !important; }"
            , "  .saldo-display { font-size: 1.1em !important; padding: 8px 15px !important; margin: 8px auto 15px auto !important; }"
            , "}"
            , ""
            , "@media screen and (max-width: 1200px) {"
            , "  .container-jogo { max-width: 450px !important; padding: 20px !important; }"
            , "  .botao-cor { padding: 10px 15px !important; font-size: 0.9em !important; margin: 6px !important; }"
            , "  .saldo-display { font-size: 1em !important; padding: 8px 12px !important; margin: 10px auto 15px auto !important; }"
            , "}"
            , ""
            , "@media screen and (max-width: 1000px) {"
            , "  .roleta-visual { width: 180px !important; height: 180px !important; }"
            , "  .resultado-centro { width: 70px !important; height: 70px !important; font-size: 2em !important; }"
            , "  .container-jogo { max-width: 400px !important; padding: 15px !important; }"
            , "  .saldo-display { font-size: 0.95em !important; padding: 6px 10px !important; margin: 8px auto 12px auto !important; }"
            , "}"
            , ""
            , "@media screen and (max-width: 800px) {"
            , "  .titulo-principal { font-size: 1.5em !important; }"
            , "  .saldo-display { font-size: 0.9em !important; padding: 6px 10px !important; margin: 6px auto 10px auto !important; }"
            , "  .botao-girar { font-size: 1.1em !important; padding: 12px 25px !important; }"
            , "  .content-wrapper { padding-top: 65px !important; } /* Espaçamento extra para telas menores */"
            , "}"
            , ""
            , "@media screen and (max-width: 600px) {"
            , "  .saldo-display { font-size: 0.85em !important; padding: 5px 8px !important; margin: 5px auto 8px auto !important; }"
            , "  .content-wrapper { padding-top: 70px !important; } /* Ainda mais espaço para telas muito pequenas */"
            , "  .container-jogo { padding: 10px !important; }"
            , "}"
            , ""
            , "/* ==================== ANIMAÇÕES APRIMORADAS ==================== */"
            , ".roleta-container {"
            , "  transition: transform 0.1s ease;"
            , "  transform-origin: center center; /* Garantir rotação do centro */"
            , "}"
            , ""
            , "/* Correção da animação da roleta */"
            , ".roleta-visual {"
            , "  transform-origin: center center !important;"
            , "  will-change: transform; /* Otimização para animações */"
            , "}"
            , ""
            , ".roleta-girando {"
            , "  animation: girarRoleta 2s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;"
            , "}"
            , ""
            , "@keyframes girarRoleta {"
            , "  0% { transform: rotate(0deg); }"
            , "  100% { transform: rotate(1440deg); }"
            , "}"
            , ""
            , ".botao-hover {"
            , "  transition: all 0.3s ease;"
            , "  transform: scale(1);"
            , "}"
            , ".botao-hover:hover {"
            , "  transform: scale(1.05);"
            , "  box-shadow: 0 5px 15px rgba(0,0,0,0.3);"
            , "}"
            , ".botao-clicado {"
            , "  transform: scale(0.95);"
            , "  transition: transform 0.1s ease;"
            , "}"
            , ".resultado-animacao {"
            , "  animation: pulsarResultado 0.5s ease-in-out;"
            , "}"
            , "@keyframes pulsarResultado {"
            , "  0% { transform: scale(1); }"
            , "  50% { transform: scale(1.1); }"
            , "  100% { transform: scale(1); }"
            , "}"
            , ".vitoria-animacao {"
            , "  animation: celebrarVitoria 1s ease-in-out;"
            , "}"
            , "@keyframes celebrarVitoria {"
            , "  0% { transform: scale(1) rotate(0deg); }"
            , "  25% { transform: scale(1.2) rotate(5deg); }"
            , "  50% { transform: scale(1.1) rotate(-5deg); }"
            , "  75% { transform: scale(1.2) rotate(3deg); }"
            , "  100% { transform: scale(1) rotate(0deg); }"
            , "}"
            , ".derrota-animacao {"
            , "  animation: lamentarDerrota 0.8s ease-in-out;"
            , "}"
            , "@keyframes lamentarDerrota {"
            , "  0% { transform: translateX(0); }"
            , "  25% { transform: translateX(-10px); }"
            , "  50% { transform: translateX(10px); }"
            , "  75% { transform: translateX(-5px); }"
            , "  100% { transform: translateX(0); }"
            , "}"
            , ".centralizado {"
            , "  display: flex;"
            , "  justify-content: center;"
            , "  align-items: center;"
            , "  width: 100%;"
            , "}"
            , "</style>"
            ]

    -- JavaScript aprimorado com correções
    let jsScript = unlines
            [ "<script>"
            , "function tocarSom(tipoSom) {"
            , "  const audio = document.getElementById('audio' + tipoSom);"
            , "  if (audio) {"
            , "    audio.currentTime = 0;"
            , "    audio.play().catch(e => console.log('Erro ao tocar som:', e));"
            , "  }"
            , "}"
            , ""
            , "// Correção da função de girar roleta"
            , "function girarRoletaParaCor(cor) {"
            , "  const roleta = document.getElementById('roletaVisual');"
            , "  if (roleta) {"
            , "    // Primeiro, resetar qualquer transformação anterior"
            , "    roleta.style.transition = 'none';"
            , "    roleta.style.transform = 'rotate(0deg)';"
            , "    "
            , "    // Forçar reflow para garantir que o reset seja aplicado"
            , "    roleta.offsetHeight;"
            , "    "
            , "    let anguloFinal = 0;"
            , "    // Ângulos ajustados para o ponteiro no topo (12h = 0deg)"
            , "    if (cor === 'Vermelho') {"
            , "      anguloFinal = 275; // Para parar na seção vermelha"
            , "    } else if (cor === 'Preto') {"
            , "      anguloFinal = 105; // Para parar na seção preta" 
            , "    } else if (cor === 'Verde') {"
            , "      anguloFinal = 10; // Para parar na seção verde"
            , "    }"
            , "    "
            , "    // Adicionar variação aleatória para tornar mais realista"
            , "    const variacao = Math.random() * 60 - 30; // ±30 graus de variação"
            , "    anguloFinal += variacao;"
            , "    "
            , "    // 4 voltas completas + ângulo final"
            , "    const anguloTotal = 1440 + anguloFinal;"
            , "    "
            , "    // Aplicar a animação após um pequeno delay"
            , "    setTimeout(() => {"
            , "      roleta.style.transition = 'transform 2s cubic-bezier(0.25, 0.46, 0.45, 0.94)';"
            , "      roleta.style.transform = `rotate(${anguloTotal}deg)`;"
            , "    }, 50);"
            , "    "
            , "    console.log(`Girando roleta para ${cor}: ${anguloTotal} graus`);"
            , "  } else {"
            , "    console.error('Elemento roletaVisual não encontrado!');"
            , "  }"
            , "}"
            , ""
            , "function animarBotao(elementId) {"
            , "  const botao = document.getElementById(elementId);"
            , "  if (botao) {"
            , "    botao.classList.add('botao-clicado');"
            , "    setTimeout(() => {"
            , "      botao.classList.remove('botao-clicado');"
            , "    }, 150);"
            , "  }"
            , "}"
            , ""
            , "function animarResultado(tipo) {"
            , "  const resultado = document.getElementById('resultadoTexto');"
            , "  if (resultado) {"
            , "    resultado.classList.remove('vitoria-animacao', 'derrota-animacao', 'resultado-animacao');"
            , "    setTimeout(() => {"
            , "      if (tipo === 'vitoria') {"
            , "        resultado.classList.add('vitoria-animacao');"
            , "      } else if (tipo === 'derrota') {"
            , "        resultado.classList.add('derrota-animacao');"
            , "      } else {"
            , "        resultado.classList.add('resultado-animacao');"
            , "      }"
            , "    }, 50);"
            , "  }"
            , "}"
            , ""
            , "function desabilitarBotoesEscolha() {"
            , "  const botoes = ['btnVermelho', 'btnPreto', 'btnVerde'];"
            , "  botoes.forEach(id => {"
            , "    const btn = document.getElementById(id);"
            , "    if (btn) {"
            , "      btn.style.opacity = '0.3';"
            , "      btn.style.cursor = 'not-allowed';"
            , "      btn.style.pointerEvents = 'none';"
            , "    }"
            , "  });"
            , "}"
            , ""
            , "function reabilitarBotoesEscolha() {"
            , "  const botoes = ['btnVermelho', 'btnPreto', 'btnVerde'];"
            , "  botoes.forEach(id => {"
            , "    const btn = document.getElementById(id);"
            , "    if (btn) {"
            , "      btn.style.opacity = '0.7';"
            , "      btn.style.cursor = 'pointer';"
            , "      btn.style.pointerEvents = 'auto';"
            , "    }"
            , "  });"
            , "}"
            , ""
            , "// Loop seamless da música de fundo"
            , "document.addEventListener('DOMContentLoaded', function() {"
            , "  const audioMusica = document.getElementById('audioMusicaRoleta');"
            , "  if (audioMusica) {"
            , "    // Garantir loop sem pausas"
            , "    audioMusica.addEventListener('timeupdate', function() {"
            , "      // Reiniciar um pouco antes do final para evitar pausa"
            , "      if (audioMusica.currentTime >= audioMusica.duration - 0.1) {"
            , "        audioMusica.currentTime = 0;"
            , "      }"
            , "    });"
            , "    "
            , "    // Forçar reinício se pausar por qualquer motivo"
            , "    audioMusica.addEventListener('ended', function() {"
            , "      audioMusica.currentTime = 0;"
            , "      audioMusica.play().catch(e => console.log('Erro ao reiniciar música:', e));"
            , "    });"
            , "  }"
            , "});"
            , ""
            , "// Adicionar efeitos hover aos botões"
            , "document.addEventListener('DOMContentLoaded', function() {"
            , "  const botoes = document.querySelectorAll('button');"
            , "  botoes.forEach(botao => {"
            , "    if (!botao.classList.contains('no-hover')) {"
            , "      botao.classList.add('botao-hover');"
            , "    }"
            , "  });"
            , "});"
            , "</script>"
            ]

    getHead window >>= \h -> void $ element h #+ [UI.mkElement "style" # set UI.html cssAnimacoes]
    getHead window >>= \h -> void $ element h #+ [UI.mkElement "script" # set UI.html jsScript]

-- | Interface da Roleta
roletaUI :: Window -> PlayerID -> UI () -> UI ()
roletaUI window playerId voltarAoMenu = do
    -- Adicionar estilos e animações
    adicionarEstilosEAnimacoes window
    
    -- Estado para controlar se a música está mutada
    musicMuted <- liftIO $ newIORef False
    -- Estado para controlar a cor escolhida
    corEscolhida <- liftIO $ newIORef (Nothing :: Maybe Logica.CorRoleta)
    
    body <- getBody window
    void $ element body # set UI.children []
    
    -- Estilo de fundo do body
    void $ element body # set UI.style
        [ ("background", "linear-gradient(135deg, #1a1a2e, #16213e, #0f3460)")
        , ("color", "#ffffff")
        , ("font-family", "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif")
        , ("height", "100vh")
        , ("margin", "0")
        , ("padding", "0")
        , ("overflow", "hidden")
        ]

    -- Busca os dados do jogador para a nav-bar
    maybeJogador <- liftIO $ buscarJogadorPorID playerId
    case maybeJogador of
        Just jogador -> do
            -- Nav-bar FIXA NO TOPO (altura 60px)
            let nomeStr = "👤 " ++ nome jogador
                idStr = "🎫 ID: " ++ show (playerID jogador)
                saldoStr = "💰 $" ++ show (saldo jogador)
            
            pNome <- UI.span #+ [string nomeStr] 
                            # set UI.style [("margin", "0"), ("color", "#ffd700")]
            pId <- UI.span #+ [string idStr] 
                          # set UI.style [("margin", "0"), ("color", "#ffd700")]
            pSaldo <- UI.span #+ [string saldoStr] 
                             # set UI.style [("margin", "0"), ("color", "#ffd700")]

            navBarElem <- UI.div #+ [element pNome, element pId, element pSaldo] 
                                # set UI.style 
                                    [ ("background", "rgba(0,0,0,0.3)")
                                    , ("padding", "10px 30px")
                                    , ("width", "100%")
                                    , ("height", "40px")
                                    , ("display", "flex")
                                    , ("justify-content", "space-around")
                                    , ("align-items", "center")
                                    , ("position", "fixed")
                                    , ("top", "0")
                                    , ("left", "0")
                                    , ("z-index", "1000")
                                    , ("box-shadow", "0 2px 10px rgba(0, 0, 0, 0.7)")
                                    , ("font-size", "1em")
                                    , ("border-bottom", "2px solid #ffd700")
                                    , ("backdrop-filter", "blur(10px)")
                                    ]

            -- Container principal com classe para responsividade
            contentWrapper <- UI.div # set (UI.attr "class") "content-wrapper"
                                    # set UI.style 
                [ ("padding-top", "60px")
                , ("height", "calc(100vh - 60px)")
                , ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "space-between")
                , ("text-align", "center")
                , ("overflow", "hidden")
                , ("width", "100%")
                , ("box-sizing", "border-box")
                ]

            -- Header compacto com título e controles
            headerSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("margin", "10px 0")
                , ("flex-shrink", "0") -- Evita que o header encolha
                ]

            -- Título com efeito glow
            title <- UI.h1 #+ [string "🎰 ROLETA 🎰"]
                          # set (UI.attr "class") "titulo-principal"
                          # set UI.style 
                              [ ("color", "#ffd700")
                              , ("font-size", "1.8em")
                              , ("margin", "0 0 10px 0")
                              , ("text-shadow", "0 0 20px rgba(255, 215, 0, 0.8), 2px 2px 4px rgba(0,0,0,0.5)")
                              , ("font-weight", "bold")
                              ]

            -- Botões de controle com efeitos
            btnMuteMusic <- UI.button #+ [string "🔊"]
                                     # set (UI.attr "id") "btnMuteMusic"
                                     # set (UI.attr "class") "no-hover"
                                     # set UI.style
                                         [ ("padding", "8px 12px")
                                         , ("font-size", "16px")
                                         , ("background", "linear-gradient(145deg, #2a5a2a, #1a4a1a)")
                                         , ("color", "#90ee90")
                                         , ("border", "2px solid #4a8a4a")
                                         , ("border-radius", "8px")
                                         , ("cursor", "pointer")
                                         , ("margin", "0 5px")
                                         , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                         ]

            btnVoltarMenu <- UI.button #+ [string "🏠 MENU"]
                                      # set (UI.attr "id") "btnVoltarMenu"
                                      # set (UI.attr "class") "no-hover"
                                      # set UI.style
                                          [ ("padding", "8px 15px")
                                          , ("font-size", "14px")
                                          , ("font-weight", "bold")
                                          , ("background", "linear-gradient(145deg, #5a2a2a, #4a1a1a)")
                                          , ("color", "#ff9090")
                                          , ("border", "2px solid #8a4a4a")
                                          , ("border-radius", "8px")
                                          , ("cursor", "pointer")
                                          , ("margin", "0 5px")
                                          , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                          ]

            controlesContainer <- UI.div #+ [element btnMuteMusic, element btnVoltarMenu]
                                        # set UI.style 
                                            [ ("display", "flex")
                                            , ("justify-content", "center")
                                            , ("margin", "5px 0")
                                            ]

            -- Adicionar título
            void $ element headerSection #+ [element title, element controlesContainer]

            -- Seção do jogo
            gameSection <- UI.div # set UI.style
                [ ("display", "flex")
                , ("flex-direction", "column")
                , ("align-items", "center")
                , ("justify-content", "center")
                , ("flex-grow", "1")
                , ("min-height", "0")
                , ("width", "100%")
                , ("padding", "0 20px")
                , ("box-sizing", "border-box")
                , ("overflow", "auto") -- Permitir scroll se necessário
                ]

            -- Saldo display com correções de posicionamento
            saldoDisplay <- UI.p # set UI.text ("💰 R$ " ++ show (saldo jogador))
                                # set (UI.attr "class") "saldo-display"
                                # set UI.style 
                                    [ ("font-size", "1.2em")
                                    , ("color", "#ffd700")
                                    , ("font-weight", "bold")
                                    , ("background", "rgba(0,0,0,0.4)")
                                    , ("padding", "10px 20px")
                                    , ("border-radius", "8px")
                                    , ("border", "2px solid #ffd700")
                                    , ("margin", "8px auto 15px auto") -- Margin inferior maior para separar do jogo
                                    , ("box-shadow", "0 0 15px rgba(255, 215, 0, 0.3)")
                                    , ("text-align", "center")
                                    , ("display", "block")
                                    , ("position", "relative") -- Garantir posicionamento normal
                                    , ("z-index", "1") -- Baixo z-index para não sobrepor nada
                                    ]

            roletaVisualContainer <- UI.div # set UI.style
                [ ("display", "flex")
                , ("justify-content", "center")
                , ("align-items", "center")
                , ("margin", "20px 0")
                , ("width", "100%")
                ]

            roletaContainer2 <- UI.div # set UI.style
                [ ("position", "relative")
                , ("width", "220px")
                , ("height", "220px")
                , ("margin", "0 auto")
                ]

            roletaDisplay <- UI.div # set (UI.attr "id") "roletaVisual"
                                   # set (UI.attr "class") "roleta-container roleta-visual"
                                   # set UI.style
                [ ("width", "220px")
                , ("height", "220px")
                , ("border-radius", "50%")
                , ("background", "conic-gradient(from 0deg, #ff4444 0deg 170deg, #333333 170deg 340deg, #44ff44 340deg 360deg)")
                , ("border", "10px solid #ffd700")
                , ("position", "absolute")
                , ("top", "0")
                , ("left", "0")
                , ("box-shadow", "0 0 30px rgba(255, 215, 0, 0.6), inset 0 0 20px rgba(0,0,0,0.3)")
                , ("pointer-events", "none")
                , ("transform-origin", "center center") -- Garantir rotação do centro
                ]

            ponteiro <- UI.div # set UI.style
                [ ("position", "absolute")
                , ("top", "-15px")
                , ("left", "50%")
                , ("transform", "translateX(-50%)")
                , ("width", "0")
                , ("height", "0")
                , ("border-left", "18px solid transparent")
                , ("border-right", "18px solid transparent")
                , ("border-top", "35px solid #ffd700")
                , ("z-index", "20")
                , ("filter", "drop-shadow(0 3px 6px rgba(0,0,0,0.5))")
                , ("pointer-events", "none")
                ]

            -- Display do resultado
            resultadoDisplay <- UI.div # set UI.text "?"
                                      # set (UI.attr "id") "roletaResultDisplay"
                                      # set (UI.attr "class") "resultado-centro"
                                      # set UI.style
                                          [ ("width", "90px")
                                          , ("height", "90px")
                                          , ("border-radius", "50%")
                                          , ("background", "#333333")
                                          , ("border", "5px solid #ffd700")
                                          , ("display", "flex")
                                          , ("align-items", "center")
                                          , ("justify-content", "center")
                                          , ("position", "absolute")
                                          , ("top", "50%")
                                          , ("left", "50%")
                                          , ("transform", "translate(-50%, -50%)")
                                          , ("font-size", "2.5em")
                                          , ("font-weight", "bold")
                                          , ("color", "#ffffff")
                                          , ("box-shadow", "inset 0 0 10px rgba(0,0,0,0.5)")
                                          , ("z-index", "15")
                                          , ("pointer-events", "none")
                                          ]

            void $ element roletaContainer2 #+ [element roletaDisplay, element ponteiro, element resultadoDisplay]
            void $ element roletaVisualContainer #+ [element roletaContainer2]

            -- Botões de escolha de cor
            btnVermelho <- UI.button #+ [string "🔴 VERMELHO (47%) - 2x"]
                                    # set (UI.attr "id") "btnVermelho"
                                    # set UI.style
                                        [ ("padding", "12px 18px")
                                        , ("font-size", "1em")
                                        , ("font-weight", "bold")
                                        , ("background", "linear-gradient(145deg, #ff6666, #cc3333)")
                                        , ("color", "#ffffff")
                                        , ("border", "2px solid #ff4444")
                                        , ("border-radius", "10px")
                                        , ("cursor", "pointer")
                                        , ("margin", "8px")
                                        , ("opacity", "0.7")
                                        , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                        , ("pointer-events", "auto")
                                        ]

            btnPreto <- UI.button #+ [string "⚫ PRETO (47%) - 2x"]
                                 # set (UI.attr "id") "btnPreto"
                                 # set UI.style
                                     [ ("padding", "12px 18px")
                                     , ("font-size", "1em")
                                     , ("font-weight", "bold")
                                     , ("background", "linear-gradient(145deg, #555555, #222222)")
                                     , ("color", "#ffffff")
                                     , ("border", "2px solid #333333")
                                     , ("border-radius", "10px")
                                     , ("cursor", "pointer")
                                     , ("margin", "8px")
                                     , ("opacity", "0.7")
                                     , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                     , ("pointer-events", "auto")
                                     ]

            btnVerde <- UI.button #+ [string "🟢 VERDE (6%) - 15x"]
                                 # set (UI.attr "id") "btnVerde"
                                 # set UI.style
                                     [ ("padding", "12px 18px")
                                     , ("font-size", "1em")
                                     , ("font-weight", "bold")
                                     , ("background", "linear-gradient(145deg, #66ff66, #33cc33)")
                                     , ("color", "#ffffff")
                                     , ("border", "2px solid #44ff44")
                                     , ("border-radius", "10px")
                                     , ("cursor", "pointer")
                                     , ("margin", "8px")
                                     , ("opacity", "0.7")
                                     , ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")
                                     ]

            botoesEscolha <- UI.div #+ [element btnVermelho, element btnPreto, element btnVerde]
                                   # set UI.style
                                       [ ("display", "flex")
                                       , ("justify-content", "center")
                                       , ("flex-wrap", "wrap")
                                       , ("margin", "15px 0")
                                       ]

            -- Informação da aposta
            infoAposta <- UI.p # set UI.text ("💎 Aposta: R$ " ++ show Logica.valorApostaRoleta)
                              # set UI.style 
                                  [ ("font-size", "1em")
                                  , ("color", "#ffffff")
                                  , ("margin", "8px 0")
                                  , ("font-weight", "500")
                                  ]

            -- Botão girar
            btnGirar <- UI.button #+ [string "🎰 GIRAR ROLETA!"]
                        # set (UI.attr "id") "btnGirar"
                        # set (UI.attr "class") "botao-girar"
                        # set UI.style
                            [ ("font-size", "1.3em")
                            , ("font-weight", "bold")
                            , ("padding", "15px 30px")
                            , ("background", "linear-gradient(145deg, #666666, #444444)")
                            , ("color", "#999999")
                            , ("border", "2px solid #666666")
                            , ("border-radius", "12px")
                            , ("cursor", "not-allowed")
                            , ("margin", "15px 0")
                            , ("box-shadow", "0 6px 12px rgba(0,0,0,0.3)")
                            ]

            resultado <- UI.p # set UI.text "Escolha uma cor para apostar!"
                             # set (UI.attr "id") "resultadoTexto"
                             # set UI.style 
                                 [ ("font-size", "1.1em")
                                 , ("font-weight", "bold")
                                 , ("margin", "10px 0")
                                 , ("min-height", "30px")
                                 , ("color", "#ffd700")
                                 ]

            roletaContainer <- UI.div #+ [ element roletaVisualContainer
                                         , element botoesEscolha
                                         , element infoAposta
                                         , element btnGirar
                                         , element resultado
                                         ]
                                     # set (UI.attr "class") "container-jogo"
                                     # set UI.style
                                         [ ("background", "rgba(0,0,0,0.2)")
                                         , ("border", "3px solid #ffd700")
                                         , ("border-radius", "20px")
                                         , ("padding", "25px")
                                         , ("backdrop-filter", "blur(15px)")
                                         , ("box-shadow", "0 8px 32px rgba(0,0,0,0.4)")
                                         , ("display", "flex")
                                         , ("flex-direction", "column")
                                         , ("align-items", "center")
                                         , ("width", "100%")
                                         , ("max-width", "500px")
                                         , ("margin", "0 auto")
                                         ]

            void $ element gameSection #+ [element saldoDisplay, element roletaContainer]

            -- Efeitos sonoros
            audioMusica <- UI.audio
              # set UI.src "static/Roleta/musica.mp3"
              # set (UI.attr "autoplay") "true"
              # set (UI.attr "loop") "true"
              # set (UI.attr "preload") "auto"
              # set (UI.attr "controls") "false"
              # set (UI.attr "id") "audioMusicaRoleta"
              # set UI.style [("display", "none")]

            audioClique <- UI.audio
              # set UI.src "static/Roleta/clique.mp3"
              # set (UI.attr "preload") "auto"
              # set (UI.attr "id") "audioClique"
              # set UI.style [("display", "none")]

            audioGirar <- UI.audio
              # set UI.src "static/Roleta/girar.mp3"
              # set (UI.attr "preload") "auto"
              # set (UI.attr "id") "audioGirar"
              # set UI.style [("display", "none")]

            audioVitoria <- UI.audio
              # set UI.src "static/Roleta/vitoria.mp3"
              # set (UI.attr "preload") "auto"
              # set (UI.attr "id") "audioVitoria"
              # set UI.style [("display", "none")]

            audioDerrota <- UI.audio
              # set UI.src "static/Roleta/derrota.mp3"
              # set (UI.attr "preload") "auto"
              # set (UI.attr "id") "audioDerrota"
              # set UI.style [("display", "none")]

            void $ element contentWrapper #+ [element headerSection, element gameSection]

            let atualizarBotoesEscolha cor = do
                  -- Reset todos os botões
                  void $ element btnVermelho # set UI.style [("opacity", "0.7"), ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")]
                  void $ element btnPreto # set UI.style [("opacity", "0.7"), ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")]
                  void $ element btnVerde # set UI.style [("opacity", "0.7"), ("box-shadow", "0 4px 8px rgba(0,0,0,0.3)")]
                  
                  case cor of
                    Logica.Vermelho -> void $ element btnVermelho # set UI.style 
                        [("opacity", "1"), ("box-shadow", "0 0 20px rgba(255, 68, 68, 0.8), 0 4px 8px rgba(0,0,0,0.3)")]
                    Logica.Preto -> void $ element btnPreto # set UI.style 
                        [("opacity", "1"), ("box-shadow", "0 0 20px rgba(51, 51, 51, 0.8), 0 4px 8px rgba(0,0,0,0.3)")]
                    Logica.Verde -> void $ element btnVerde # set UI.style 
                        [("opacity", "1"), ("box-shadow", "0 0 20px rgba(68, 255, 68, 0.8), 0 4px 8px rgba(0,0,0,0.3)")]
                  
                  void $ element btnGirar # set UI.style
                      [ ("background", "linear-gradient(145deg, #ffd700, #b8860b)")
                      , ("color", "#222")
                      , ("cursor", "pointer")
                      , ("border", "2px solid #ffd700")
                      , ("box-shadow", "0 0 20px rgba(255, 215, 0, 0.5), 0 6px 12px rgba(0,0,0,0.3)")
                      ]

            void $ on UI.click btnVermelho $ \_ -> do
                runFunction $ ffi "tocarSom('Clique')"
                runFunction $ ffi "animarBotao('btnVermelho')"
                liftIO $ writeIORef corEscolhida (Just Logica.Vermelho)
                atualizarBotoesEscolha Logica.Vermelho
                void $ element resultado # set UI.text "Cor escolhida: VERMELHO! Clique em GIRAR!"
                                        # set UI.style [("color", "#ff6666")]
                runFunction $ ffi "animarResultado('normal')"

            void $ on UI.click btnPreto $ \_ -> do
                runFunction $ ffi "tocarSom('Clique')"
                runFunction $ ffi "animarBotao('btnPreto')"
                liftIO $ writeIORef corEscolhida (Just Logica.Preto)
                atualizarBotoesEscolha Logica.Preto
                void $ element resultado # set UI.text "Cor escolhida: PRETO! Clique em GIRAR!"
                                        # set UI.style [("color", "#cccccc")]
                runFunction $ ffi "animarResultado('normal')"

            void $ on UI.click btnVerde $ \_ -> do
                runFunction $ ffi "tocarSom('Clique')"
                runFunction $ ffi "animarBotao('btnVerde')"
                liftIO $ writeIORef corEscolhida (Just Logica.Verde)
                atualizarBotoesEscolha Logica.Verde
                void $ element resultado # set UI.text "Cor escolhida: VERDE! Clique em GIRAR!"
                                        # set UI.style [("color", "#66ff66")]
                runFunction $ ffi "animarResultado('normal')"

            void $ on UI.click btnMuteMusic $ \_ -> do
                runFunction $ ffi "tocarSom('Clique')"
                runFunction $ ffi "animarBotao('btnMuteMusic')"
                isMuted <- liftIO $ readIORef musicMuted
                if not isMuted
                  then do
                    liftIO $ writeIORef musicMuted True
                    void $ element btnMuteMusic # set UI.text "🔇"
                    void $ element btnMuteMusic # set UI.style
                        [ ("background", "linear-gradient(145deg, #5a2a2a, #4a1a1a)")
                        , ("color", "#ff9090")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusicaRoleta').muted = true"
                  else do
                    liftIO $ writeIORef musicMuted False
                    void $ element btnMuteMusic # set UI.text "🔊"
                    void $ element btnMuteMusic # set UI.style
                        [ ("background", "linear-gradient(145deg, #2a5a2a, #1a4a1a)")
                        , ("color", "#90ee90")
                        ]
                    runFunction $ ffi "document.getElementById('audioMusicaRoleta').muted = false"

            -- voltar ao menu
            void $ on UI.click btnVoltarMenu $ \_ -> do
                runFunction $ ffi "tocarSom('Clique')"
                runFunction $ ffi "animarBotao('btnVoltarMenu')"
                voltarAoMenu

            void $ on UI.click btnGirar $ \_ -> do
                maybeCor <- liftIO $ readIORef corEscolhida
                case maybeCor of
                  Nothing -> do
                    void $ element resultado # set UI.text "Escolha uma cor primeiro!"
                                           # set UI.style [("color", "#ff6b6b")]
                    runFunction $ ffi "animarResultado('derrota')"
                  Just corApostada -> do
                    saldoAtual <- liftIO $ Logica.mostraSaldoRoleta playerId
                    if saldoAtual < Logica.valorApostaRoleta
                      then do
                        void $ element resultado # set UI.text "💸 Saldo insuficiente!" 
                                               # set UI.style [("color", "#ff6b6b")]
                        runFunction $ ffi "tocarSom('Derrota')"
                        runFunction $ ffi "animarResultado('derrota')"
                      else do
                        runFunction $ ffi "desabilitarBotoesEscolha()"
                        
                        void $ element btnGirar # set UI.style
                            [ ("background", "linear-gradient(145deg, #666666, #444444)")
                            , ("color", "#999999")
                            , ("cursor", "not-allowed")
                            , ("border", "2px solid #666666")
                            , ("opacity", "0.6")
                            , ("pointer-events", "none")
                            ]
                        
                        corSorteada <- liftIO $ Logica.girarRoleta
                        let vitoria = Logica.verificarVitoria corApostada corSorteada
                        
                        if vitoria
                          then do
                            let multiplicador = Logica.obterMultiplicadorRoleta corApostada
                            let ganho = multiplicador * Logica.valorApostaRoleta
                            liftIO $ registrarJogada playerId "Roleta" (round Logica.valorApostaRoleta) ganho
                          else liftIO $ registrarJogada playerId "Roleta" (round Logica.valorApostaRoleta) 0
                        
                        runFunction $ ffi "tocarSom('Girar')"
                        -- Correção: aguardar um pouco antes de chamar a animação
                        runFunction $ ffi $ "setTimeout(() => girarRoletaParaCor('" ++ show corSorteada ++ "'), 100);"
                        runFunction $ ffi "animarBotao('btnGirar')"
                        
                        -- "girando..."
                        void $ element resultado # set UI.text "🎰 Girando a roleta..."
                                                # set UI.style [("color", "#ffd700")]
                        
                        let corTexto = case corSorteada of
                              Logica.Vermelho -> "🔴"
                              Logica.Preto -> "⚫"
                              Logica.Verde -> "🟢"
                        
                        let scriptAtualizarCentro = 
                                "setTimeout(function() {" ++
                                "var roletaResult = document.getElementById('roletaResultDisplay');" ++
                                "if (roletaResult) {" ++
                                "roletaResult.textContent = '" ++ corTexto ++ "';" ++
                                "roletaResult.style.background = '" ++ corParaCSS corSorteada ++ "';" ++
                                "}}, 2100);"
                        runFunction $ ffi scriptAtualizarCentro
                        
                        -- Mostrar resultado 
                        let scriptMostrarResultado = 
                                "setTimeout(function() {" ++
                                "var display = document.getElementById('resultadoTexto');" ++
                                "if (display) {" ++
                                (if vitoria
                                  then let multiplicador = Logica.obterMultiplicadorRoleta corApostada
                                           ganho = multiplicador * Logica.valorApostaRoleta
                                       in "display.textContent = '🎉 GANHOU! R$ " ++ show ganho ++ "!';" ++
                                          "display.style.color = '#90ee90';" ++
                                          "tocarSom('Vitoria');" ++
                                          "animarResultado('vitoria');"
                                  else "display.textContent = '😔 Saiu " ++ show corSorteada ++ "! Tente novamente!';" ++
                                       "display.style.color = '#ff9090';" ++
                                       "tocarSom('Derrota');" ++
                                       "animarResultado('derrota');") ++
                                "}}, 2500);"
                        runFunction $ ffi scriptMostrarResultado
                        
                        -- Atualizar saldo na UI
                        void $ atualizarSaldo saldoDisplay playerId
                        novoSaldo <- liftIO $ Logica.mostraSaldoRoleta playerId
                        void $ element pSaldo # set UI.text ("💰 $" ++ show novoSaldo)
                        
                        let scriptReset = 
                                "setTimeout(function() {" ++
                                "var btnGirar = document.getElementById('btnGirar');" ++
                                "if (btnGirar) {" ++
                                "btnGirar.style.background = 'linear-gradient(145deg, #666666, #444444)';" ++
                                "btnGirar.style.color = '#999999';" ++
                                "btnGirar.style.cursor = 'not-allowed';" ++
                                "btnGirar.style.border = '2px solid #666666';" ++
                                "btnGirar.style.boxShadow = '0 6px 12px rgba(0,0,0,0.3)';" ++
                                "btnGirar.style.pointerEvents = 'auto';" ++
                                "}" ++
                                "var display = document.getElementById('resultadoTexto');" ++
                                "if (display) {" ++
                                "display.textContent = 'Escolha uma cor para apostar!';" ++
                                "display.style.color = '#ffd700';" ++
                                "}" ++
                                "reabilitarBotoesEscolha();" ++
                                "}, 4000);"
                        runFunction $ ffi scriptReset
                        
                        -- Reset escolha
                        liftIO $ writeIORef corEscolhida Nothing

            void $ element body #+ [ element navBarElem
                                   , element contentWrapper
                                   , element audioMusica
                                   , element audioClique
                                   , element audioGirar
                                   , element audioVitoria
                                   , element audioDerrota
                                   ]
            
        Nothing -> do
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
                              , ("box-shadow", "0 8px 32px rgba(0,0,0,0.4)")
                              ]
            
            void $ element body #+ [element erro]