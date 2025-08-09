{-# LANGUAGE DeriveGeneric #-}

module EstadoGlobal (
    PlayerID,
    Porcentagem,
    Jogador(..),
    Configuracoes(..),
    DadosGlobais(..),
    arquivoDados,
    globalData,
    carregarDados,
    salvarDados,
    salvarDadosSeguro,
    inicializarSistema,
    getGlobalData,
    updateGlobalData,
    adicionarJogador,
    atualizarStats,
    adicionarSaldo,
    removerSaldo,
    mostrarSaldo,
    mostrarRanking,
    obterRankingTop10,
    listarJogadores,
    registrarJogada,
    resetarDados,
    buscarJogadorPorID
) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Exception (try, SomeException)
import System.Random (randomRIO)
-- import System.FilePath (takeDirectory) -- Removido para evitar dependência

-- Tipos de dados
type PlayerID = Int
type Porcentagem = Float

data Jogador = Jogador {
    playerID     :: PlayerID,
    nome         :: String,
    totalApostas :: Int,
    totalGanho   :: Float,
    historico    :: [(String, Int)],
    saldo        :: Float
} deriving (Show, Generic)

instance ToJSON Jogador
instance FromJSON Jogador

data Configuracoes = Configuracoes {
    saldoInicial :: Int,
    apostaMinima :: Int
} deriving (Show, Generic)

instance ToJSON Configuracoes
instance FromJSON Configuracoes

data DadosGlobais = DadosGlobais {
    jogadores   :: [Jogador],
    configs     :: Configuracoes
} deriving (Show, Generic)

instance ToJSON DadosGlobais
instance FromJSON DadosGlobais

-- Caminho do arquivo de persistência
arquivoDados :: FilePath
arquivoDados = "data/dados_jogo.json"

-- Diretório onde ficam os dados
diretorioDados :: FilePath
diretorioDados = "data"

-- Dados padrão para inicialização (apenas se arquivo não existir)
dadosIniciais :: DadosGlobais
dadosIniciais = DadosGlobais {
    jogadores = [],
    configs = Configuracoes {
        saldoInicial = 100,
        apostaMinima = 5
    }
}

-- CHAVE DA SOLUÇÃO: Sempre carregar do arquivo se existir
{-# NOINLINE globalData #-}
globalData :: IORef DadosGlobais
globalData = unsafePerformIO $ do
    putStrLn "[INIT] Inicializando estado global..."
    
    -- Cria diretório se necessário
    createDirectoryIfMissing True diretorioDados
    
    -- Verifica se arquivo existe
    exists <- doesFileExist arquivoDados
    if exists then do
        putStrLn "[LOAD] Arquivo JSON encontrado, carregando dados existentes..."
        resultado <- try (B.readFile arquivoDados) :: IO (Either SomeException B.ByteString)
        case resultado of
            Left err -> do
                putStrLn $ "[ERROR] Erro ao ler arquivo: " ++ show err
                putStrLn "[NEW] Criando com dados padrão..."
                newIORef dadosIniciais
            Right conteudo -> 
                case decode conteudo of
                    Just dados -> do
                        putStrLn $ "[OK] Dados carregados: " ++ show (length $ jogadores dados) ++ " jogadores encontrados"
                        putStrLn "[READY] Sistema pronto com dados existentes!"
                        newIORef dados
                    Nothing -> do
                        putStrLn "[WARN] JSON corrompido, criando backup e usando dados padrão..."
                        -- Fazer backup do arquivo corrompido
                        _ <- try (B.writeFile (arquivoDados ++ ".backup") conteudo) :: IO (Either SomeException ())
                        newIORef dadosIniciais
    else do
        putStrLn "[NEW] Arquivo não existe, criando novo..."
        let ref = newIORef dadosIniciais
        -- Salva imediatamente o arquivo inicial
        B.writeFile arquivoDados (encode dadosIniciais)
        putStrLn "[SAVE] Arquivo inicial criado"
        ref

-- ========== FUNÇÕES DE PERSISTÊNCIA ========== --

-- Função de inicialização (opcional, mas recomendada)
inicializarSistema :: IO ()
inicializarSistema = do
    putStrLn "[CASINO] Sistema El Cassino inicializado!"
    dados <- readIORef globalData
    putStrLn $ "[PLAYERS] Jogadores ativos: " ++ show (length $ jogadores dados)
    return ()

carregarDados :: IO DadosGlobais
carregarDados = do
    conteudo <- B.readFile arquivoDados
    case decode conteudo of
        Just dados -> return dados
        Nothing -> do
            putStrLn "[ERROR] Erro na decodificação, usando dados padrão"
            return dadosIniciais

-- Salva dados de forma segura (SEMPRE salva no arquivo)
salvarDados :: IO ()
salvarDados = do
    dados <- readIORef globalData
    createDirectoryIfMissing True diretorioDados
    B.writeFile arquivoDados (encode dados)

salvarDadosSeguro :: IO ()
salvarDadosSeguro = do
    result <- try salvarDados :: IO (Either SomeException ())
    case result of
        Left err -> do
            putStrLn $ "[ERROR] ERRO CRÍTICO ao salvar: " ++ show err
            putStrLn "[WARN] Os dados podem ser perdidos!"
        Right _ -> do
            dados <- readIORef globalData
            putStrLn $ "[SAVE] Dados persistidos: " ++ show (length $ jogadores dados) ++ " jogadores"

-- ========== FUNÇÕES DE GERENCIAMENTO ========== --

getGlobalData :: IO DadosGlobais
getGlobalData = readIORef globalData

updateGlobalData :: DadosGlobais -> IO ()
updateGlobalData novosDados = do
    putStrLn "[UPDATE] Atualizando dados globais..."
    writeIORef globalData novosDados
    salvarDadosSeguro -- CRÍTICO: SEMPRE salva após qualquer mudança

-- Gera um PlayerID aleatório único (4 dígitos)
gerarIDUnico :: IO PlayerID
gerarIDUnico = do
    dados <- getGlobalData
    let idsExistentes = map playerID (jogadores dados)
    novoID <- randomRIO (1000, 9999)
    if novoID `elem` idsExistentes
        then gerarIDUnico
        else return novoID

-- Adiciona novo jogador com ID aleatório
adicionarJogador :: String -> IO PlayerID
adicionarJogador nomeJogador = do
    putStrLn $ "[NEW PLAYER] Criando jogador: " ++ nomeJogador
    dados <- getGlobalData
    novoID <- gerarIDUnico
    let saldoInicialFloat = fromIntegral (saldoInicial $ configs dados)
    let novoJogador = Jogador {
            playerID = novoID,
            nome = nomeJogador,
            totalApostas = 0,
            totalGanho = 0,
            historico = [],
            saldo = saldoInicialFloat
        }
    let dadosAtualizados = dados { jogadores = novoJogador : jogadores dados }
    updateGlobalData dadosAtualizados
    putStrLn $ "[OK] Jogador criado: " ++ nomeJogador ++ " (ID: " ++ show novoID ++ ")"
    return novoID

atualizarStats :: PlayerID -> String -> Int -> Float -> IO ()
atualizarStats pid jogo valor ganho = do
    putStrLn $ "[STATS] Atualizando stats do jogador " ++ show pid
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j {
                totalApostas = totalApostas j + 1,
                totalGanho   = totalGanho j + ganho,
                historico    = (jogo, valor) : take 19 (historico j),
                saldo        = max 0 (saldo j - fromIntegral valor + ganho)
            }
            else j) (jogadores dados)
    let dadosAtualizados = dados { jogadores = jogadoresAtualizados }
    updateGlobalData dadosAtualizados

-- Buscar jogador pelo ID
buscarJogadorPorID :: PlayerID -> IO (Maybe Jogador)
buscarJogadorPorID pid = do
    dados <- getGlobalData
    return $ case filter (\j -> playerID j == pid) (jogadores dados) of
        []    -> Nothing
        (j:_) -> Just j

-- Obter ranking dos 10 melhores jogadores
obterRankingTop10 :: IO [Jogador]
obterRankingTop10 = do
    putStrLn "[RANKING] Gerando ranking Top 10..."
    dados <- getGlobalData
    let ranking = take 10 $ sortBy (comparing (negate . totalGanho)) (jogadores dados)
    putStrLn $ "[TOP10] Ranking: " ++ show (length ranking) ++ " jogadores no top"
    return ranking

adicionarSaldo :: PlayerID -> Float -> IO ()
adicionarSaldo pid valor = do
    putStrLn $ "[MONEY] Adicionando R$ " ++ show valor ++ " ao jogador " ++ show pid
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = saldo j + valor }
            else j) (jogadores dados)
    let dadosAtualizados = dados { jogadores = jogadoresAtualizados }
    updateGlobalData dadosAtualizados

removerSaldo :: PlayerID -> Float -> IO ()
removerSaldo pid valor = do
    putStrLn $ "[MONEY] Removendo R$ " ++ show valor ++ " do jogador " ++ show pid
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = max 0 (saldo j - valor) }
            else j) (jogadores dados)
    let dadosAtualizados = dados { jogadores = jogadoresAtualizados }
    updateGlobalData dadosAtualizados

mostrarSaldo :: PlayerID -> IO ()
mostrarSaldo pid = do
    dados <- getGlobalData
    case filter (\j -> playerID j == pid) (jogadores dados) of
        [] -> putStrLn "[ERROR] Jogador não encontrado."
        (j:_) -> putStrLn $ "[BALANCE] Saldo do jogador " ++ nome j ++ ": R$ " ++ show (saldo j)

mostrarRanking :: IO ()
mostrarRanking = do
    dados <- getGlobalData
    let ranking = sortBy (comparing (negate . totalGanho)) (jogadores dados)
    putStrLn "[RANKING] === RANKING GERAL ==="
    mapM_ (\(pos, j) -> putStrLn $ show pos ++ "º " ++ nome j ++ ": R$ " ++ show (totalGanho j)) 
          (zip [1..] ranking)

listarJogadores :: IO ()
listarJogadores = do
    dados <- getGlobalData
    putStrLn "[PLAYERS] === JOGADORES CADASTRADOS ==="
    mapM_ (\j -> putStrLn $ "ID " ++ show (playerID j) ++ ": " ++ nome j ++ " (Saldo: R$ " ++ show (saldo j) ++ ")") 
          (jogadores dados)

registrarJogada :: PlayerID -> String -> Int -> Float -> IO ()
registrarJogada pid jogo valor ganho = do
    putStrLn $ "[GAME] Registrando jogada: " ++ jogo ++ " - Jogador " ++ show pid
    atualizarStats pid jogo valor ganho

resetarDados :: IO ()
resetarDados = do
    putStrLn "[RESET] RESETANDO TODOS OS DADOS..."
    --writeIORef globalData dadosIniciais
    salvarDadosSeguro
    putStrLn "[RESET] Dados resetados para o estado inicial!"