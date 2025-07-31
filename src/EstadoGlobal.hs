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
    getGlobalData,
    updateGlobalData,
    adicionarJogador,
    atualizarStats,
    adicionarSaldo,
    removerSaldo,
    mostrarSaldo,
    mostrarRanking,
    listarJogadores,
    registrarJogada
) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Exception (try, SomeException)

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
    proximoID   :: PlayerID,
    configs     :: Configuracoes
} deriving (Show, Generic)

instance ToJSON DadosGlobais
instance FromJSON DadosGlobais

-- Caminho do arquivo de persistência ajustado para a pasta 'data'
arquivoDados :: FilePath
arquivoDados = "data/dados_jogo.json"

-- Variável global em memória
{-# NOINLINE globalData #-}
globalData :: IORef DadosGlobais
globalData = unsafePerformIO $ do
    exists <- doesFileExist arquivoDados
    if exists then
        carregarDados >>= newIORef
    else
        newIORef DadosGlobais {
            jogadores = [],
            proximoID = 1,
            configs = Configuracoes {
                saldoInicial = 100,
                apostaMinima = 5
            }
        }

-- ========== FUNÇÕES DE PERSISTÊNCIA ========== --

carregarDados :: IO DadosGlobais
carregarDados = do
    conteudo <- B.readFile arquivoDados
    case decode conteudo of
        Just dados -> return dados
        Nothing -> error "Erro na leitura dos dados"

salvarDados :: IO ()
salvarDados = do
    dados <- readIORef globalData
    B.writeFile arquivoDados (encode dados)

salvarDadosSeguro :: IO ()
salvarDadosSeguro = do
    result <- try salvarDados :: IO (Either SomeException ())
    case result of
        Left err -> putStrLn $ "Erro ao salvar: " ++ show err
        Right _ -> putStrLn "Dados salvos com sucesso!"

-- ========== FUNÇÕES DE GERENCIAMENTO ========== --

getGlobalData :: IO DadosGlobais
getGlobalData = readIORef globalData

updateGlobalData :: DadosGlobais -> IO ()
updateGlobalData novosDados = do
    writeIORef globalData novosDados
    salvarDadosSeguro

adicionarJogador :: String -> IO PlayerID
adicionarJogador nomeJogador = do
    dados <- getGlobalData
    let novoID = proximoID dados
    let saldoInicialFloat = fromIntegral (saldoInicial $ configs dados)
    let novoJogador = Jogador {
            playerID = novoID,
            nome = nomeJogador,
            totalApostas = 0,
            totalGanho = 0,
            historico = [],
            saldo = saldoInicialFloat
        }
    updateGlobalData dados {
        jogadores = novoJogador : jogadores dados,
        proximoID = novoID + 1
    }
    return novoID

atualizarStats :: PlayerID -> String -> Int -> Float -> IO ()
atualizarStats pid jogo valor ganho = do
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j {
                totalApostas = totalApostas j + 1,
                totalGanho   = totalGanho j + ganho,
                historico    = (jogo, valor) : historico j,
                saldo        = max 0 (saldo j - fromIntegral valor + ganho)
            }
            else j) (jogadores dados)
    updateGlobalData dados { jogadores = jogadoresAtualizados }

-- Adiciona saldo ao jogador
adicionarSaldo :: PlayerID -> Float -> IO ()
adicionarSaldo pid valor = do
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = saldo j + valor }
            else j) (jogadores dados)
    updateGlobalData dados { jogadores = jogadoresAtualizados }
    putStrLn $ "Saldo de " ++ show valor ++ " adicionado ao jogador " ++ show pid

-- Remove saldo do jogador
removerSaldo :: PlayerID -> Float -> IO ()
removerSaldo pid valor = do
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = max 0 (saldo j - valor) }
            else j) (jogadores dados)
    updateGlobalData dados { jogadores = jogadoresAtualizados }
    putStrLn $ "Saldo de " ++ show valor ++ " removido do jogador " ++ show pid

-- Mostrar saldo de um jogador
mostrarSaldo :: PlayerID -> IO ()
mostrarSaldo pid = do
    dados <- getGlobalData
    case filter (\j -> playerID j == pid) (jogadores dados) of
        [] -> putStrLn "Jogador não encontrado."
        (j:_) -> putStrLn $ "Saldo atual do jogador " ++ nome j ++ ": R$ " ++ show (saldo j)

-- Mostrar ranking de jogadores
mostrarRanking :: IO ()
mostrarRanking = do
    dados <- getGlobalData
    let ranking = sortBy (comparing (negate . totalGanho)) (jogadores dados)
    putStrLn "Ranking de jogadores por total ganho:"
    mapM_ (\j -> putStrLn $ nome j ++ ": R$ " ++ show (totalGanho j)) ranking

-- Listar jogadores (ID e nome)
listarJogadores :: IO ()
listarJogadores = do
    dados <- getGlobalData
    putStrLn "Jogadores cadastrados:"
    mapM_ (\j -> putStrLn $ show (playerID j) ++ ": " ++ nome j) (jogadores dados)

-- ========== INTERFACE PARA OS JOGOS ========== --

registrarJogada :: PlayerID -> String -> Int -> Float -> IO ()
registrarJogada pid jogo valor ganho = do
    atualizarStats pid jogo valor ganho
    dados <- getGlobalData
    case filter (\j -> playerID j == pid) (jogadores dados) of
        [] -> putStrLn "Jogador não encontrado."
        (j:_) -> do
            let performance = if totalApostas j > 0
                              then totalGanho j / fromIntegral (totalApostas j)
                              else 0
            putStrLn $ "Estatísticas atualizadas - Performance média: " ++ show performance ++ "%"
            putStrLn $ "Novo saldo: R$ " ++ show (saldo j)
            putStrLn $ "Histórico: " ++ show (historico j)