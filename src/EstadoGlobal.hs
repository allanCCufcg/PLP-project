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
    registrarJogada,
    resetarDados,
    buscarJogadorPorID
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
import System.Random (randomRIO)

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
    updateGlobalData dados {
        jogadores = novoJogador : jogadores dados
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

-- Nova função para buscar jogador pelo ID
buscarJogadorPorID :: PlayerID -> IO (Maybe Jogador)
buscarJogadorPorID pid = do
    dados <- getGlobalData
    return $ case filter (\j -> playerID j == pid) (jogadores dados) of
        []    -> Nothing
        (j:_) -> Just j

adicionarSaldo :: PlayerID -> Float -> IO ()
adicionarSaldo pid valor = do
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = saldo j + valor }
            else j) (jogadores dados)
    updateGlobalData dados { jogadores = jogadoresAtualizados }
    putStrLn $ "Saldo de " ++ show valor ++ " adicionado ao jogador " ++ show pid

removerSaldo :: PlayerID -> Float -> IO ()
removerSaldo pid valor = do
    dados <- getGlobalData
    let jogadoresAtualizados = map (\j ->
            if playerID j == pid
            then j { saldo = max 0 (saldo j - valor) }
            else j) (jogadores dados)
    updateGlobalData dados { jogadores = jogadoresAtualizados }
    putStrLn $ "Saldo de " ++ show valor ++ " removido do jogador " ++ show pid

mostrarSaldo :: PlayerID -> IO ()
mostrarSaldo pid = do
    dados <- getGlobalData
    case filter (\j -> playerID j == pid) (jogadores dados) of
        [] -> putStrLn "Jogador não encontrado."
        (j:_) -> putStrLn $ "Saldo atual do jogador " ++ nome j ++ ": R$ " ++ show (saldo j)

mostrarRanking :: IO ()
mostrarRanking = do
    dados <- getGlobalData
    let ranking = sortBy (comparing (negate . totalGanho)) (jogadores dados)
    putStrLn "Ranking de jogadores por total ganho:"
    mapM_ (\j -> putStrLn $ nome j ++ ": R$ " ++ show (totalGanho j)) ranking

listarJogadores :: IO ()
listarJogadores = do
    dados <- getGlobalData
    putStrLn "Jogadores cadastrados:"
    mapM_ (\j -> putStrLn $ show (playerID j) ++ ": " ++ nome j) (jogadores dados)

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

resetarDados :: IO ()
resetarDados = do
    let dadosIniciais = DadosGlobais {
            jogadores = [],
            configs = Configuracoes {
                saldoInicial = 100,
                apostaMinima = 5
            }
        }
    writeIORef globalData dadosIniciais
    B.writeFile arquivoDados (encode dadosIniciais)
    putStrLn "Dados resetados para o estado inicial!"
