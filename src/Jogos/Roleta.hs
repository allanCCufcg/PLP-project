-- src/Jogos/Roleta.hs
module Jogos.Roleta
  ( jogarRoleta
  , girarRoleta
  , verificarVitoria
  , multiplicadoresRoleta
  , valorApostaRoleta
  , mostraSaldoRoleta
  , obterMultiplicadorRoleta
  , CorRoleta(..)
  ) where

import System.Random (randomRIO)
import EstadoGlobal
import qualified Data.Map as Map
import Data.Map (Map)

-- Tipo de dados para as cores da roleta
data CorRoleta = Vermelho | Preto | Verde deriving (Show, Eq, Ord)

multiplicadoresRoleta :: Map CorRoleta Float
multiplicadoresRoleta = Map.fromList
  [ (Vermelho, 2.0)
  , (Preto, 2.0)
  , (Verde, 15.0)
  ]

valorApostaRoleta :: Float
valorApostaRoleta = 10

coresComPeso :: [CorRoleta]
coresComPeso =
  replicate 47 Vermelho ++
  replicate 47 Preto ++
  replicate 6 Verde

girarRoleta :: IO CorRoleta
girarRoleta = do
  idx <- randomRIO (0, length coresComPeso - 1)
  return (coresComPeso !! idx)

verificarVitoria :: CorRoleta -> CorRoleta -> Bool
verificarVitoria corEscolhida corSorteada = corEscolhida == corSorteada

obterMultiplicadorRoleta :: CorRoleta -> Float
obterMultiplicadorRoleta cor = Map.findWithDefault 0 cor multiplicadoresRoleta

jogarRoleta :: PlayerID -> IO ()
jogarRoleta pid = do
  dados <- getGlobalData
  let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
  case maybeJogador of
    [] -> putStrLn "Jogador nao encontrado."
    (jogador:_) -> do
      if saldo jogador < valorApostaRoleta
        then putStrLn "Saldo insuficiente para apostar!"
        else putStrLn "Use a interface grafica para jogar roleta!"

-- Mostra saldo específico para roleta (usando função do estado global)
mostraSaldoRoleta :: PlayerID -> IO Float
mostraSaldoRoleta pid = do
    dados <- getGlobalData
    let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
    case maybeJogador of
        (j:_) -> return (saldo j)
        []    -> return 0