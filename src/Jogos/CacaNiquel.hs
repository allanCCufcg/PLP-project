-- src/Jogos/CacaNiquel.hs
module Jogos.CacaNiquel
  ( jogarCacaNiquel
  , girar
  , verificarVitoria
  , multiplicadores
  , valorAposta
  , mostraSaldo
  ) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import EstadoGlobal
import qualified Data.Map as Map
import Data.Map (Map)

type Simbolo = String

-- Mapa de multiplicadores por símbolo
multiplicadores :: Map Simbolo Float
multiplicadores = Map.fromList
  [ ("TIGRE", 250)
  , ("CEREJA", 100)
  , ("OURO", 10)
  , ("LIMAO", 8)
  , ("FLOR", 5)
  , ("ESTRELA", 3)
  ]

-- Símbolos com probabilidade controlada para balancear chances (soma 100)
-- Exemplo: "ESTRELA" tem 22% de chance, "TIGRE" 13%, etc.
-- A lista é construída replicando os símbolos conforme a frequência desejada.
simbolosComPeso :: [Simbolo]
simbolosComPeso =
  replicate 13 "TIGRE" ++
  replicate 13 "CEREJA" ++
  replicate 15 "OURO" ++
  replicate 16 "LIMAO" ++
  replicate 21 "FLOR" ++
  replicate 22 "ESTRELA"

valorAposta :: Float
valorAposta = 10

-- Sorteia um símbolo com base na lista com peso
sortearSimbolo :: IO Simbolo
sortearSimbolo = do
  idx <- randomRIO (0, length simbolosComPeso - 1)
  return (simbolosComPeso !! idx)

-- Gira os 3 rolos
girar :: IO [Simbolo]
girar = replicateM 3 sortearSimbolo

-- Verifica se houve vitória (3 símbolos iguais)
verificarVitoria :: [Simbolo] -> Bool
verificarVitoria [x,y,z] = x == y && y == z
verificarVitoria _ = False

jogarCacaNiquel :: PlayerID -> IO ()
jogarCacaNiquel pid = do
  dados <- getGlobalData
  let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
  case maybeJogador of
    [] -> putStrLn "Jogador nao encontrado."
    (jogador:_) -> do
      if saldo jogador < valorAposta
        then putStrLn "Saldo insuficiente para apostar!"
        else do
          putStrLn "Girando os rolos..."
          resultado <- girar
          putStrLn $ "Resultado: " ++ unwords resultado
          if verificarVitoria resultado
            then do
              let simbolo = head resultado
              let multiplicador = Map.findWithDefault 0 simbolo multiplicadores
              let ganho = multiplicador * valorAposta
              registrarJogada pid "Caca-Niquel" (round valorAposta) ganho
              putStrLn $ "Parabens! Voce ganhou " ++ show ganho ++ " com tres " ++ simbolo ++ "!"
            else do
              registrarJogada pid "Caca-Niquel" (round valorAposta) 0
              putStrLn "Que pena! Voce nao ganhou desta vez."

mostraSaldo :: PlayerID -> IO Float
mostraSaldo pid = do
    dados <- getGlobalData
    let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
    case maybeJogador of
        (j:_) -> return (saldo j)
        []    -> return 0
