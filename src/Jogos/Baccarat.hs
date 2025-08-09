module Jogos.Baccarat
  ( jogarBaccarat
  , calcularVencedor
  , multiplicadorPremio
  , custoBaccarat
  , Aposta(..)
  ) where

import System.Random (randomRIO)
import EstadoGlobal (PlayerID, getGlobalData, jogadores, playerID, saldo, registrarJogada)
import qualified Data.Map as Map


data Aposta = Banco | Player | Empate deriving (Show, Eq, Read)

custoBaccarat :: Float
custoBaccarat = 10

multiplicadorPremio :: Aposta -> Float
multiplicadorPremio Empate = 8.0
multiplicadorPremio _      = 1.0

sorteiaCarta :: IO Int
sorteiaCarta = randomRIO (0, 9)

sortearMaos :: IO ([Int], [Int])
sortearMaos = do
    j1 <- sorteiaCarta
    j2 <- sorteiaCarta
    b1 <- sorteiaCarta
    b2 <- sorteiaCarta
    return ([j1, j2], [b1, b2])

calcularVencedor :: [Int] -> [Int] -> Aposta
calcularVencedor maoJ maoB =
    let pontosJ = sum maoJ `mod` 10
        pontosB = sum maoB `mod` 10
    in case compare pontosJ pontosB of
        GT -> Player
        LT -> Banco
        EQ -> Empate

jogarBaccarat :: PlayerID -> Aposta -> Float -> IO ()
jogarBaccarat pid aposta valorAposta = do
    dados <- getGlobalData
    let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
    case maybeJogador of
      [] -> putStrLn "Jogador nao encontrado."
      (j:_) -> do
        let saldoAtual = saldo j
            custoTotal = custoBaccarat + valorAposta
        if saldoAtual < custoTotal
          then putStrLn "Saldo insuficiente para apostar!"
          else do
            putStrLn "Sorteando cartas..."
            (maoJogador, maoBanco) <- sortearMaos
            let vencedor = calcularVencedor maoJogador maoBanco
                ganhou = aposta == vencedor
                pontosJ = sum maoJogador `mod` 10
                pontosB = sum maoBanco `mod` 10
                premio = if ganhou
                          then multiplicadorPremio aposta * valorAposta
                          else 0
            registrarJogada pid "Baccarat" (round custoTotal) premio
            putStrLn $ "Mao Jogador: " ++ show maoJogador ++ " -> " ++ show pontosJ
            putStrLn $ "Mao Banco:   " ++ show maoBanco ++ " -> " ++ show pontosB
            putStrLn $ "Vencedor: " ++ show vencedor
            putStrLn $ "Sua aposta: " ++ show aposta
            if ganhou
              then putStrLn $ "Parabens! Voce ganhou " ++ show premio ++ " pontos!"
              else putStrLn "Que pena! Voce nao ganhou"
                    


