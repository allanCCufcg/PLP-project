module Jogos.Baccarat
  ( jogarBaccarat
  , calcularVencedor
  , multiplicadorPremio
  , Aposta(..)
  ) where

import System.Random (randomRIO)
import EstadoGlobal (PlayerID, buscarJogadorPorID, registrarJogada, saldo)

data Aposta = Banco | Player | Empate deriving (Show, Eq, Read)


multiplicadorPremio :: Aposta -> Float
multiplicadorPremio Empate = 8.0
multiplicadorPremio Banco  = 1.0
multiplicadorPremio Player = 1.0

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
    maybeJogador <- buscarJogadorPorID pid
    case maybeJogador of
      Nothing -> putStrLn "Jogador não encontrado."
      Just j -> do
        let saldoAtual = saldo j
        
        if valorAposta < 10
          then putStrLn "Aposta mínima: R$ 10!"
          else if valorAposta > saldoAtual
            then putStrLn $ "💸 Aposta máxima: R$ " ++ show saldoAtual
            else do
                putStrLn "Sorteando cartas..."
                (maoJogador, maoBanco) <- sortearMaos
                
                let vencedor = calcularVencedor maoJogador maoBanco
                    ganhou = aposta == vencedor
                    pontosJ = sum maoJogador `mod` 10
                    pontosB = sum maoBanco `mod` 10
                    premio = if ganhou
                              then valorAposta * multiplicadorPremio aposta
                              else 0
                
                registrarJogada pid "Baccarat" (round valorAposta) premio
                
                putStrLn $ "\n=== RESULTADO ==="
                putStrLn $ "Mão Jogador: " ++ show maoJogador ++ " (" ++ show pontosJ ++ " pontos)"
                putStrLn $ "Mão Banco:   " ++ show maoBanco ++ " (" ++ show pontosB ++ " pontos)"
                putStrLn $ "Vencedor: " ++ show vencedor
                putStrLn $ "Sua aposta: " ++ show aposta
                
                if ganhou
                  then putStrLn $ "\n🎉 Parabéns! Você ganhou R$ " ++ show premio ++ "!"
                  else putStrLn "\n😔 Que pena! Você não ganhou."
                
                maybeJogadorAtualizado <- buscarJogadorPorID pid
                case maybeJogadorAtualizado of
                  Just j' -> putStrLn $ "💰 Saldo atualizado: R$ " ++ show (saldo j')
                  Nothing -> return ()
