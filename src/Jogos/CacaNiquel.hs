-- src/Jogos/CacaNiquel.hs
module Jogos.CacaNiquel (jogarCacaNiquel) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import EstadoGlobal

simbolos :: [String]
simbolos = ["🐯", "🍒", "💰", "🍋", "⭐"]

valorAposta :: Int
valorAposta = 10

girar :: IO [String]
girar = replicateM 3 $ do
  idx <- randomRIO (0, length simbolos - 1)
  return (simbolos !! idx)

verificarVitoria :: [String] -> Bool
verificarVitoria [x, y, z] = x == y && y == z 
verificarVitoria _ = False

jogarCacaNiquel :: PlayerID -> IO ()
jogarCacaNiquel pid = do
    dados <- getGlobalData
    let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)

    case maybeJogador of
      [] -> putStrLn "Jogador não encontrado."
      (jogador:_) -> do
        if saldo jogador < fromIntegral valorAposta
          then putStrLn "💸 Saldo insuficiente para apostar!"
          else do
            putStrLn "🎰 Girando os rolos..."
            resultado <- girar
            putStrLn $ "Resultado: " ++ unwords resultado

            let ganho = if verificarVitoria resultado then 50.0 else 0.0
            registrarJogada pid "Caça-níquel" valorAposta ganho

            if ganho > 0
              then putStrLn "🎉 Você ganhou 50 moedas!"
              else putStrLn "😞 Tente novamente!"
