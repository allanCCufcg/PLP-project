module Jogos.CaixaSurpresa (abrirCaixa) where

import System.Random (randomRIO)
import EstadoGlobal 
  ( PlayerID, Jogador(..), getGlobalData, registrarJogada, jogadores )

type Premio = Int

-- Lista de prêmios com suas respectivas "probabilidades" (ponderação)
premiosPonderados :: [(Premio, Int)]
premiosPonderados = [(0, 20), (5, 25), (10, 25), (25, 15), (50, 10), (75, 8), (100, 5)]

sortearPremio :: IO Premio
sortearPremio = do
    let listaExpandida = concatMap (\(p, w) -> replicate w p) premiosPonderados
    idx <- randomRIO (0, length listaExpandida - 1)
    return (listaExpandida !! idx)

abrirCaixa :: PlayerID -> IO ()
abrirCaixa pid = do
    dados <- getGlobalData
    let jogadoresAtivos = jogadores dados
        jogadorEncontrado = filter (\j -> playerID j == pid) jogadoresAtivos
    case jogadorEncontrado of
        [] -> putStrLn "Jogador não encontrado."
        (j:_) -> do
            let saldoAtual = saldo j
                custo = 20
            if saldoAtual < fromIntegral custo
                then putStrLn "Saldo insuficiente para abrir a caixa (custa R$ 20)."
                else do
                    premio <- sortearPremio
                    registrarJogada pid "Caixa Surpresa" custo (fromIntegral premio)  -- Converta para Float
                    putStrLn $ "Você recebeu R$ " ++ show premio ++ " da caixa surpresa!"