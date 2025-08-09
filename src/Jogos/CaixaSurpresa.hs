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

abrirCaixa :: PlayerID -> IO (Int, Float)
abrirCaixa pid = do
    dados <- getGlobalData
    let jogadoresAtivos = jogadores dados
        jogadorEncontrado = filter (\j -> playerID j == pid) jogadoresAtivos
    case jogadorEncontrado of
        [] -> do
            putStrLn "Jogador não encontrado."
            return (0, 0)
        (j:_) -> do
            let saldoAtual = saldo j       
                custo :: Float
                custo = 20.0
            if saldoAtual < custo
                then do
                    putStrLn "Saldo insuficiente para abrir a caixa (custa R$ 20)."
                    return (0, saldoAtual)
                else do
                    premio <- sortearPremio  
                    registrarJogada pid "Caixa Surpresa" (round custo) (fromIntegral premio)
                    let novoSaldo = saldoAtual - custo + fromIntegral premio
                    putStrLn $ "Você recebeu R$ " ++ show premio ++ " da caixa surpresa!"
                    return (premio, novoSaldo)
