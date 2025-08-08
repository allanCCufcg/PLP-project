import System.Random (randomRIO)
import EstadoGlobal 
  ( PlayerID, Jogador(..), getGlobalData, registrarJogada, jogadores )

-- Tipos de apostas possiveis
data Aposta = Banco | Player | Empate deriving (Show, Eq)

-- Custo para jogar uma rodada de Baccarat
custoBaccarat :: Int
custoBaccarat = 10

multiplicadorPremio :: Aposta -> Float
multiplicadorPremio Empate = 8.0 -- 8x para empate
multiplicadorPremio _      = 2.0  -- 2x para outros (1:1)


sorteiaCarta :: IO Int
sorteiaCarta = randomRIO (0, 9)

sortearMaos :: IO ([Int], [Int])
sortearMaos = do
    j1 <- sorteiaCarta
    j2 <- sorteiaCarta
    b1 <- sorteiaCarta
    b2 <- sorteiaCarta
    return ([j1, j2], [b1, b2])

-- Calcula quem esta mais perto de 9 (soma mod 10)
calcularVencedor :: [Int] -> [Int] -> Aposta
calcularVencedor maoJ maoB =
    let pontosJ = sum maoJ `mod` 10
        pontosB = sum maoB `mod` 10
    in case compare pontosJ pontosB of
        GT -> Player  -- Jogador mais perto de 9
        LT -> Banco   -- Banco mais perto de 9
        EQ -> Empate  -- Empate


jogarBaccarat :: PlayerID -> Aposta -> Int -> IO ()
jogarBaccarat pid aposta valorAposta = do
    dados <- getGlobalData
    let jogadoresAtivos = jogadores dados
        jogadorEncontrado = filter (\j -> playerID j == pid) jogadoresAtivos
    
    case jogadorEncontrado of
        [] -> putStrLn "Jogador não encontrado."
        (j:_) -> do
            let saldoAtual = saldo j
                custoTotal = custoBaccarat + valorAposta
            
            if saldoAtual < fromIntegral custoTotal
                then putStrLn $ "Saldo insuficiente. Custo total: " ++ show custoTotal ++ " pontos."
                else do
                    (maoJogador, maoBanco) <- sortearMaos
                    
                    -- Determina vencedor
                    let vencedor = calcularVencedor maoJogador maoBanco
                        ganhou = aposta == vencedor
                        pontosJ = sum maoJogador `mod` 10
                        pontosB = sum maoBanco `mod` 10
                    
                    
                    let premio = if ganhou 
                                then round (fromIntegral valorAposta * multiplicadorPremio aposta)
                                else 0
                    
                   
                    registrarJogada pid "Baccarat" custoTotal (fromIntegral premio)
                    
                    
                    putStrLn $ "\n=== RESULTADO BACCARAT ==="
                    putStrLn $ "Jogador: " ++ show maoJogador ++ " → " ++ show pontosJ ++ " (mais perto de 9)"
                    putStrLn $ "Banco:   " ++ show maoBanco   ++ " → " ++ show pontosB ++ " (mais perto de 9)"
                    putStrLn $ "--------------------------------"
                    putStrLn $ "Vencedor: " ++ case vencedor of
                                              Player  -> "JOGADOR"
                                              Banco   -> "BANCO"
                                              Empate  -> "EMPATE"
                    putStrLn $ "Sua aposta: " ++ show aposta
                    putStrLn $ "--------------------------------"
                    if ganhou
                        then putStrLn $ " GANHOU! +" ++ show premio ++ " pontos!"
                        else putStrLn " PERDEU"
                    putStrLn $ "Saldo atual: " ++ show (saldo j - fromIntegral custoTotal + fromIntegral premio)
                    putStrLn ""
                    
