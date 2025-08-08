import System.Random (randomRIO)

-- Novo tipo para trabalhar com Strings valoradas e inteiros
data Carta = As | Numero Int | Valete | Dama | Rei
  deriving (Eq)

-- Transformando o tipo Carta em string
mostrarCarta :: Carta -> String
mostrarCarta As         = "A"
mostrarCarta (Numero n) = show n
mostrarCarta Valete     = "J"
mostrarCarta Dama       = "Q"
mostrarCarta Rei        = "K"

-- Simulando um baralho
baralho :: [Carta]
baralho = concat $ replicate 4
  [ As
  , Numero 2, Numero 3, Numero 4, Numero 5, Numero 6, Numero 7, Numero 8, Numero 9, Numero 10
  , Valete, Dama, Rei
  ]

-- Sorteia uma carta, retornando ela e o baralho sem ela
sortCarta :: [Carta] -> IO (Carta, [Carta])
sortCarta baralho = do
    idx <- randomRIO (0, length baralho - 1)
    let carta = baralho !! idx
    let novoBaralho = take idx baralho ++ drop (idx + 1) baralho
    return (carta, novoBaralho)

-- Calcula o valor da carta
valorCarta :: Carta -> Int
valorCarta As         = 1
valorCarta (Numero n) = n
valorCarta Valete     = 10
valorCarta Dama       = 10
valorCarta Rei        = 10

-- Soma a pontuação da mão
pontuacao :: [Carta] -> Int
pontuacao cartas =
    let
        valoresSemAs = sum [valorCarta c | c <- cartas, c /= As]
        quantidadeAs = length (filter (== As) cartas)

        -- Função auxiliar para ajustar os Áses
        ajustarAs total 0 = total
        ajustarAs total n
          | total + 10 <= 21 = ajustarAs (total + 10) (n - 1)
          | otherwise = total
    in
        ajustarAs (valoresSemAs + quantidadeAs) quantidadeAs

-- Mostra a mão do jogador
mostrarMao :: [Carta] -> String
mostrarMao = unwords . map mostrarCarta

-- Função para o loop principal do jogo
loop :: [Carta] -> [Carta] -> [Carta] -> IO ()
loop [] [] baralho = do
    (j1, baralho1) <- sortCarta baralho
    (b1, baralho2) <- sortCarta baralho1
    loop [j1] [b1] baralho2

loop [j1] [b1] baralho = do
    (j2, baralho1) <- sortCarta baralho
    (b2, baralho2) <- sortCarta baralho1
    let maoJog = [j1, j2]
    let maoBanca = [b1, b2]

    -- Verifica se a banca fez 21 nas duas primeiras cartas
    if pontuacao maoBanca == 21 then do
        putStrLn $ "Suas cartas: " ++ mostrarMao maoJog ++ " | Total: " ++ show (pontuacao maoJog)
        putStrLn $ "Cartas da banca: " ++ mostrarMao maoBanca ++ " | Total: 21"
        putStrLn "A banca fez 21! Oponente venceu."
    else loop maoJog maoBanca baralho2

loop pj pb baralho = do
    let somaJ = pontuacao pj
    let somaB = pontuacao pb

    putStrLn $ "Suas cartas: " ++ mostrarMao pj ++ " | Total: " ++ show somaJ
    -- Mostra apenas a primeira carta da banca
    putStrLn $ "Carta visível da banca: " ++ mostrarCarta (head pb)

    if somaJ > 21 then
        verificaVencedor pj pb
    else do
        putStrLn "Deseja comprar mais uma carta? [s/n]"
        entrada <- getChar
        _ <- getLine

        if entrada == 's' then do
            (novaJog, baralho1) <- sortCarta baralho
            if somaB < 18 then do
                (novaBan, baralho2) <- sortCarta baralho1
                loop (pj ++ [novaJog]) (pb ++ [novaBan]) baralho2
            else loop (pj ++ [novaJog]) pb baralho1
        else do
            (novaPB, baralho1) <- loopBanca pb baralho
            verificaVencedor pj novaPB

-- Loop da banca compra cartas até >= 18
loopBanca :: [Carta] -> [Carta] -> IO ([Carta], [Carta])
loopBanca pb baralho = do
    let somaB = pontuacao pb
    if somaB < 18 then do
        (novaBan, baralho1) <- sortCarta baralho
        loopBanca (pb ++ [novaBan]) baralho1
    else return (pb, baralho)

-- Verifica quem venceu
verificaVencedor :: [Carta] -> [Carta] -> IO ()
verificaVencedor pj pb = do
    let somaJ = pontuacao pj
    let somaB = pontuacao pb
    let distJ = somaJ - 21
    let distB = somaB - 21

    putStrLn $ "Sua pontuação final: " ++ show somaJ
    putStrLn $ "Pontuação final da banca: " ++ show somaB
    putStrLn $ "Cartas da banca: " ++ mostrarMao pb

    let resultado
            | somaJ > 21 && somaB > 21 && distJ < distB = "Ambos estouraram! Mas você está mais perto de 21, Você venceu."
            | somaJ > 21 && somaB > 21 && distJ > distB = "Ambos estouraram! Mas o oponente está mais perto de 21, O oponente venceu."
            | somaJ > 21 && somaB > 21 && distJ == distB = "Ambos estouraram! Com distâncias iguais, é um empate."
            | somaJ > 21 = "Você estourou! Oponente venceu."
            | somaB > 21 = "Oponente estourou! Você venceu."
            | somaJ > somaB = "Você venceu!"
            | somaB > somaJ = "Oponente venceu!"
            | otherwise = "Empate!"

    putStrLn resultado

main :: IO ()
main = do
    let baralhoInicial = baralho
    loop [] [] baralhoInicial
    putStrLn "Deseja jogar novamente? [s/n]"
    resposta <- getChar
    _ <- getLine
    if resposta == 's' then main else putStrLn "Obrigado por jogar!"
