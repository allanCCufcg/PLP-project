module Jogos.Blackjack
  ( Carta(..)
  , Valor(..)
  , Naipe(..)
  , baralho
  , sortCarta
  , pontuacao
  , mostrarMao
  , valorAposta
  , mostraSaldo
  , jogarBlackjack
  ) where

import System.Random (randomRIO)
import EstadoGlobal
import qualified Data.Map as Map
import Control.Monad (when)

-- Tipo dos naipes
data Naipe = Espadas | Copas | Ouros | Paus
  deriving (Eq, Enum, Bounded, Show)

-- Tipo dos valores das cartas
data Valor = As | Numero Int | Valete | Dama | Rei
  deriving (Eq, Show)

-- Tipo da carta
data Carta = Carta { valor :: Valor, naipe :: Naipe }
  deriving (Eq)

instance Show Carta where
    show = mostrarCarta

-- Mostrar carta como string
mostrarCarta :: Carta -> String
mostrarCarta (Carta v n) = valorStr v ++ naipeStr n
  where
    valorStr As         = "A"
    valorStr (Numero x) = show x
    valorStr Valete     = "J"
    valorStr Dama       = "Q"
    valorStr Rei        = "K"
    naipeStr Espadas = "♠"
    naipeStr Copas   = "♥"
    naipeStr Ouros   = "♦"
    naipeStr Paus    = "♣"

mostrarMao :: [Carta] -> String
mostrarMao = unwords . map mostrarCarta

-- Baralho padrão completo (52 cartas)
baralho :: [Carta]
baralho = [Carta v n | n <- [Espadas .. Paus], 
                       v <- [As] ++ map Numero [2..10] ++ [Valete, Dama, Rei]]

sortCarta :: [Carta] -> IO (Carta, [Carta])
sortCarta [] = error "Baralho vazio!"
sortCarta b = do
    idx <- randomRIO (0, length b - 1)
    let carta = b !! idx
    let novoBaralho = take idx b ++ drop (idx + 1) b
    return (carta, novoBaralho)

-- Valor da carta
valorCarta :: Carta -> Int
valorCarta (Carta v _) = case v of
    As         -> 1
    Numero n   -> n
    Valete     -> 10
    Dama       -> 10
    Rei        -> 10

-- Soma a pontuação da mão (considerando Ás como 11 se não estourar)
pontuacao :: [Carta] -> Int
pontuacao cartas =
    let
        valoresSemAs = sum [valorCarta c | c <- cartas, valor c /= As]
        quantidadeAs = length (filter (\c -> valor c == As) cartas)
        ajustarAs total 0 = total
        ajustarAs total n
          | total + 10 <= 21 = ajustarAs (total + 10) (n - 1)
          | otherwise = total
    in
        ajustarAs (valoresSemAs + quantidadeAs) quantidadeAs

valorAposta :: Float
valorAposta = 10

mostraSaldo :: PlayerID -> IO Float
mostraSaldo pid = do
    dados <- getGlobalData
    let maybeJogador = filter (\j -> playerID j == pid) (jogadores dados)
    case maybeJogador of
        (j:_) -> return (saldo j)
        []    -> return 0

jogarBlackjack :: PlayerID -> IO ()
jogarBlackjack pid = do
    saldoAtual <- mostraSaldo pid
    if saldoAtual < valorAposta
        then putStrLn "Saldo insuficiente para jogar!"
        else do
            let baralhoCompleto = baralho
            
            (c1, b1) <- sortCarta baralhoCompleto
            (c2, b2) <- sortCarta b1
            let maoJogador = [c1, c2]

            (d1, b3) <- sortCarta b2
            (d2, b4) <- sortCarta b3
            let maoDealer = [d1, d2]

            putStrLn $ "Sua mão: " ++ mostrarMao maoJogador ++ " (" ++ show (pontuacao maoJogador) ++ ")"
            putStrLn $ "Mão do dealer: " ++ mostrarCarta d1 ++ " ??"

            let pontosJ = pontuacao maoJogador
            let pontosD = pontuacao maoDealer

            if pontosJ > pontosD && pontosJ <= 21 || pontosD > 21
                then do
                    let ganho = valorAposta
                    registrarJogada pid "BlackJack" (round valorAposta) ganho
                    putStrLn "Você venceu!"
                else do
                    registrarJogada pid "BlackJack" (round valorAposta) 0
                    putStrLn "Você perdeu!"
