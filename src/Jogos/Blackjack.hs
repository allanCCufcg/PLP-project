module Jogos.Blackjack
  ( Carta(..)
  , Valor(..)
  , Naipe(..)
  , baralho
  , sortCarta
  , pontuacao
  , mostrarMao
  ) where

import System.Random (randomRIO)

-- Tipo dos naipes
data Naipe = Espadas | Copas | Ouros | Paus
  deriving (Eq, Enum, Bounded, Show)

-- Tipo dos valores das cartas
data Valor = As | Numero Int | Valete | Dama | Rei
  deriving (Eq)

-- Tipo da carta
data Carta = Carta { valor :: Valor, naipe :: Naipe }
  deriving (Eq)

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

-- Baralho padrão
baralho :: [Carta]
baralho = [Carta v n | v <- todosValores, n <- todosNaipes]
  where
    todosValores = [As] ++ map Numero [2..10] ++ [Valete, Dama, Rei]
    todosNaipes = [minBound .. maxBound] :: [Naipe]

-- Sorteia uma carta e retorna a carta e o baralho sem ela
sortCarta :: [Carta] -> IO (Carta, [Carta])
sortCarta baralho = do
    idx <- randomRIO (0, length baralho - 1)
    let carta = baralho !! idx
    let novoBaralho = take idx baralho ++ drop (idx + 1) baralho
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
        valoresSemAs = sum [valorCarta c | c <- cartas, valor (c) /= As]
        quantidadeAs = length (filter (\c -> valor c == As) cartas)
        ajustarAs total 0 = total
        ajustarAs total n
          | total + 10 <= 21 = ajustarAs (total + 10) (n - 1)
          | otherwise = total
    in
        ajustarAs (valoresSemAs + quantidadeAs) quantidadeAs
