-- AULA 8 --
-- ALUNO: Joao Gabriel Fritsch

import Data.Char
import Data.List

-- 1) Escreva uma função recursiva isBin :: String -> Bool para verificar se uma dada String representa um número binário,
-- ou seja, contém apenas caracteres '0' ou '1'. As únicas funções pré-definidas autorizadas aqui são head e tail. 
-- Exemplos de uso de isBin:

-- isBin "101010"
-- True
-- isBin "1212"
-- False
-- isBin ""
-- False

isBin :: String -> Bool
isBin "" = True 
isBin str = if (((head str) == '1') || ((head str) == '0'))
    then isBin (tail str)
    else False



-- 2) Reescreva a função acima de forma não-recursiva. Dê outro nome para ela, por exemplo isBin'. Aqui você pode usar 
-- quaisquer funções auxiliares pré-definidas em Haskell.
binN :: Char -> Bool
binN c = (c == '1') || (c == '0')

isBin' :: String -> Bool
isBin' "" = False
isBin' str = all binN str



-- 3) Encontra-se abaixo a definição parcial da função bin2dec :: [Int] -> Int, que converte uma lista de 0's e 1's 
-- (representando um número binário), em seu equivalente em decimal.

-- Observe que:
-- . Usou-se undefined para o caso em que a função não tem resultado definido.
-- . Usou-se uma função auxiliar (auxBin2Dec) que recebe, como segundo argumento, o expoente que deverá multiplicar o 
-- primeiro elemento da lista.
-- . Implemente a função auxBin2Dec de forma recursiva, para que bin2dec funcione corretamente, conforme os exemplos 
-- abaixo:

-- bin2dec [0]
--0
-- bin2dec [1]
--1
-- bin2dec [0,1]
--1
-- bin2dec [1,0,1]
--5

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec bits exp
    | exp == 0 = (head bits) * 1
    | otherwise = (head bits) * 2^exp + auxBin2Dec (tail bits) (exp-1)


bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)



-- 4) Reescreva a função do exercício anterior de forma não-recursiva, usando funções pré-definidas em Haskell. Dê outro
-- nome para a função (por exemplo, bin2dec').

bin2dec' :: [Int] -> Int
bin2dec' = foldl' (\acc x -> acc * 2 + x) 0



-- 5) Crie uma função recursiva dec2bin :: Int -> [Int] que receba um número inteiro positivo e retorne sua representação
-- em binário, sob forma de uma lista de 0's e 1's. As funções auxiliares autorizadas aqui são mod, div e reverse. 
-- Exemplos de uso da função:

auxDec2Bin :: Int -> [Int]
auxDec2Bin 0 = [0]
auxDec2Bin num
    | num < 2 = [1]
    | otherwise = num `mod` 2 : auxDec2Bin (num `div` 2)


dec2bin :: Int -> [Int]
dec2bin num = reverse (auxDec2Bin num)



-- 6) Implemente uma dessas funções: isHex :: String -> Bool ou hex2dec :: String -> Int ou dec2hex :: Int -> String, que
-- são semelhantes às dos exercícios anteriores, porém com números hexadecimais no lugar de números binários. Aqui está
-- tudo liberado: você pode escolher qual das funções irá implementar, sem restrições sobre como deve fazer isso.

isHex :: String -> Bool
isHex str = all (isHexDigit) str



