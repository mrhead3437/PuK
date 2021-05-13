-- 26 Buchstaben des Alphabets
-- Groß- und Kleinschreibung beachten
-- Satz- und Sonderzeichen sollen immer ignoriert werden
-- Leerzeichen sollen aus der Eingabe entfern werden
-- Verschlüsselten Text sollen die Zeichen in Fünfergruppen dargestellt werden
-- fromEnum Char to Int 
-- toEnum Int to Char.
-- A - Z 65 - 90
-- a - z 97 - 122
import Data.Char

text = "Haskell ist gar nicht so schwer!"

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar k x = intToString (shiftAll (stringToInt x) (-k))

encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar k x = addSpaces(intToString (shiftAll (stringToInt x) k))

--Quelle: https://stackoverflow.com/questions/22748546/how-to-add-spaces-to-string-in-haskell
addSpaces :: [Char] -> [Char]
addSpaces xs = if length xs <= 5 
    then xs
    else take 5 xs ++ " " ++ addSpaces (drop 5 xs)

deleteSpaces :: [Char] -> [Char]
deleteSpaces = filter (/=' ')

stringToInt :: [Char] -> [Int]
stringToInt n = map fromEnum (deleteSpaces n)

intToString :: [Int] -> [Char]
intToString = map toEnum

shiftAll :: [Int] -> Int -> [Int]
shiftAll [] _ = []
shiftAll (x:xs) k = shift x k : shiftAll xs k    

shift :: Int -> Int -> Int
shift x k   
    | isCharacter x = handleOverflow(x + k)
    | otherwise = x

-- A - Z 65 - 90
-- a - z 97 - 122
isCharacter :: Int -> Bool
isCharacter value
    | value >= 65 && value <= 90 = True
    | value >= 97 && value <= 122 = True
    | otherwise = False

handleOverflow :: Int -> Int
handleOverflow value 
    | value >= 65 && value <= 90 = value
    | value >= 97 && value <= 122 = value
    | value <= 64 = value + 26
    | value <= 96 = value + 26
    | otherwise = value - 26

vkey = "beuthhochschule"

encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere key text = addSpaces(intToString (encrypt (formatTextAsInt text) (formatTextAsInt key)))

formatTextAsInt :: [Char] -> [Int]
formatTextAsInt text = stringToInt (map toUpper (deleteSpaces text))

decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere key text = intToString (decrypt (formatTextAsInt text) (formatTextAsInt key))

encrypt :: [Int] -> [Int] -> [Int]
encrypt [] key = []
encrypt text [] = []
encrypt (t:ts) (k:ks) = encode k t : encrypt ts (ks++[k])
  where
    encode k t =
        if isCharacter t
            then 65 + mod (t + k - 2 * 26) 26
        else t

decrypt :: [Int] -> [Int] -> [Int]
decrypt [] key = []
decrypt text [] = []
decrypt (t:ts) (k:ks) = decode k t : decrypt ts (ks++[k])
  where
    decode k t = 
        if isCharacter t 
            then 65 + mod (t - k + 26) 26
        else t