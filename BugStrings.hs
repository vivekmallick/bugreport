module BugStrings
( encodeString
, decodeString
, splitAlong
, removeAngBrack
, bugDetStrToLstSt 
, stringToIntError
, insertBreaks
) where


import Data.Char

-- Code to encode and decode strings stored in BugList

es_r :: String -> String -> String
es_r s t
    | (length t) == 0    = s
    | (head t) == ' '    = es_r (s ++ "_b") (tail t)
    | (head t) == '\n'   = es_r (s ++ "_n") (tail t)
    | (head t) == '_'    = es_r (s ++ "__") (tail t)
    | True               = es_r (s ++ [(head t)]) (tail t)

encodeString :: String -> String
encodeString s = es_r "" s

-- Int: Mode: 0 Normal 1 Encountered _ command in next step
ds_r :: String -> Int -> String -> String
ds_r dec mode enc
    | (length enc) == 0                  = dec
    | (head enc) == '_'    && mode == 0  = ds_r dec 1 (tail enc)
    | (head enc) == '_' && mode == 1     = ds_r (dec ++ "_") 0 (tail enc)
    | (head enc) == 'b' && mode == 1     = ds_r (dec ++ " ") 0 (tail enc)
    | (head enc) == 'n' && mode == 1     = ds_r (dec ++ "\n") 0 (tail enc)
    | True                               = ds_r (dec ++ [(head enc)]) 0 (tail enc)

decodeString :: String -> String
decodeString enc = ds_r "" 0 enc

-- String manipulation

splitAlongR :: String -> Char -> [String] -> [String]
splitAlongR s c l
    | (length s) == 0 = l
    | (length l) == 0 = splitAlongR s c [""]
    | (head s) == c = splitAlongR (tail s) c ("":l)
    | (length l) == 1 = splitAlongR (tail s) c [(head l) ++ [(head s)]]
    | True = splitAlongR (tail s) c (((head l) ++ [(head s)]):(tail l))

splitAlong :: String -> Char -> [String]
splitAlong s c = reverse (splitAlongR s c [])

removeAngBrack :: String -> String
removeAngBrack s = take ((length s) - 2) (tail s)

bugDetStrToLstSt :: String -> [String]
bugDetStrToLstSt s = splitAlong (removeAngBrack s) '|'

stringToIntError :: String -> (Integer, Bool)
stringToIntError s = if (length s) > 0 && (and (map isDigit s))
    then ((read s :: Integer), True)
    else (0, False)

insertBreaksR :: [a] -> [a] -> a -> Int -> [a]
insertBreaksR l nl b state
    | length(l) == 0 = nl
    | state == 0 = insertBreaksR (tail l) (nl ++ [(head l)]) b 1
    | True = insertBreaksR (tail l) (nl ++ [b] ++ [(head l)]) b 1

insertBreaks :: [a] -> a -> [a]
insertBreaks lst br = insertBreaksR lst [] br 0


