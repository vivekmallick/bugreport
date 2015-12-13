module BugListDefn
( BugList(..)
, BugDetail(..)
, Severity(..)
, Status(..)
, listOfBugDetail
, trivialBugDetail
, trivialBugList
, insertBugDetail
, printBugDetail
, showDetailId
, lstStrOfBugD
, fileToBugDetailList
, fileLastId
, fileToBugList
, storeFormat
, bugListToFile
, updateStatus
, stringToSeverityError
, updateSeverity
, updateLongDescInfo
, searchLDescStr
) where

import BugStrings

-- Data structure to handle bugs.

data Severity = Unknown | Ignorable | Low | Medium | High | Urgent deriving (Show, Read)
data Status = Registered | Confirmed | InProcess | Fixed deriving (Show, Read)
data BugDetail = BugDetail { bugid :: Integer
                           , software :: String
                           , shortDesc :: String
                           , longDesc :: String
                           , severity :: Severity
                           , status :: Status
                           }

bugDescid bd = "Bug ID: " ++ (show (bugid bd)) ++ "\n"
bugDescSoftware bd = "Software: " ++ (decodeString (software bd)) ++ "\n"
bugDescShort bd = "Short Description: " ++ (decodeString (shortDesc bd)) ++ "\n"
bugDescLong bd = "Long Description:\n" ++ (decodeString (longDesc bd)) ++ "\n"
bugDescSeverity bd = "Severity: " ++ (show (severity bd)) ++ "\n"
bugDescStatus bd = "Status: " ++ (show (status bd)) ++ "."
bugDesc bd = (bugDescid bd) ++ (bugDescSoftware bd) ++ (bugDescShort bd) ++ (bugDescLong bd) ++ (bugDescSeverity bd) ++ (bugDescStatus bd)

instance Show BugDetail where
    show bd = bugDesc bd

type ID = Integer
type BugList = ([BugDetail], ID)

-- End of defining data structures

listOfBugDetail :: BugList -> [BugDetail]
listOfBugDetail = fst

trivialBugDetail = (BugDetail 0 "" "" "" Unknown Registered)
trivialBugList = ([trivialBugDetail], 0)

lstStrToBugDet :: [String] -> BugDetail
lstStrToBugDet ls =
    if (length ls) /= 6
        then trivialBugDetail
        else BugDetail (read (ls !! 0) :: Integer)
                       (ls !! 1)
                       (ls !! 2)
                       (ls !! 3)
                       (read (ls !! 4) :: Severity)
                       (read (ls !! 5) :: Status)

insertBugDetail :: BugDetail -> BugList -> BugList
insertBugDetail bugDet bugLst = ((fst bugLst) ++ [bugDet], (bugid bugDet))

printBugDetail :: BugDetail -> String
printBugDetail d = (show (bugid d)) ++ ": " ++ (decodeString (software d)) ++ " | " ++ (decodeString (shortDesc d))

showDetailId :: Integer -> [BugDetail] -> String
showDetailId n b
    | (length b) == 0       = "ID " ++ (show n) ++ " not found."
    | (bugid (head b)) == n = show (head b)
    | True                  = showDetailId n (tail b)

sfBugid b = show (bugid b)
sfSoft  b = software b
sfdescs b = shortDesc b
sfdescl b = longDesc b
sfsever b = show (severity b)
sfstate b = show (status b)

lstStrOfBugD :: BugDetail -> [String]
lstStrOfBugD b = [(sfBugid b),
                  (sfSoft b),
                  (sfdescs b),
                  (sfdescl b),
                  (sfsever b),
                  (sfstate b)]

fileToBugDetailList :: String -> [BugDetail]
fileToBugDetailList s = map (lstStrToBugDet.  bugDetStrToLstSt) (lines s)

fileLastId :: [BugDetail] -> Integer
fileLastId bdl
    | (length bdl) == 0     = 0
    | True                  = maximum (map bugid bdl)

fileToBugList :: String -> BugList
fileToBugList s = (sbd, (fileLastId sbd))
    where sbd = fileToBugDetailList s

storeFormat :: BugDetail -> String
storeFormat b = foldl (++) "" (["<"] ++ (insertBreaks (lstStrOfBugD b) "|") ++ [">\n"])

bugListToFileR :: [BugDetail] -> String -> String
bugListToFileR lbd s
    | length(lbd) == 0 = s
    | True             = bugListToFileR (tail lbd) (s ++ (storeFormat (head lbd)))

bugListToFile :: BugList -> String
bugListToFile b = bugListToFileR (listOfBugDetail b) ""

updateBugDetState :: BugDetail -> BugDetail
updateBugDetState b
    | (show (status b)) == "Registered" = BugDetail
            (bugid b)
            (software b)
            (shortDesc b)
            (longDesc b)
            (severity b)
            (read "Confirmed" :: Status)
    | (show (status b)) == "Confirmed" = BugDetail
            (bugid b)
            (software b)
            (shortDesc b)
            (longDesc b)
            (severity b)
            (read "InProcess" :: Status)
    | (show (status b)) == "InProcess" = BugDetail
            (bugid b)
            (software b)
            (shortDesc b)
            (longDesc b)
            (severity b)
            (read "Fixed" :: Status)
    | (show (status b)) == "Fixed" = BugDetail
            (bugid b)
            (software b)
            (shortDesc b)
            (longDesc b)
            (severity b)
            (read "Fixed" :: Status)

    
updateStatusR :: [BugDetail] -> Integer -> [BugDetail] -> [BugDetail]
updateStatusR bd n processedBd
    | length bd == 0 = processedBd
    | True = 
        if (bugid (head bd) == n)
            then processedBd ++ [updateBugDetState (head bd)] ++ (tail bd)
            else updateStatusR (tail bd) n (processedBd ++ [(head bd)])

updateStatus :: BugList -> Integer -> BugList
updateStatus b n = let
        bl = fst b
        ln = snd b
            in
                (updateStatusR bl n [], ln)

stringToSeverityError :: String -> (Severity, Bool)
stringToSeverityError s =
    if s `elem` ["Unknown", "Ignorable",  "Low", "Medium", "High", "Urgent"]
        then ((read s :: Severity), True)
        else (Unknown, False)

updateSeverityBD :: BugDetail -> Severity -> BugDetail
updateSeverityBD bd s = BugDetail
            (bugid bd)
            (software bd)
            (shortDesc bd)
            (longDesc bd)
            (s)
            (status bd)

updateSeverityR :: [BugDetail] -> Integer -> Severity -> [BugDetail] -> [BugDetail]
updateSeverityR bdl n svr bdlProcessed
    | (length bdl) == 0 = bdlProcessed
    | True = 
        if (bugid (head bdl) == n)
            then bdlProcessed ++ [(updateSeverityBD (head bdl) svr)] ++ (tail bdl)
            else updateSeverityR (tail bdl) n svr (bdlProcessed ++ [(head bdl)])

updateSeverity :: BugList -> Integer -> Severity -> BugList
updateSeverity bl numid newSeverity = 
    let (bugdetl, lastID) = bl
    in (updateSeverityR bugdetl numid newSeverity [], lastID)

searchLDescStrR :: [BugDetail] -> Integer -> String
searchLDescStrR bl n
    | (length bl) == 0 = ""
    | (bugid (head bl)) == n = longDesc (head bl)
    | True = searchLDescStrR (tail bl) n

searchLDescStr :: BugList -> Integer -> String
searchLDescStr b n = searchLDescStrR (fst b) n

updateLDInBugDetail :: BugDetail -> String -> BugDetail 
updateLDInBugDetail bd s = BugDetail
            (bugid bd)
            (software bd)
            (shortDesc bd)
            (s)
            (severity bd)
            (status bd)


updateLongDescInfoR :: [BugDetail] -> Integer -> String -> [BugDetail] -> [BugDetail]
updateLongDescInfoR b n s procB
    | (length b) == 0 = procB
    | True =
        if (bugid (head b)) == n
            then procB ++ ((updateLDInBugDetail (head b) s):(tail b))
            else updateLongDescInfoR (tail b) n s (procB ++ [(head b)])

updateLongDescInfo :: BugList -> Integer -> String -> BugList
updateLongDescInfo b n s =
    let (bugdetl, lastID) = b
    in ((updateLongDescInfoR bugdetl n s []), lastID)
