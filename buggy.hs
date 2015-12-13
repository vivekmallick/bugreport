import System.IO
import System.Directory
import System.Process
import Data.Char

-- Bug detail modules
import BugStrings
import BugListDefn

-- Files to store bugs

bugfile = "bugs"
newbugfile = "bugs1"
helpfile = "buggyhelp.txt"
tempfile = "bugtemp.txt"

inputBugDetails :: BugList -> IO BugList
inputBugDetails bugList = do
    putStr "Software in question: "
    hFlush stdout
    sware <- getLine
    putStr "Short Description: "
    hFlush stdout
    sDesc <- getLine
    putStr "Long Description: "
    hFlush stdout
    lDesc <- getLine
    return (((BugDetail ((snd bugList) + 1) (encodeString sware) (encodeString sDesc) (encodeString lDesc) Unknown Registered):(fst bugList)), (((snd bugList) + 1)))


listBugDetailsR :: [BugDetail] -> IO ()
listBugDetailsR bl = 
    if length(bl) == 0
        then do
            putStrLn "-- End of List--"
        else do
            putStrLn (printBugDetail (head bl))
            listBugDetailsR (tail bl)

listBugDetails :: BugList -> IO ()
listBugDetails b = listBugDetailsR (listOfBugDetail b)

showParticularDetail :: BugList -> IO ()
showParticularDetail b = do
    putStr "Bug ID of the bug: "
    hFlush stdout
    id <- getLine
    let (numid, isId) = stringToIntError id
    if isId
        then
            putStrLn $ showDetailId numid (listOfBugDetail b)
        else
            putStrLn "Invalid id"

changeStatus :: BugList -> IO BugList
changeStatus b = do
    putStr "ID of software: "
    hFlush stdout
    id <- getLine
    let (numid, isId) = stringToIntError id
    if isId
        then
            return (updateStatus b numid)
        else
            return b

setSeverity b = do
    putStr "ID of the software: "
    hFlush stdout
    id <- getLine
    let (numid, isId) = stringToIntError id
    if isId
        then do
            putStr "New severity level (Unknown | Ignorable | Low | Medum | High | Urgent): "
            hFlush stdout
            newSeverityStr <- getLine
            let (newSeverity, isSeverity) = stringToSeverityError newSeverityStr
            if isSeverity
                then
                    return (updateSeverity b numid newSeverity)
                else
                    return b
        else
            return b

showHelp :: IO ()
showHelp = do
    helpContents <- readFile helpfile
    putStr helpContents

defaultEditor :: String
defaultEditor = "vim"

setEditor :: String -> String
setEditor s = if (length s) == 0 then defaultEditor else s

decodeIntoFile s f = writeFile f (decodeString s)

updateLongDescIO :: BugList -> Integer -> String -> IO BugList
updateLongDescIO b n s = do
    decodeIntoFile (searchLDescStr b n) tempfile
    callCommand ((setEditor s) ++ " " ++ tempfile)
    updatedContents <- readFile tempfile
    removeFile tempfile
    return (updateLongDescInfo b n (encodeString updatedContents))

runEditCmdLong :: BugList -> String -> IO BugList
runEditCmdLong b s = do
    putStr "Bug ID to edit: "
    hFlush stdout
    id <- getLine
    let (numid, isID) = stringToIntError id
    if isID
        then do
            bnew <- updateLongDescIO b numid s
            return bnew
        else
            return b

processCmd :: String -> BugList -> IO BugList
processCmd a b
    | a == "add" = do
        bl <- inputBugDetails b
        return bl
    | a == "list" = do
        listBugDetails b
        return b
    | a == "show" = do
        showParticularDetail b
        return b
    | a == "changeStatus" = do
        newb <- changeStatus b
        return newb
    | a == "setSeverity" = do
        newb <- setSeverity b
        return newb
    | a == "help" = do
        showHelp
        return b
    | a == "editLongDesc" = do
        newb <- runEditCmdLong b ""
        return newb
    | True = do
        putStrLn $ "Unknown command: " ++ a
        return b



buggy_main_loop :: BugList -> IO String
buggy_main_loop bugList = 
    do
        putStrLn ""
        putStr "Buggy> "
        hFlush stdout
        bugcmd <- getLine
        if bugcmd /= "exit"
            then do
                bl <- processCmd bugcmd bugList
                buggy_main_loop bl
            else do
                return (bugListToFile bugList)


main :: IO ()
main = do
    bughdl <- openFile bugfile ReadMode
    bugdetails <- hGetContents bughdl
    editeddetails <- buggy_main_loop (fileToBugList bugdetails)
    putStrLn "Good Bye!"
    bugwhdl <- openFile newbugfile WriteMode
    hPutStr bugwhdl editeddetails
    hFlush bugwhdl
    hClose bugwhdl
    hClose bughdl
    removeFile bugfile
    renameFile newbugfile bugfile
