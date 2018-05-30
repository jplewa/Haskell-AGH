import Data.List
import System.Environment
import System.IO.Error
import Control.Exception

{--
checkArgs :: Maybe String
checkArgs = if (mempty getArgs) then Nothing
            else (Just (findFileName))
            where findFileName  = do
                    args <- getArgs
                    return (read $ head args :: String)
--}

lineCount :: IO ()
lineCount = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba linii: " ++ (show(length (lines contents))))

wordCount :: IO ()
wordCount = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba wyrazów: " ++ (show(length (words contents))))

letterCount :: IO ()
letterCount = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba znaków (ze spacją): " ++ (show(length (filter (\s -> s /= '\n' && s /= '\t') contents))))
 
nonWhiteCharCount :: IO ()
nonWhiteCharCount = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba znaków (bez spacji): " ++ (show(length (filter (\s -> s /= '\n' && s /= '\t' && s /= ' ') contents))))
 

distinctWordCount :: IO ()
distinctWordCount = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba różnych słów: " ++ (show(length(nub $ words contents))))

linesLongerThan80Count :: IO ()
linesLongerThan80Count = do 
            (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn ("liczba linii o długości większej niż 80 znaków: " ++ (show (length (filter (\s -> (length s) > 80) (lines contents)))))


exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else if isUserError ex
                then putStrLn "No arguments provided!"
                else ioError ex

stats :: IO ()
stats = do  
    lineCount
    wordCount
    letterCount
    nonWhiteCharCount
    distinctWordCount
    linesLongerThan80Count

main = do
    stats `catch` exHdlr    