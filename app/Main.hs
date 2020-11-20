--
-- EPITECH PROJECT, 2020
-- evalexpr
-- File description:
-- Main.hs
--

import Lib
import System.Environment
import System.Exit
import Text.Printf

failExit :: IO ()
failExit = exitWith (ExitFailure 84)

main:: IO()
main = do
    args <- getArgs
    if (False `elem` (map (`elem` "0123456789 \t\n.-+/*^()") (head args)))
        then do
            putStrLn "String contains incorrect caracters."
            failExit
        else case evalexpr (head args) of
                Left err -> putStrLn err >> failExit
                Right result -> printf "%.2f\n" $ result