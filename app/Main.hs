module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Error
import CmdAdd
import CmdList

main :: IO ()
main = do
    status <- getArgs >>= run
    exitWith $ if status == StatusOK then ExitSuccess
                                     else ExitFailure (fromEnum status)

run :: [String] -> IO ExitStatus
run ("add":xs)  = cmdAdd xs
run ("list":xs) = cmdList xs
run (xs)        = cmdList xs
