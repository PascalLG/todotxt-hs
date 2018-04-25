-----------------------------------------------------------------------------
-- TodoTxt
-- Copyright (c) 2018, Pascal Levy
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-----------------------------------------------------------------------------

module CmdPriority (
      cmdPri
    , cmdDepri
) where

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import Data.Char (isDigit, ord, chr)
import Data.List (partition)
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'pri' command.
--
cmdPri :: [String] -> IO ExitStatus
cmdPri args = do
    result <- parseArgsM args []
    case result of
        Right (_, [rank, [pri]]) | all isDigit rank && pri >= 'A' && pri <= 'Z' 
                      -> loadFileAndRun $ doPriority (read rank) (Just (ord pri - 64))
        Right (_, _)  -> putErr (ErrInvalidPriority "pri") >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Parse arguments for the 'depri' command.
--
cmdDepri :: [String] -> IO ExitStatus
cmdDepri args = do
    result <- parseArgsM args []
    case result of
        Right (_, [rank]) | all isDigit rank 
                      -> loadFileAndRun $ doPriority (read rank) Nothing
        Right (_, _)  -> putErr (ErrInvalidPriority "depri") >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'pri' or 'depri' command.
--
doPriority :: Int -> Maybe Int -> [Task] -> IO ExitStatus
doPriority rank pri = update . partition ((== rank) . taskRank)
    where
        update :: ([Task], [Task]) -> IO ExitStatus
        update ([match], rest) = do
            status <- saveFile $ match {taskPriority = pri } : rest
            case status of
                Left err -> putErr err >> return StatusFailed
                Right _  -> do
                    putStrLn $ case pri of
                        Nothing -> "TODO: task #" ++ (show rank) ++ " deprioritized"
                        Just p  -> "TODO: task #" ++ (show rank) ++ " prioritized (" ++ [chr (p + 64)] ++ ")"
                    return StatusOK
        update ([], _) = putErr (ErrUnknownTask rank) >> return StatusInvalidCommand
        update _       = error "unexpected state"

-----------------------------------------------------------------------------
