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
      cmdPriority
) where

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import Data.Char (isDigit, ord)
import Data.List (partition)
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'pri' command.
--
cmdPriority :: [String] -> IO ExitStatus
cmdPriority args = do
    result <- parseArgsM args []
    case result of
        Right (_, [rank, [pri]])
             | all isDigit rank && pri >= 'A' && pri <= 'Z' -> loadFileAndRun $ doPriority (read rank) pri
        Right (_, _)  -> putErr (ErrInvalidPriority) >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'pri' command.
--
doPriority :: Int -> Char -> [Task] -> IO ExitStatus
doPriority rank pri tasks = do
    let (match, rest) = partition ((== rank) . taskRank) tasks
    if null match then putErr (ErrUnknownTask rank) >> return StatusInvalidCommand
                  else update (head match) rest
    where
        update :: Task -> [Task] -> IO ExitStatus
        update t rest = do
            status <- saveFile $ t {taskPriority = Just (ord pri - 64) } : rest
            case status of
                Left err -> putErr err >> return StatusFailed
                Right _  -> do
                    putStrLn $ "TODO: task #" ++ (show rank) ++ " prioritized (" ++ [pri] ++ ")"
                    return StatusOK

-----------------------------------------------------------------------------
