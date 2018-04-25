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

module CmdDone (
      cmdDone
) where

import Data.Char (isDigit)
import Data.List (partition)
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'do' command.
--
cmdDone :: [String] -> IO ExitStatus
cmdDone args = do
    result <- parseArgsM args [OptionRemove]
    case result of
        Right (opts, [rank]) | all isDigit rank 
                      -> loadFileAndRun $ doDone (OptionRemove `elem` opts) (read rank)
        Right (_, _)  -> putErr (ErrInvalidPriority "do") >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'do' command.
--
doDone :: Bool -> Int -> [Task] -> IO ExitStatus
doDone remove rank = update . partition ((== rank) . taskRank)
    where
        update :: ([Task], [Task]) -> IO ExitStatus
        update ([match], rest) = do
            let newtasks = if remove then rest 
                                     else match { taskDone = True } : rest
            status <- saveFile newtasks
            case status of
                Left err -> putErr err >> return StatusFailed
                Right _  -> putStrLn ("TODO: task #" ++ (show rank) ++ " marked as done") >> return StatusOK
        update ([], _) = putErr (ErrUnknownTask rank) >> return StatusInvalidCommand
        update _       = error "unexpected state"

-----------------------------------------------------------------------------
