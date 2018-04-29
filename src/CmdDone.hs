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

{-# LANGUAGE OverloadedStrings #-}

module CmdDone (
      cmdDone
    , cmdUndone
) where

import qualified Data.Text.IO as T
import Data.Char (isDigit)
import Data.List (partition)
import Data.Monoid ((<>))
import Console
import Misc
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'done' command.
--
cmdDone :: [String] -> IO ExitStatus
cmdDone args = do
    result <- parseArgsM args [OptionRemove]
    case result of
        Right (opts, [rank]) | all isDigit rank 
                      -> loadFileAndRun $ doDone (action opts) (read rank)
        Right (_, _)  -> putErr ErrInvalidCommandArguments >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

    where
        action :: [Option] -> Action
        action opts | OptionRemove `elem` opts = Remove
                    | otherwise                = MarkAsDone

-- | Parse arguments for the 'undone' command.
--
cmdUndone :: [String] -> IO ExitStatus
cmdUndone args = do
    result <- parseArgsM args []
    case result of
        Right (_, [rank]) | all isDigit rank 
                      -> loadFileAndRun $ doDone MarkAsUndone (read rank)
        Right (_, _)  -> putErr ErrInvalidCommandArguments >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Possible actions for the done/undone command.
--
data Action = MarkAsDone
            | MarkAsUndone
            | Remove
            deriving (Eq)

-- | Execute the 'done' or 'undone' command.
--
doDone :: Action -> Int -> [Task] -> IO ExitStatus
doDone action rank = update . partition ((== rank) . taskRank)
    where
        update :: ([Task], [Task]) -> IO ExitStatus
        update ([match], rest) = do
            let (newtasks, msg) = case action of
                                    MarkAsDone   -> (match { taskDone = True } : rest, "marked as done")
                                    MarkAsUndone -> (match { taskDone = False } : rest, "marked as undone")
                                    Remove       -> (rest, "deleted")
            status <- saveFile newtasks
            case status of
                Left err -> do
                    putErr err 
                    return StatusIOError
                Right _  -> do
                    cm <- getConsoleMode
                    T.putStrLn $  "todo: task " <> (printRank cm rank) <> " " <> msg <> "."
                    return StatusOK
        update ([], _) = putErr (ErrUnknownTask rank) >> return StatusInvalidCommand
        update _       = error "unexpected state"

-----------------------------------------------------------------------------
