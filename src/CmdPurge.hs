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

module CmdPurge (
      cmdPurge
    , helpPurge
    , renumTasks    -- exported for unit testing
) where

import qualified Data.Text as T
import Data.List (partition)
import Data.Monoid ((<>))
import Console
import Environment
import Misc
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'purge' command.
--
cmdPurge :: [String] -> IO ExitStatus
cmdPurge args = do
    result <- parseArgsM args [OptionPack]
    case result of
        Right (opts, []) -> loadFileAndRun $ doPurge (OptionPack `elem` opts)
        Right (_, _)     -> putErr ErrInvalidCommandArguments >> return StatusInvalidCommand
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'purge' command.
--
doPurge :: Bool -> [Task] -> IO ExitStatus
doPurge pack = update . partition taskDone
    where
        update :: ([Task], [Task]) -> IO ExitStatus
        update (del, rest)
            | null del  = putStrLn "todo: nothing to purge." >> return StatusOK
            | otherwise = let transform = if pack then renumTasks else id
                          in  saveTasks (transform rest) $ "todo: " <> T.pack (show (length del)) <> " tasks deleted."

-- | Renumber tasks, removing blank lines.
--
renumTasks :: [Task] -> [Task]
renumTasks = map (\(r, t) -> t { taskRank = r }) . zip [1..] . sortByRank

-----------------------------------------------------------------------------

helpPurge :: IO ()
helpPurge = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:todo purge}} [{y:--pack}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Delete all tasks that are marked as done. By default, corresponding lines"
    putLine $ "    are only erased, so the task numbering is preserved. By adding the {y:--pack}}"
    putLine $ "    flag, you can have all these empty lines physically deleted. The resulting"
    putLine $ "    file is shorter, but some tasks may be assigned new ranks."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-p}}, {y:--pack}}       Remove empty lines."
    putLine $ ""

-----------------------------------------------------------------------------
