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
) where

import qualified Data.Text as T
import Data.List (partition)
import Data.Monoid ((<>))
import Environment
import Misc
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'purge' command.
--
cmdPurge :: [String] -> IO ExitStatus
cmdPurge args = do
    result <- parseArgsM args []
    case result of
        Right (_, []) -> loadFileAndRun $ doPurge
        Right (_, _)  -> putErr ErrInvalidCommandArguments >> return StatusInvalidCommand
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'purge' command.
--
doPurge :: [Task] -> IO ExitStatus
doPurge = update . partition taskDone
    where
        update :: ([Task], [Task]) -> IO ExitStatus
        update (del, rest)
            | null del  = putStrLn "todo: nothing to purge." >> return StatusOK
            | otherwise = saveTasks rest $ "todo: " <> T.pack (show (length del)) <> " tasks deleted."

-----------------------------------------------------------------------------
