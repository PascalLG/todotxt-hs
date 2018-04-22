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

module CmdList (
      cmdList
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (mapM_)
import Data.List (sort)
import Error
import Environment
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'ls' command.
--
cmdList :: [String] -> IO ExitStatus
cmdList args = do
    result <- parseArgsM args [OptionAll]
    case result of
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (opts, xs) -> loadFileAndRun $ doList (OptionAll `elem` opts) xs

-- | Execute the 'ls' command.
--
doList :: Bool -> [String] -> [Task] -> IO ExitStatus
doList al patterns tasks = do
    let f1 = if al then id else filter (not . taskDeleted)
    let f2 = if null patterns then id else filter (matchPattern (map (T.toCaseFold . T.pack) patterns))
    mapM_ (T.putStrLn . showTask) ((sort . f2 . f1) tasks)
    return StatusOK

-- | Test if a task name contains all the words of a given
-- list. Comparison is not case sensitive.
--
matchPattern :: [T.Text] -> Task -> Bool
matchPattern patterns task = let content = T.words $ T.toCaseFold $ taskName task
                             in  all (flip elem content) patterns

-----------------------------------------------------------------------------
