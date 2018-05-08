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
    , helpList
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (mapM_, when)
import Console
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'ls' command.
--
cmdList :: [String] -> IO ExitStatus
cmdList args = do
    result <- parseArgsM args [OptionDone]
    case result of
        Right (opts, xs) -> loadFileAndRun $ doList (OptionDone `elem` opts) xs
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand

-- | Execute the 'ls' command.
--
doList :: Bool -> [String] -> [Task] -> IO ExitStatus
doList done patterns tasks = do
    let f1 = if done then id else filter (not . taskDone)
    let f2 = if null patterns then id else filter (matchPattern (map (T.toCaseFold . T.pack) patterns))
    let list = (sortByNaturalOrder . f2 . f1) tasks
    when ((not . null) list) $ do
         cm <- getConsoleMode
         putStrLn "#   Pri Created    Description"
         mapM_ (T.putStrLn . showTask cm) list
    return StatusOK

-- | Check if a task contains all the words of a given
-- list. Comparison is not case sensitive.
--
matchPattern :: [T.Text] -> Task -> Bool
matchPattern patterns task = let content = T.words $ T.toCaseFold $ taskName task
                             in  all (flip elem content) patterns

-----------------------------------------------------------------------------

helpList :: IO ()
helpList = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:todo ls}} [{y:--done}}] [{y:keywords...}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    List the content of the {m:todo.txt}} file, one task a line. The first column"
    putLine $ "    displays the task rank (actually the line number in the file). This number"
    putLine $ "    is used to refer to tasks in other commands. The following columns display"
    putLine $ "    the priority (if any) as a letter from {m:A}} to {m:Z}}, the creation date, and the"
    putLine $ "    task description. The list is sorted by decreasing priority. Tasks having"
    putLine $ "    no priority come at the end of the list. Although the file format allows"
    putLine $ "    for a completion date, it is currently never displayed."
    putLine $ ""
    putLine $ "    Finished tasks are not printed by default. To list them as well, add the"
    putLine $ "    {y:--done}} flag."
    putLine $ ""
    putLine $ "    You can filter the task list by adding one or more keywords (or tags) after"
    putLine $ "    the command. Only tasks containing all the specified keywords and tags are"
    putLine $ "    listed. Comparison is not case sensitive."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-d}}, {y:--done}}       List done tasks as well."
    putLine $ ""

-----------------------------------------------------------------------------
