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

module CmdAdd (
      cmdAdd
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time (Day)
import Console
import Environment
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Parse arguments for the 'add' command.
--
cmdAdd :: [String] -> IO ExitStatus
cmdAdd args = do
    result <- parseArgsM args []
    case result of
        Left errs     -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (_, ts) -> loadFileAndRun $ doAdd ts

-- | Execute the 'add' command.
--
doAdd :: [String] -> [Task] -> IO ExitStatus
doAdd names tasks = do
    today <- utctDay <$> getCurrentTime
    let newtasks = map (makeTask today) (zip names [maximum (map taskRank tasks) + 1..])
    status <- saveFile (tasks ++ newtasks)
    case status of
        Left err -> putErr err >> return StatusFailed
        Right _  -> do
            mode <- getConsoleMode
            putStrLn "Added:"
            mapM_ (T.putStrLn . showTask mode) newtasks
            return StatusOK
    where
        makeTask :: Day -> (String, Int) -> Task
        makeTask date (name, rank) = Task { taskRank = rank
                                          , taskDone = False
                                          , taskPriority = Nothing
                                          , taskCompletionDate = Nothing
                                          , taskCreationDate = Just date
                                          , taskName = T.pack name }

-----------------------------------------------------------------------------
