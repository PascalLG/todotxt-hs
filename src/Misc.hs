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

module Misc (
      printRank
    , saveTasks
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Console
import Error
import TodoFile

-----------------------------------------------------------------------------

-- | Print a rank.
--
printRank :: ConsoleMode -> Int -> T.Text
printRank cm rank = foreColor cm AnsiYellow (T.pack ("#" ++ show rank))

-- | Save the list of tasks and display either an error
-- or a success message.
--
saveTasks :: [Task] -> T.Text -> IO ExitStatus
saveTasks tasks msg = do
    status <- saveFile tasks
    case status of
        Left err -> putErr err >> return StatusIOError
        Right _  -> T.putStrLn msg >> return StatusOK

-----------------------------------------------------------------------------
