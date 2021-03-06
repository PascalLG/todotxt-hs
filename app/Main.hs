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

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Error
import CmdHelp
import CmdAdd
import CmdList
import CmdPriority
import CmdDone
import CmdPurge

-----------------------------------------------------------------------------

-- | Entry point.
--
main :: IO ()
main = do
    status <- getArgs >>= run
    exitWith $ if status == StatusOK then ExitSuccess
                                     else ExitFailure (fromEnum status)

-- | Command dispatching.
--
run :: [String] -> IO ExitStatus
run ("ls":xs)            = cmdList xs
run ("add":xs)           = cmdAdd xs
run ("pri":xs)           = cmdPri xs
run ("depri":xs)         = cmdDepri xs
run ("done":xs)          = cmdDone xs
run ("undone":xs)        = cmdUndone xs
run ("purge":xs)         = cmdPurge xs
run ("help":"ls":xs)     = cmdHelp helpList xs
run ("help":"add":xs)    = cmdHelp helpAdd xs
run ("help":"pri":xs)    = cmdHelp helpPriority xs
run ("help":"depri":xs)  = cmdHelp helpPriority xs
run ("help":"done":xs)   = cmdHelp helpDone xs
run ("help":"undone":xs) = cmdHelp helpDone xs
run ("help":"purge":xs)  = cmdHelp helpPurge xs
run (_)                  = printUsage >> return StatusOK

-----------------------------------------------------------------------------
