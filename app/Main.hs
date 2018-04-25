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
import CmdAdd
import CmdList
import CmdPriority

main :: IO ()
main = do
    status <- getArgs >>= run
    exitWith $ if status == StatusOK then ExitSuccess
                                     else ExitFailure (fromEnum status)

run :: [String] -> IO ExitStatus
run ("add":xs)      = cmdAdd xs
run ("ls":xs)       = cmdList xs
run ("pri":xs)      = cmdPri xs
run ("depri":xs)    = cmdDepri xs
run (xs)            = cmdList xs

-----------------------------------------------------------------------------
