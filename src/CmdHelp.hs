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

module CmdHelp (
      cmdHelp
    , printUsage
) where

import Data.Version (showVersion)
import Paths_TodoTxt
import Console
import Environment
import Error

-----------------------------------------------------------------------------

-- | Execute the 'help' command.
--
cmdHelp :: IO () -> [String] -> IO ExitStatus
cmdHelp help args = do
    result <- parseArgsM args []
    case result of
        Right (_, xs)
            | null xs   -> help >> return StatusOK
            | otherwise -> putErr ErrInvalidCommandArguments >> return StatusInvalidCommand
        Left errs       -> mapM_ putErr errs >> return StatusInvalidCommand

-----------------------------------------------------------------------------

-- | Print application usage.
--
printUsage :: IO ()
printUsage = do
    putLine $ "{c:TodoTxt - Version " ++ showVersion version ++ "}}"
    putLine $ "{c:(c) 2018, Æquans}}"
    putLine $ ""
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:todo}} <{y:command}}> [{y:options}}] [{y:parameters}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    {y:todo}} is a minimalist to-do list manager that conforms to the todo.txt"
    putLine $ "    format spec. See below for available commands. Refer to {_:{m:http://todotxt.org/}}}}"
    putLine $ "    for more information."
    putLine $ ""
    putLine $ "{*:COMMANDS}}"
    putLine $ "    {y:ls}}      Print the list of tasks"
    putLine $ "    {y:add}}     Add one or more tasks to the list"
    putLine $ "    {y:pri}}     Prioritise a task"
    putLine $ "    {y:depri}}   Deprioritise a task"
    putLine $ "    {y:done}}    Mark a task as done"
    putLine $ "    {y:undone}}  Mark a task as not done"
    putLine $ "    {y:purge}}   Permanently remove done tasks"
    putLine $ "    {y:help}}    Print help about a command"
    putLine $ ""
    putLine $ "    For more information about options and parameters a command supports,"
    putLine $ "    type: {y:todo help}} <{y:command}}>"
    putLine $ ""

-----------------------------------------------------------------------------
