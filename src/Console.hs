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

module Console (
      AnsiColor(..)
    , ConsoleMode
    , getConsoleMode
    , foreColor
) where

import System.IO (hIsTerminalDevice, stdout)
import System.Environment (lookupEnv)

-----------------------------------------------------------------------------

-- | Console modes.
--
data ConsoleMode = ModeBasic    -- No fancy text effects
                 | ModeXTerm    -- Xterm console
                 deriving (Eq)

-- | Supported colours.
--
data AnsiColor = AnsiBlack
               | AnsiRed
               | AnsiGreen
               | AnsiYellow
               | AnsiBlue
               | AnsiMagenta
               | AnsiCyan
               | AnsiWhite
               deriving(Eq, Enum)

-- | Associate each colour with a letter. (Except for black
-- since we probably won't print anything in that colour.)
--
colors :: [(Char, AnsiColor)]
colors = [ ('c', AnsiCyan)
         , ('m', AnsiMagenta)
         , ('y', AnsiYellow)
         , ('r', AnsiRed)
         , ('g', AnsiGreen)
         , ('b', AnsiBlue)
         , ('w', AnsiWhite)]

-- | Guess the console mode. If we are in a terminal and the TERM
-- variable is not set to "dumb", then we probably are in a full
-- colour XTerm console.
--
getConsoleMode :: IO ConsoleMode
getConsoleMode = do
    istty <- hIsTerminalDevice stdout
    isdumb <- maybe False (== "dumb") <$> lookupEnv "TERM"
    return $ if istty && not isdumb then ModeXTerm else ModeBasic

-- | Return the ANSI escape sequence for the specified foreground
-- colour.
--
ansiCodeForColor :: AnsiColor -> String
ansiCodeForColor color = "\ESC[" ++ (show (30 + fromEnum color)) ++ "m"

-- | Surround text with the required ANSI escape sequences to print
-- it in the specified colour. (This is not reentrant, due to the
-- way ANSI escape sequences work.)
--
foreColor :: ConsoleMode -> AnsiColor -> String -> String
foreColor ModeBasic _     text = text
foreColor _         color text = (ansiCodeForColor color) ++ text ++ "\ESC[39m"

-----------------------------------------------------------------------------
