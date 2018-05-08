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
    , ConsoleMode(..)
    , getConsoleMode
    , foreColor
    , putLine
) where

import qualified Data.Text as T
import System.IO (hIsTerminalDevice, stdout)
import System.Environment (lookupEnv)
import Data.Maybe (isJust, fromJust)
import Data.List (uncons)

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
foreColor :: ConsoleMode -> AnsiColor -> T.Text -> T.Text
foreColor ModeBasic _     text = text
foreColor _         color text = T.pack (ansiCodeForColor color) `T.append` text `T.append` T.pack "\ESC[39m"

-----------------------------------------------------------------------------

-- | Print a string containing formatting tags. See @processTags@ in the
-- PrettyPrint.Internal module for more information.
--
putLine :: String -> IO ()
putLine text = do
    cm <- getConsoleMode
    putStrLn $ processTags cm [State { isBold = False
                                     , isUnderline = False
                                     , textColor = AnsiWhite }] text

-- | Text attributes.
--
data State = State { isBold :: !Bool
                   , isUnderline :: !Bool
                   , textColor :: !AnsiColor }

-- | Process formatting tags and generate a string containing ANSI escape
-- sequences. Tags follow the pattern {x:text}} where "x" is a character
-- indicating a text attribute (colour, bold or underline) and "text" is 
-- the string that attribute applies to.
--
processTags :: ConsoleMode -> [State] -> String -> String
processTags _ _ "" = ""

processTags cm stack ('{':'*':':':cs) = emitEscapeCode cm old new ++ processTags cm (new:stack) cs
    where old = head stack
          new = old { isBold = True }

processTags cm stack ('{':'_':':':cs) = emitEscapeCode cm old new ++ processTags cm (new:stack) cs
    where old = head stack
          new = old { isUnderline = True }

processTags cm stack ('{':t:':':cs) | isJust color = emitEscapeCode cm old new ++ processTags cm (new:stack) cs
    where  color = lookup t colors
           old = head stack
           new = old { textColor = fromJust color }

processTags cm stack ('}':'}':cs) | length stack > 1 = emitEscapeCode cm old new ++ processTags cm rest cs
    where Just (old, rest) = uncons stack
          new = head rest

processTags cm stack (c:cs) = c : processTags cm stack cs

-- | Emit ANSI escape code corresponding to a transition between
-- two states of text attributes.
--
emitEscapeCode :: ConsoleMode -> State -> State -> String
emitEscapeCode ModeBasic _ _ = ""
emitEscapeCode _ (State b1 u1 c1) (State b2 u2 c2) = emitBold b1 b2 ++ emitUnderline u1 u2 ++ emitColor c1 c2
    where
        emitBold False True  = "\ESC[1m"
        emitBold True  False = "\ESC[22m"
        emitBold _     _     = ""

        emitUnderline False True  = "\ESC[4m"
        emitUnderline True  False = "\ESC[24m"
        emitUnderline _     _     = ""

        emitColor co1 co2
            | co1 /= co2 = ansiCodeForColor co2
            | otherwise  = ""

-----------------------------------------------------------------------------
