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

module Error (
      ExitStatus(..)
    , Error(..)
    , putErr
) where

import System.IO (hPutStrLn, stderr)
import Console

-----------------------------------------------------------------------------
-- Error handling.

-- | Type that represents exit status codes.
--
data ExitStatus =
      StatusOK                  -- Should be first so that (fromEnum StatusOK) == 0
    | StatusInvalidCommand
    | StatusFailed
    deriving (Show, Enum, Eq)

-- | Type that represents error conditions.
--
data Error =
      ErrorReadFile (Maybe FilePath) String
    | ErrorWriteFile (Maybe FilePath) String
    | ErrUnsupportedOption String
    | ErrExtraArgument String
    deriving (Eq)

instance Show Error where
    show = renderError

-- | Render an error as a string.
--
renderError :: Error -> String
renderError (ErrorReadFile Nothing desc)        = "cannot read file: " ++ desc
renderError (ErrorReadFile (Just path) desc)    = "cannot read file '" ++ path ++ "': " ++ desc
renderError (ErrorWriteFile Nothing desc)       = "cannot write file: " ++ desc
renderError (ErrorWriteFile (Just path) desc)   = "cannot write file '" ++ path ++ "': " ++ desc
renderError (ErrUnsupportedOption opt)          = "unsupported option: " ++ opt
renderError (ErrExtraArgument arg)              = "extra argument: " ++ arg

-- | Print an error on the standard error output.
--
putErr :: Error -> IO ()
putErr err = do
    console <- getConsoleMode
    hPutStrLn stderr $ foreColor console AnsiRed ("error: " ++ show err)

-----------------------------------------------------------------------------
