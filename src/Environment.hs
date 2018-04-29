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

module Environment (
      Option(..)
    , parseArgsM
    , parseArgs
) where

import Data.List (nub)
import Data.Either (partitionEithers)
import Error

-----------------------------------------------------------------------------
-- GNU style command line option handling.

-- | An enumeration of all possible options the application supports.
--
data Option =
      OptionDone
    | OptionRemove
    deriving (Show, Ord, Eq)

-- | Map between option flags, option letters and option full names.
--
supportedOptions :: [(Option, Char, String)]
supportedOptions = [ ( OptionDone,   'd', "done"    )
                   , ( OptionRemove, 'r', "remove"  )]

-- | Lookup a short option (from its letter).
--
lookups :: [(Option, Char, String)] -> Char -> Either Error Option
lookups list opt = case lookup opt (map (\(o, c, _) -> (c, o)) list) of
    Nothing -> Left $ ErrUnsupportedOption ('-':[opt])
    Just o  -> Right o

-- | Lookup a long option (from its full name).
--
lookupl :: [(Option, Char, String)] -> String -> Either Error Option
lookupl list opt = case lookup opt (map (\(o, _, s) -> (s, o)) list) of
    Nothing -> Left $ ErrUnsupportedOption ("--" ++ opt)
    Just o  -> Right o

-- | Parse the command line in our monad stack, to handle the --no-ansi
-- flag at a global level.
--
parseArgsM :: [String] -> [Option] -> IO (Either [Error] ([Option], [String]))
parseArgsM cmdline optlist = do
    let (errs, opts, args) = parseArgs cmdline optlist
    return $ if null errs then Right (opts, args)
                          else Left errs

-- | Parse a list of arguments and return a list of recognised options,
-- a list of errors (if any), and the list of remaining parameters.
--
parseArgs :: [String] -> [Option] -> ([Error], [Option], [String])
parseArgs cmdline optlist = let (errs, opts, params) = parse cmdline
                             in  (nub errs, nub opts, params)
    where
        parse :: [String] -> ([Error], [Option], [String])
        parse (('-':'-':opt):args) = let (errs, opts, params) = parse args
                                     in case lookupl supported opt of
                                            Left err'  -> (err':errs, opts, params)
                                            Right opt' -> (errs, opt':opts, params)
        parse (('-':opt):args)     = let (errs, opts, params) = parse args
                                         (errs', opts') = partitionEithers $ map (lookups supported) opt
                                     in  (errs' ++ errs, opts' ++ opts, params)
        parse args                 = ([], [], args)

        supported :: [(Option, Char, String)]
        supported = filter (\(opt, _, _) -> opt `elem` optlist) supportedOptions

-----------------------------------------------------------------------------
