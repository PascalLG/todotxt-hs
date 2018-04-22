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

{-# LANGUAGE OverloadedStrings #-}

module TodoFile (
      Task(..)
    , showTask
    , todoFilePath
    , loadFile
    , loadFileAndRun
    , saveFile
    , parseFile     -- exported for unit testing
    , tryParseDate  -- exported for unit testing
) where

import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, copyFile, removeFile)
import System.FilePath (addTrailingPathSeparator, normalise, (<.>))
import Data.List (isPrefixOf, unfoldr)
import Data.Maybe (isJust)
import Data.Char (ord, chr, isSpace)
import Data.Time (Day)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Control.Exception (try, tryJust, catch)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError, ioeGetErrorString, ioeGetFileName)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Console
import Error

-----------------------------------------------------------------------------
-- Tasks.

-- | Record representing a task. The ToDo list file is a made of
-- a list of tasks.
--
data Task = Task { taskRank :: Int                      -- rank in the file
                 , taskDeleted :: Bool                  -- whether the task is done or not
                 , taskPriority :: Maybe Int            -- priority (between 0 and 25)
                 , taskCompletionDate :: Maybe Day      -- completion date
                 , taskCreationDate :: Maybe Day        -- creation date
                 , taskName :: T.Text                   -- task description
                 } deriving (Show, Eq)

instance Ord Task where
    compare (Task _  d1 _         _ _ _) (Task _  d2 _         _ _ _) | d1 /= d2 = compare d1 d2
    compare (Task _  _  (Just p1) _ _ _) (Task _  _  (Just p2) _ _ _) | p1 /= p2 = compare p1 p2
    compare (Task _  _  (Nothing) _ _ _) (Task _  _  (Just _)  _ _ _)            = GT
    compare (Task _  _  (Just _)  _ _ _) (Task _  _  (Nothing) _ _ _)            = LT
    compare (Task r1 _  _         _ _ _) (Task r2 _  _         _ _ _)            = compare r1 r2

-- | Print a task. The output is properly formatted in columns, and 
-- context and project tags are colourised.
--
showTask :: ConsoleMode -> Task -> T.Text
showTask mode (Task rank del pri _ creat name)
     | del       = T.intersperse '\x0336' $ line ModeBasic
     | otherwise = line mode
    where
        line :: ConsoleMode -> T.Text
        line cm = T.concat [ printRank cm rank
                           , T.singleton ' '
                           , printPri cm pri
                           , T.singleton ' '
                           , printDate creat
                           , T.singleton ' '
                           , printDesc cm name ]

        printRank :: ConsoleMode -> Int -> T.Text
        printRank cm x = foreColor cm AnsiYellow (T.pack $ printf "%03d" x)

        printPri :: ConsoleMode -> Maybe Int -> T.Text
        printPri cm (Just x) = foreColor cm AnsiGreen (T.pack $ '(' : chr (x + 64) : ")")
        printPri _  Nothing  = T.pack "   "

        printDate :: Maybe Day -> T.Text
        printDate d@(Just _) = showDate d
        printDate Nothing    = T.pack "          "

        printDesc :: ConsoleMode -> T.Text -> T.Text
        printDesc cm t = T.intercalate (T.singleton ' ') (map colorize (T.split isSpace t))
            where colorize word
                    | "@" `T.isPrefixOf` word = foreColor cm AnsiMagenta word
                    | "+" `T.isPrefixOf` word = foreColor cm AnsiCyan word
                    | otherwise               = word

-----------------------------------------------------------------------------
-- File handling.

-- | Return the path to the ToDo file. Default is ~/todo.txt, but
-- if the TODOTXT environment variable is set, its value is used 
-- instead.
--
todoFilePath :: IO FilePath
todoFilePath = filePath >>= expandTilde >>= return . normalise
    where
        filePath :: IO String
        filePath = do
            p <- lookupEnv "TODOTXT"
            case p of
                Just path -> return path
                Nothing   -> return "~/todo.txt"

        expandTilde :: String -> IO String
        expandTilde path
            | "~" `isPrefixOf` path = getHomeDirectory >>= \home ->  return $ addTrailingPathSeparator home ++ tail path
            | otherwise             = return path

-- | Load the ToDo file and return a list of tasks. If the file does
-- not exists, return an empty list. If the file cannot be loaded,
-- return an ErrorReadFile error.
--
loadFile :: IO (Either Error [Task])
loadFile = do
    result <- try (todoFilePath >>= T.readFile >>= return . parseFile)
    return $ case result of
                 Right tasks -> Right tasks
                 Left exc    -> if isDoesNotExistError exc then Right []
                                                           else Left (ErrorReadFile (ioeGetFileName exc) (ioeGetErrorString exc))

-- | Load the ToDo file and pass the list of tasks to the specified
-- action. If an error occurs, print an error message and abort.
--
loadFileAndRun :: ([Task] -> IO ExitStatus) -> IO ExitStatus
loadFileAndRun action = do
    tasks <- loadFile
    case tasks of
        Left err   -> putErr err >> return StatusFailed
        Right list -> action list

-- | Parse the ToDo file content. Each line is parsed independently using
-- a simple state machine. Empty lines and lines where the task name is
-- missing are discarded.
--
parseFile :: T.Text -> [Task]
parseFile content = filter (not . T.null . taskName)
                  $ map (\(line, rank) -> (fixDate . parseLine 0) (T.unpack line, emptyTask rank))
                  $ zip (T.lines content) [1..]
    where
        emptyTask :: Int -> Task
        emptyTask rank = Task rank False Nothing Nothing Nothing T.empty

        parseLine :: Int -> (String, Task) -> (String, Task)
        parseLine 0 ('x':' ':xs, t)         = parseLine 1 (xs, t { taskDeleted = True })
        parseLine 0 st                      = parseLine 1 st
        parseLine 1 (' ':xs, t)             = parseLine 1 (xs, t)
        parseLine 1 ('(':p:')':' ':xs, t)
            | p >= 'A' && p <= 'Z'          = parseLine 2 (xs, t { taskPriority = Just (ord p - 64) })
        parseLine 1 st                      = parseLine 2 st
        parseLine 2 (' ':xs, t)             = parseLine 2 (xs, t)
        parseLine 2 (xs, t) | isJust d      = parseLine 3 (drop n xs, t { taskCompletionDate = d }) where (n, d) = readDate xs
        parseLine 2 st                      = parseLine 4 st
        parseLine 3 (' ':xs, t)             = parseLine 3 (xs, t)
        parseLine 3 (xs, t) | isJust d      = parseLine 4 (drop n xs, t { taskCreationDate = d }) where (n, d) = readDate xs
        parseLine 3 st                      = parseLine 4 st
        parseLine 4 (xs, t)                 = ("", t { taskName = T.strip (T.pack xs) })
        parseLine _ _                       = error "unexpected state"

        fixDate :: (String, Task) -> Task
        fixDate (_, t@(Task _ _ _ (Just compl) Nothing _)) = t { taskCompletionDate = Nothing, taskCreationDate = Just compl }
        fixDate (_, t                                    ) = t

        readDate :: String -> (Int, Maybe Day)
        readDate s = let firstword = takeWhile (/= ' ') s
                     in  (1 + length firstword, tryParseDate firstword)

-- | Try to parse a date in the YYYY-MM-DD format. Return a Julian Day
-- or Nothing if an error occurred.
--
tryParseDate :: String -> Maybe Day
tryParseDate s = case map castToInt (unfoldr separator s) of
                     [Just y, Just m, Just d] -> buildDate y m d
                     _                        -> Nothing
    where
        separator :: String -> Maybe (String, String)
        separator [] = Nothing
        separator cs = Just . fmap (drop 1) . break (== '-') $ cs

        castToInt :: String -> Maybe Int
        castToInt cs = case (reads cs :: [(Int, String)]) of
                          [(x, "")] -> Just x
                          _         -> Nothing

        buildDate :: Int -> Int -> Int -> Maybe Day
        buildDate y m d
            | y >= 1900 && y <= 2100 && m >= 1 && m <= 12 && d >= 1 && d <= 31 = Just $ fromGregorian (fromIntegral y) m d
            | otherwise                                                        = Nothing

-- | Save a list of tasks to the ToDo file. The previous version is first
-- copied to a backup file. If the writing succeeds, this backup file is
-- deleted.
--
saveFile :: [Task] -> IO (Either Error ())
saveFile ts = do
    path <- todoFilePath
    let backup = path <.> ".bak"
    catch (do _ <- tryJust (guard . isDoesNotExistError) (copyFile path backup)
              T.writeFile path $ generateFile ts
              _ <- try (removeFile backup) :: IO (Either IOError ())
              return $ Right ())
          (\exc -> return $ Left (ErrorWriteFile (ioeGetFileName exc) (ioeGetErrorString exc)))

-- | Generate the ToDo file content from a list of tasks. The rank information
-- in each Task record is ignored: tasks are simply written in order.
--
generateFile :: [Task] -> T.Text
generateFile = T.unlines . map printTask
    where
        printTask :: Task -> T.Text
        printTask (Task _ del pri compl creat name) = T.concat [printDel del, printPri pri, printDate compl, printDate creat, name]

        printDel :: Bool -> T.Text
        printDel True = T.pack "x "
        printDel _    = T.empty

        printPri :: Maybe Int -> T.Text
        printPri (Just x) = T.pack $ '(' : chr (x + 64) : ") "
        printPri Nothing  = T.empty

        printDate :: Maybe Day -> T.Text
        printDate d@(Just _) = T.snoc (showDate d) ' '
        printDate Nothing    = T.empty

-----------------------------------------------------------------------------
-- Helper functions.

-- | Print an optional date to a string in the YYYY-MM-DD format. Return
-- an empty string if the date is missing.
--
showDate :: Maybe Day -> T.Text
showDate (Just x)  = let (y, m, d) = toGregorian x in T.pack $ printf "%04d-%02d-%02d" y m d
showDate (Nothing) = T.empty

-----------------------------------------------------------------------------
