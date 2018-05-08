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

module CmdAddSpec (
    spec
) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Test.Hspec
import TodoFile
import CmdAdd

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "CmdAdd" $ do
        it "makes a new task" $ do
            today <- utctDay <$> getCurrentTime
            let task = makeTask today ("test", 17)
            taskRank task `shouldBe` 17
            taskDone task `shouldBe` False
            taskPriority task `shouldBe` Nothing
            taskCompletionDate task `shouldBe` Nothing
            taskCreationDate task `shouldBe` Just today
            taskName task `shouldBe` T.pack ("test")

-----------------------------------------------------------------------------
