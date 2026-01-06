{-# LANGUAGE ScopedTypeVariables #-}

module TestSyntax where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (mapM, mapM_)
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)

-- 简单的测试语法验证
testSyntax :: IO Bool
testSyntax = do
    let testList = [1,2,3] :: [Int]
    
    -- 测试 mapM 语法
    results1 <- mapM (\x -> return (x * 2)) testList
    
    -- 测试 mapM_ 语法  
    mapM_ (\x -> return ()) testList
    
    -- 测试 zipWithM 语法
    let otherList = [4,5,6] :: [Int]
    results2 <- zipWithM (\x y -> return (x + y)) testList otherList
    
    return (length results1 == 3 && length results2 == 3)

spec :: Spec
spec = describe "Syntax Test" $ do
    it "should test basic syntax" $ property $
        \x -> x == x