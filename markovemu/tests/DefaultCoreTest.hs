module Main where

import qualified System.Exit as Exit

import Test.HUnit

import Core

-- Правильно написанные программы должны хорошо читаться снизу вверх
successor = [("aL", "La", False),
             ("a0", "0a", False),
             ("a" , "b" , False),
             ("Lb", "b0", False),
             ("0b", "L" , True ),
             ("b" , "L" , True ),
             (""  , "a" , False)]

successorUnary = [("00", "0L0" , True),
                  ("0L", "0LL" , True)]
initUnary = "00"
sumUnary = [("+00"  , ""     , True),
            ("L0+0L", "LL0+0", False),
            ("00+0L", "0L0+0", False)]
multUnary = [("00*00" , "00"    , True ),
             ("L0*00" , "0*00"  , False), 
             ("00*0L" , "00*0"  , False), -- x * 0 = 0

             ("0L0*"  , ""      , True ),
             ("*0L0"  , ""      , True ), -- x * 1 = x
    
             ("0y"     , "0"       , False), -- Этап 2: Добавляем маркер, который будет дублировать 'x' и оставлять 'L'
             ("Ly"     , "yL"      , False), -- Этап 2: ', чтобы не сработало раньше времени правила (x * 1 = x) 
             ("xy"     , "yxL"     , False), -- Этап 2: y - вспомогательный маркер для дублирования
             ("x0*0LL" , "xyz0*0'L", False), -- Этап 2: z - маркер данного состояния
             ("z0*0'LL", "yz0*0'L" , False), -- Этап 2: 
             ("0h"     , "0"       , False), -- Этап 3
             ("Lh"     , "hL"      , False), -- Этап 3
             ("xh"     , "hL"      , False), -- Этап 3: Символов 'L' в правой части выражения не осталось, заменяем все 'x' на 'L'
             ("z0*0'L0", "h0*0'L0" , False), -- Этап 3: h - маркер данного состояния
             ("*0'L0"  , "*0L0"    , False), -- Этап 4: В правой части 1 -> x * 1 = x
             ("xL"     , "xx"      , False), -- Этап 1
             ("0L"     , "0x"      , False)] -- Этап 1: заменяем все символы 'L' на 'x', чтобы дублировать только 'x' на каждом сложении


test1 :: Test
test1 = TestCase (assertEqual "should return LL00" "LL00" (last $ Core.run successor "L0LL"))

{- === -}

test2 :: Test
test2 = TestCase (assertEqual "should return 0L0"      "0L0"     (last $ Core.run successorUnary initUnary))

test3 :: Test
test3 = TestCase (assertEqual "should return 0LL0"     "0LL0"    (last $ Core.run successorUnary "0L0"))

test4 :: Test
test4 = TestCase (assertEqual "should return 0L0"      "0L0"     (last $ Core.run sumUnary "0L0+00"))

test5 :: Test
test5 = TestCase (assertEqual "should return 0L0"      "0L0"     (last $ Core.run sumUnary "00+0L0"))

test6 :: Test
test6 = TestCase (assertEqual "should return 0LL0"     "0LL0"    (last $ Core.run sumUnary "0L0+0L0"))

test7 :: Test
test7 = TestCase (assertEqual "should return 0LLLLL0"  "0LLLLL0" (last $ Core.run sumUnary "0LL0+0LLL0"))

test8 :: Test
test8 = TestCase (assertEqual "should return 00"       "00"      (last $ Core.run multUnary "00*0LLL0"))

test9 :: Test
test9 = TestCase (assertEqual "should return 00"       "00"      (last $ Core.run multUnary "0LLL0*00"))

test10 :: Test
test10 = TestCase (assertEqual "should return 0LLL0"   "0LLL0"   (last $ Core.run multUnary "0L0*0LLL0"))

test11 :: Test
test11 = TestCase (assertEqual "should return 0LLL0"           "0LLL0"   (last $ Core.run multUnary "0LLL0*0L0"))

test12 :: Test
test12 = TestCase (assertEqual "should return 0LLLLLL0"        "0LLLLLL0"  (last $ Core.run multUnary "0LL0*0LLL0"))

test13 :: Test
test13 = TestCase (assertEqual "should return 0LLLLLLLLLLLL0"  "0LLLLLLLLLLLL0"  (last $ Core.run multUnary "0LLLL0*0LLL0"))

{- === -}

tests :: Test
tests = TestList [TestLabel "test1"   test1, 
                  TestLabel "test2"   test2,
                  TestLabel "test3"   test3,
                  TestLabel "test4"   test4,
                  TestLabel "test5"   test5,
                  TestLabel "test6"   test6,
                  TestLabel "test7"   test7,
                  TestLabel "test8"   test8,
                  TestLabel "test9"   test9,
                  TestLabel "test10" test10,
                  TestLabel "test11" test11,
                  TestLabel "test12" test12,
                  TestLabel "test13" test13]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
