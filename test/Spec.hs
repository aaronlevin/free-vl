{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
module Main
    ( main
    ) where

import           Control.Monad.Writer
import           Data.Sequence
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad.Free.VanLaarhovenE

main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "free-vl" [
    testCase "example usage" $ do
      let res = execWriter $ iterM interpreter $ do
                  logDebug "Hey a debug"
                  n <- randomNumber
                  logInfo ("Got a random number " <> show n)
      res @?= fromList [ (Debug, "Hey a debug")
                       , (Info, "Got a random number 42")
                       ]
  ]


type MyEffects = Logging ': Random ': '[]


data LogLevel = Info
              | Debug deriving (Show, Eq)


data Logging m = Logging {
      logMsg :: LogLevel -> String -> m ()
    }


data Random m = Random {
     rng :: m Int
   }


logDebug :: HasEffect eff Logging => String -> Free eff ()
logDebug msg = liftF $ \Logging {..} -> logMsg Debug msg


logInfo :: HasEffect eff Logging => String -> Free eff ()
logInfo msg = liftF $ \Logging {..} -> logMsg Info msg


randomNumber :: HasEffect eff Random => Free eff Int
randomNumber = liftF $ \Random {..} -> rng


interpreter :: Effects MyEffects (Writer (Seq (LogLevel, String)))
interpreter = fakeLogger .:. fakeRNG .:. EmptyE


fakeLogger :: Logging (Writer (Seq (LogLevel, String)))
fakeLogger = Logging (\lvl msg -> tell (singleton (lvl, msg)))


fakeRNG :: Monad m => Random m
fakeRNG = Random (return 42)
