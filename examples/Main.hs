{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Concurrent

import           Control.Monad.Free.VanLaarhovenE


main :: IO ()
main = iterM ioInterpreter program >> putStrLn "exit!"

program :: ( HasEffect effects Logging
           , HasEffect effects Suspend
           , HasEffect effects Http)
        => Free effects (Either Int (Response String))
program = do
  logMsg "hold on"
  suspend 1000
  getHttp "http://example.org"


logMsg :: HasEffect effects Logging => String -> Free effects ()
logMsg msg = liftF $ \e -> logEff e msg


suspend :: HasEffect effects Suspend => Int -> Free effects ()
suspend micros = liftF $ \e -> suspendEff e micros


getHttp :: HasEffect effects Http => Url -> Free effects (Either Int (Response String))
getHttp url = liftF $ \e -> getHttpEff e url


-- interpret logging actions in IO
logIO :: Logging IO
logIO = Logging { logEff = putStrLn }


-- suspend in IO
suspendIO :: Suspend IO
suspendIO = Suspend { suspendEff = threadDelay }


httpIO :: Http IO
httpIO = Http { getHttpEff = error "to be implemented"
              , postHttpEff = error "to be implemented"}

-- our effect stack
type MyEffects = ( Http ': Logging ': Suspend ': '[] )


-- our interpreter
ioInterpreter :: Effects MyEffects IO
ioInterpreter = httpIO .:. logIO .:. suspendIO .:. EmptyE


-- Http Effect
data Http m =
    Http { getHttpEff  :: Url -> m (Either Int (Response String))
         , postHttpEff :: Url -> RequestBody -> m (Either Int (Response String))
         }


-- Logging Effect
data Logging m = Logging { logEff :: String -> m () }


-- suspend effect
data Suspend m = Suspend { suspendEff :: Int -> m () }


type Url = String
type RequestBody = String
data Response b =Response { responseBody :: b }
