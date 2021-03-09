module Main where

import Eval
import Parser
import Pretty
import Syntax

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskelline

---------------------------------------------------------------------------------
showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ ppexpr x)

---------------------------------------------------------------------------------
process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, ~steps) = runEval ex
      mapM_ showStep steps
      print out

---------------------------------------------------------------------------------
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Untyped> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop
---------------------------------------------------------------------------------
