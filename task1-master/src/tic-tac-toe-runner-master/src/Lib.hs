{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Lens ( (&), (^.), (.~) )
import Control.Exception (handle)
import qualified Data.ByteString as BS
import Data.Char (isNumber)
import Data.List as L (all, concat)
import Data.Text ()
import Data.String.Conversions (cs)
import Network.Wreq ( defaults, header, responseBody)
import Network.HTTP.Client (HttpException(..))
import qualified Network.Wreq.Session as Sess
import System.Console.ANSI
    ( setSGR,
      Color(Blue, Green, Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )
import System.Environment (getArgs)
import System.Exit ( ExitCode(ExitSuccess, ExitFailure), exitWith )
import System.Process ( readProcessWithExitCode )
import Control.Monad (when, void)

data Source = Get Int | None
  deriving (Show, Eq)
newtype Destination = Post Int
  deriving (Show, Eq) 

data Step = Delete Int | Action (Source, Destination) | Terminate

isDelete :: Step -> Bool
isDelete (Delete _) = True
isDelete _ = False

host :: String
host = "tic-tac-toe.homedir.eu"

run :: Char -> [Step] -> IO ()
run who actions = do
  args <- getArgs
  case args of
    ["", _] -> fatal "Task number is empty" >> exitWith (ExitFailure 1)
    [_, ""] -> fatal "Path is empty" >> exitWith (ExitFailure 2)
    [no, path] ->
      if L.all isNumber no
        then Sess.newAPISession >>= runWithArgs (read no) path who actions
        else fatal (no ++ " not a number") >> exitWith (ExitFailure 3)
    l -> do
      fatal $ "Two arguments expected (task number and path to executable), got " ++ show l
      exitWith (ExitFailure 4)

runWithArgs :: Int -> String -> Char -> [Step] -> Sess.Session -> IO ()
runWithArgs n p w (Delete i : as) sess = do
  runner $ "Cleanup (" ++ show (n+i) ++ ")"
  let opts = defaults & header "Accept" .~ ["text/plain;charset=utf-8"]
  void $ Sess.deleteWith opts sess ("http://" ++ host ++ "/mailbox/" ++ show (n+i))
  runWithArgs n p w as sess
runWithArgs n p w (Action (s,d) : as) sess =
  handle (\e@(HttpExceptionRequest _ _) -> print e >> runWithArgs n p w (filter isDelete as) sess) $ do
  stdin <-
    case s of
      None -> return "*"
      Get i -> do
          runner "Retrieving competitor's message"
          let opts = defaults & header "Accept" .~ ["text/plain;charset=utf-8"]
          r <- Sess.getWith opts sess ("http://" ++ host ++ "/mailbox/" ++ show (n+i))
          return $ cs $ r ^. responseBody
  let who = [w]
  runner $ L.concat ["Executing `", p, " ", who, "`"]
  runner "Stdin:"
  program stdin
  (exitCode, stdout, stderr) <- readProcessWithExitCode p [who] stdin
  runner "Stdout:"
  program stdout
  runner "Stderr:"
  program stderr
  runner "Exit code:"
  program $ show exitCode
  case exitCode of
    ExitSuccess -> send (cs stdout) >> runWithArgs n p w as sess
    ExitFailure code -> do
      when (code < 20) $ send $ cs stdout
      runWithArgs n p w (filter isDelete as) sess
  where
    send :: BS.ByteString -> IO ()
    send body = do
      runner "Sending data to a competitor"
      let opts = defaults & header "Content-Type" .~ ["text/plain;charset=utf-8"]
      let (Post i) = d
      void $ Sess.postWith opts sess ("http://" ++ host ++ "/mailbox/" ++ show (n+i)) body
runWithArgs _ _ _ (Terminate:_) _ = fatal "Game should be completed by this time"
runWithArgs _ _ _ _ _ = return ()

program :: String -> IO ()
program s = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn s
  setSGR [Reset]

fatal :: String -> IO ()
fatal s = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn s
  setSGR [Reset]

runner :: String -> IO ()
runner s = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn s
  setSGR [Reset]
