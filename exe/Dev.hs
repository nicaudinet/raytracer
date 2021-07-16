{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Optional (Optional(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Turtle
import Prelude hiding (FilePath)

newtype Executable = Executable { unExecutable :: Text }
  deriving (Show, Eq)

findExecutables :: IO [Executable]
findExecutables = findExeNames <$> T.readFile "raytracer.cabal"
  where
    exePrefix :: Text
    exePrefix = "executable "

    findExeNames :: Text -> [Executable]
    findExeNames
      = map (Executable . T.drop (T.length exePrefix))
      . filter (exePrefix `T.isPrefixOf`)
      . T.lines 

data Action
  = Repl Executable
  | Run Executable
  | Test

parser :: [Executable] -> Parser Action
parser exeNames = repl <|> run <|> test
  where
    checkExeName :: Text -> Maybe Executable
    checkExeName t =
      if elem (Executable t) exeNames
      then Just (Executable t)
      else Nothing

    exeHelpMsg :: Optional HelpMessage
    exeHelpMsg =
      Specific . HelpMessage . T.intercalate ", " $ map unExecutable exeNames

    subParser :: Parser Executable
    subParser = arg checkExeName "exec" exeHelpMsg

    repl :: Parser Action
    repl = subcommand "repl" "REPL actions" (fmap Repl subParser)

    run :: Parser Action
    run = subcommand "run" "Run actions" (fmap Run subParser)

    checkTest :: Text -> Maybe Action
    checkTest t = if t == "test" then Just Test else Nothing

    test :: Parser Action
    test = arg checkTest "test" Default

action :: Action -> Shell ExitCode
action (Repl (Executable name)) = proc "ghcid" ["-c", "cabal repl exe:" <> name] mempty
action (Run (Executable name)) = proc "cabal" ["run", "exe:" <> name] mempty
action Test = proc "cabal" ["test"] mempty

main :: IO ()
main = do
  executables <- findExecutables
  options "Dev script for life experiments" (parser executables) >>= sh . action
