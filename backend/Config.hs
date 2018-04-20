module Config  (Config, parse, notesDir) where

import Control.Monad (unless)
import Data.Monoid
import Options.Applicative (Parser, ParserInfo, execParser)
import Options.Applicative.Builder (fullDesc, info, long, metavar, short, strOption)
import System.Directory (doesDirectoryExist)
import System.Exit (die)

newtype Config = Config { notesDir :: FilePath }

parse :: IO Config
parse = do
  cfg <- execParser parserInfo
  notesDirExists <- doesDirectoryExist $ notesDir cfg
  unless notesDirExists . die $ "Notes directory does not exist " <> notesDir cfg
  return cfg

parserInfo :: ParserInfo Config
parserInfo = info configParser fullDesc

configParser :: Parser Config
configParser = Config
    <$> strOption
        ( long "notes-dir"
       <> short 'd'
       <> metavar "DIRECTORY")
