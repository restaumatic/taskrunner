module CliArgs where

import Universum
import Options.Applicative

data CliArgs = CliArgs
    { name :: Maybe String  -- Optional name argument
    , force :: Bool -- Skip cache
    , trace :: Bool -- Trace file system access (requires fsatrace)
    , traceFiles :: Bool -- Show individual files in trace (instead of directory summary)
    , cmd  :: String        -- The command to run
    , args :: [String]      -- List of arguments for the command
    } deriving (Show)

commandParser :: Parser CliArgs
commandParser = CliArgs
    <$> optional (strOption
        ( long "name"
       <> short 'n'
       <> metavar "NAME"
       <> help "Optional name for the task" ))
    <*> switch
        ( long "force"
       <> short 'f'
       <> help "Skip cache and fuzzy cache" )
    <*> switch
        ( long "trace"
       <> help "Trace file system access during task execution (requires fsatrace)" )
    <*> switch
        ( long "trace-files"
       <> help "Like --trace but show individual files instead of directory summary" )
    <*> argument str
        ( metavar "CMD"
       <> help "The command to run" )
    <*> many (strArgument 
        ( metavar "ARGS..."
       <> help "Arguments for the command" ))

opts :: ParserInfo CliArgs
opts = info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "Run a command with optional name"
   <> header "taskrunner - a simple command runner"
   <> noIntersperse )

-- Main function to parse and print the command
getCliArgs :: IO CliArgs
getCliArgs = execParser opts

