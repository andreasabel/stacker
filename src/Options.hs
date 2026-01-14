{-# LANGUAGE OverloadedStrings #-}

module Options
  ( parseOptions
  , optionsParserInfo
  , Options(..)
  ) where

import Data.Char (toLower)
import Options.Applicative
import Types

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = customExecParser (prefs showHelpOnEmpty) optionsParserInfo

-- | Parser info (exported for help rendering)
optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "A tool to bump snapshots (resolvers) in stack*.yaml files"
 <> header ("stacker version " ++ appVersion)
 <> footer "For more information, see the README" )

-- | Options parser
-- The parser tries subcommand parsing first (which includes color options),
-- then falls back to top-level options (for version/help flags and default).
-- This allows --color to be specified either before the subcommand
-- (e.g., "stacker --color=never dry-run") or within the subcommand
-- (e.g., "stacker dry-run --color=never file.yaml").
-- Precedence: Subcommand parsers take priority, so "stacker dry-run --color=never"
-- will be parsed by the subcommand parser, not the top-level parser.
optionsParser :: Parser Options
optionsParser = subcommandWithOptions <|> topLevelOptions
  where
    -- Helper to combine a command parser with the color option, creating an Options value
    withColorOption :: Parser Command -> Parser Options
    withColorOption cmdParser = Options <$> cmdParser <*> colorOption
    
    -- Subcommand parser that includes color options for each command
    subcommandWithOptions = subparser
      ( command "bump" (info (withColorOption bumpParser) (progDesc "Update stack*.yaml files (optionally specify files, or use --recursive/-r to search subdirectories)"))
     <> command "dry-run" (info (withColorOption dryRunParser) (progDesc "Show what would be updated (default, optionally specify files, or use --recursive/-r to search subdirectories)"))
     <> command "update" (info (withColorOption (pure Update)) (progDesc "Update stackage snapshots database"))
     <> command "info" (info (withColorOption (pure Info)) (progDesc "Print GHC version to snapshot mapping"))
     <> command "config" (info (withColorOption configParser) (progDesc "Configure stacker"))
     <> command "version" (info (withColorOption (pure Version)) (progDesc "Print version information (also: -V, --version)"))
     <> command "numeric-version" (info (withColorOption (pure NumericVersion)) (progDesc "Print version number (also: --numeric-version)"))
     <> command "license" (info (withColorOption (pure PrintLicense)) (progDesc "Print license text (also: --license)"))
     <> command "help" (info (withColorOption (pure Help)) (progDesc "Print this help (also: -h, --help)"))
      )
    
    -- Top-level parser for version/help flags and default command when no subcommand is specified
    topLevelOptions = Options
      <$> commandParserTopLevel
      <*> colorOption
    
    -- Top-level command parser (for flags)
    commandParserTopLevel = 
      flag' Version (long "version" <> short 'V' <> help "Print version information")
      <|> flag' NumericVersion (long "numeric-version" <> help "Print version number")
      <|> flag' PrintLicense (long "license" <> help "Print license text")
      <|> flag' Help (long "help" <> short 'h' <> help "Print help")
      <|> pure (DryRun [] False)

-- | Config command parser
configParser :: Parser Command
configParser = Config <$> subparser
  ( command "repo" (info repoParser (progDesc "Set repository path"))
  )

-- | Repo config parser
repoParser :: Parser ConfigCmd
repoParser = SetRepo <$> argument str (metavar "PATH")

-- | Helper to parse file arguments with optional recursive flag
filesParser :: ([FilePath] -> Bool -> Command) -> Parser Command
filesParser cmd = cmd 
  <$> many (argument str (metavar "FILES..." <> action "file"))
  <*> recursiveOption

-- | Recursive option parser
recursiveOption :: Parser Bool
recursiveOption = switch
  ( long "recursive"
 <> short 'r'
 <> help "Search for stack*.yaml files recursively in all subdirectories" )

-- | Bump command parser
bumpParser :: Parser Command
bumpParser = filesParser Bump

-- | Dry-run command parser
dryRunParser :: Parser Command
dryRunParser = filesParser DryRun

-- | Color option parser
colorOption :: Parser ColorWhen
colorOption = option readColorWhen
  ( long "color"
 <> metavar "WHEN"
 <> value Auto
 <> help "Use colored output (always, never, auto)" )
  where
    readColorWhen = maybeReader $ \s ->
      case map toLower s of
        "always" -> Just Always
        "never" -> Just Never
        "auto" -> Just Auto
        _ -> Nothing
