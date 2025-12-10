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
 <> header ("stack-snapshots version " ++ appVersion)
 <> footer "For more information, see the README" )

-- | Options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> colorOption

-- | Command parser
commandParser :: Parser Command
commandParser = subparser
  ( command "bump" (info (pure Bump) (progDesc "Update stack*.yaml files"))
 <> command "dry-run" (info (pure DryRun) (progDesc "Show what would be updated (default)"))
 <> command "update" (info (pure Update) (progDesc "Update stackage snapshots database"))
 <> command "info" (info (pure Info) (progDesc "Print GHC version to snapshot mapping"))
 <> command "config" (info configParser (progDesc "Configure stack-snapshots"))
 <> command "version" (info (pure Version) (progDesc "Print version information (also: -V, --version)"))
 <> command "numeric-version" (info (pure NumericVersion) (progDesc "Print version number (also: --numeric-version)"))
 <> command "license" (info (pure PrintLicense) (progDesc "Print license text (also: --license)"))
 <> command "help" (info (pure Help) (progDesc "Print this help (also: -h, --help)"))
  )
  <|> flag' Version (long "version" <> short 'V' <> help "Print version information")
  <|> flag' NumericVersion (long "numeric-version" <> help "Print version number")
  <|> flag' PrintLicense (long "license" <> help "Print license text")
  <|> flag' Help (long "help" <> short 'h' <> help "Print help")
  <|> pure DryRun

-- | Config command parser
configParser :: Parser Command
configParser = Config <$> subparser
  ( command "repo" (info repoParser (progDesc "Set repository path"))
  )

-- | Repo config parser
repoParser :: Parser ConfigCmd
repoParser = SetRepo <$> argument str (metavar "PATH")

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
