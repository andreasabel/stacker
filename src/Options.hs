{-# LANGUAGE OverloadedStrings #-}

module Options
  ( parseOptions
  , Options(..)
  ) where

import Options.Applicative
import Types

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Bump snapshots in stack*.yaml files"
     <> header "stack-snapshots - A tool for managing stack snapshots" )

-- | Options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> commandParser
  <*> colorOption

-- | Command parser
commandParser :: Parser Command
commandParser = subparser
  ( command "bump" (info (pure Bump) (progDesc "Update stack*.yaml files"))
 <> command "dry-run" (info (pure DryRun) (progDesc "Show what would be updated"))
 <> command "update" (info (pure Update) (progDesc "Update stackage snapshots database"))
 <> command "info" (info (pure Info) (progDesc "Print GHC version to snapshot mapping"))
 <> command "config" (info configParser (progDesc "Configure stack-snapshots"))
 <> command "version" (info (pure Version) (progDesc "Print version information"))
 <> command "numeric-version" (info (pure NumericVersion) (progDesc "Print version number"))
 <> command "license" (info (pure PrintLicense) (progDesc "Print license text"))
 <> command "help" (info (pure Help) (progDesc "Print help"))
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
colorOption = option auto
  ( long "color"
 <> metavar "WHEN"
 <> value Auto
 <> help "Use colored output (always, never, auto)" )
