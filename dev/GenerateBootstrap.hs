#!/usr/bin/env stack
-- stack --resolver=lts-6.6 runghc --package=shelly --package=parsec

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Control.Monad
import           CssParserLite
import           Data.List        (nub, sort)
import           Data.Monoid
import qualified Data.Text        as T
import qualified Debug.Trace      as D
import           Shelly
import           Text.Parsec
import           Text.Parsec.Text
default (T.Text, Int)

main :: IO ()
main = shelly $ do
  styleSheet <- print_stdout False $ run "curl" ["https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.3/css/bootstrap.min.css"]
  let classes = foldMap collectClassSelecter <$> parse css "parse selecter" styleSheet
      classNames = (nub.sort.fmap (mkClassName.T.pack.className)) <$> classes
  echoFileLead
  echoResult classNames

  return ()
  where
    echoFileLead = do
      echo "-- | This module provides CSS class names for common Bootstrap 4 classes.\n"
      echo "module Halogen.Themes.Bootstrap4 where\n"
      echo "import Halogen.HTML (ClassName(), className)\n"

    mkClassName line =
      let parts = T.split (=='-') line
          remain = T.toTitle <$> tail parts
          methodName = T.intercalate T.empty $ head parts : remain
      in methodName <> " :: ClassName\n"
          <> methodName <> " = className \"" <> line <> "\"\n"

    collectClassSelecter (RuleSet sels rs) =
      filter
        (\s -> case s of ClassSelecter _ -> True; _ -> False)
        sels
      <>
      foldMap collectClassSelecter rs
    collectClassSelecter _ = mempty

    className (ClassSelecter name) = name
    className _ = mempty

    echoResult (Right a) = void (sequenceA $ echo <$> a)
    echoResult (Left err) = echo_err $ T.pack $ show err
