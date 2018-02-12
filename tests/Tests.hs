{- Copyright 2018 Luis Pedro Coelho
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, OverloadedStrings #-}
module Main where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))

import qualified Data.Conduit.Zstd as CZ

main :: IO ()
main = $(defaultMainGenerator)

case_basic :: IO ()
case_basic = do
    hello <- B.concat <$> (C.runConduitRes (CC.yieldMany ["Hello", " ", "World"] .| CZ.compress 2 .| CZ.decompress .| CL.consume))
    hello @?= "Hello World"
