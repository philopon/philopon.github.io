{-# LANGUAGE DeriveDataTypeable #-}

module Bib where

import Control.Applicative
import Hakyll
import Text.CSL
import Data.Typeable(Typeable)
import Data.Binary
import qualified Data.Aeson as Json
import Data.Monoid
import           Unsafe.Coerce

newtype Bib = Bib [Reference]
            deriving ( Show, Typeable )

instance Monoid Bib where
    Bib a `mappend` Bib b = Bib $ a ++ b
    mempty = Bib []

instance Binary Bib where
    get          = maybe (Bib []) Bib . Json.decode <$> get
    put (Bib rs) = put $ Json.encode rs

instance Writable Bib where
    write _ _ = return ()

compiler :: Compiler (Item Bib)
compiler = do
    filepath <- toFilePath <$> getUnderlying
    makeItem =<< unsafeCompiler (Bib <$> readBiblioFile filepath)

cast :: Item Bib -> Item Biblio
cast = unsafeCoerce
