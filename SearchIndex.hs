{-# LANGUAGE OverloadedStrings, TupleSections #-}
module SearchIndex(searchIndex) where

import Control.Monad
import Control.Applicative
import Hakyll
import Text.MeCab
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

isNoun :: Node T.Text -> Bool
isNoun = ("\21517\35422," `T.isPrefixOf`) . nodeFeature

morpheme :: [String] -> Item String -> IO (Item (S.Set T.Text))
morpheme opts (Item idnt str) =
    fmap (Item idnt . S.fromList . map (T.toLower . nodeSurface) . filter isNoun) $
        new opts >>= flip parseToNodes (T.pack str)

reverseIndex :: [Item (S.Set T.Text)] -> H.HashMap T.Text [Int]
reverseIndex items = 
    let mrps = S.toList . S.unions $ map itemBody items
        is   = zip [0..] items
    in H.fromList $ map (\m -> (m, map fst $ filter (\i -> m `S.member` (itemBody . snd) i) is)) mrps

searchIndex :: Context String -> Int -> [String] -> [Item String] -> Compiler L.ByteString
searchIndex cxt desclen opts items = do
    hm   <- unsafeCompiler $ reverseIndex <$> mapM (morpheme opts) items
    urls <- forM items $ \item -> do
        StringField date <- unContext cxt "date" item
        StringField url  <- unContext cxt "url" item

        M.insert "date" date . M.insert "url" url .
            M.insert "description" (take desclen $ itemBody item) <$>
            getMetadata (itemIdentifier item)
    return . Json.encode . Json.object $ 
        [ "pages" Json..= urls
        , "dict"  Json..= hm
        ]
