--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           System.FilePath.Posix
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List(unfoldr)
import qualified Data.Map as M
import qualified Text.HTML.TagSoup as TS
import qualified Text.Highlighting.Kate as Kate
--------------------------------------------------------------------------------

postParPage :: Int
postParPage = 2

recentCount :: Int
recentCount = 5

postsPattern :: Pattern
postsPattern = "posts/**/post.*"

dateFormat :: String
dateFormat = "%Y-%m-%d"

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "About:Blank"
    , feedDescription = ""
    , feedAuthorName  = "philopon"
    , feedAuthorEmail = ""
    , feedRoot        = "http://philopon.github.io/"
    }

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*.min.css" $ do
        route idRoute
        compile copyFileCompiler

    match ("css/*" .&&. complement "css/*.min.css") $ do
        route   idRoute
        compile compressCssCompiler

    create ["css/highlight.css"] $ do
        route idRoute
        compile $ makeItem (Kate.styleToCss Kate.kate)

    match "templates/*" $ compile templateCompiler

    match "posts/**" $ version "raw" $ do
        route idRoute
        compile copyFileCompiler

    tags <- sortTagsBy tagOrder <$> buildTags postsPattern (\tag -> fromFilePath $ "tags" </> tag </> "1.html")

    singlePages <- createPaginate postsPattern

    multiPages  <- createPaginateWith postParPage 
        (\pn -> if pn == 1 
                then "index.html" 
                else fromFilePath $ "pages" </> show pn <.> "html")
        (postsPattern .&&. hasNoVersion)

    match postsPattern $ version "post list" $ do
        route $ customRoute ((<.> "html"). takeDirectory . toFilePath)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post-list.html" (dateField "date" dateFormat <> defaultContext)

    let postCxt = postContext tags singlePages

    paginateRules singlePages $ \_ _ -> do
        route   $ customRoute ((<.> "html"). takeDirectory . toFilePath)
        compile $ do 
            postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]

            pandocCompiler
                >>= saveSnapshot "raw_post"
                >>= loadAndApplyTemplate "templates/post.html"    (postCxt postList)
                >>= loadAndApplyTemplate "templates/default.html" (postCxt postList)
                >>= postLink

    paginateRules multiPages $ \pn pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pat "raw_post" :: Compiler [Item String]
            postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]
            
            let paginate = paginateField
                    (\p -> return . fromFilePath $ 
                        if p == 1
                        then "index.html"
                        else "pages" </> show p <.> "html") multiPages pn
                    
                pagesCxt =
                    paginate <>
                    field "title" (\i ->  
                        if itemIdentifier i == "index.html"
                          then return "Home"
                          else return $ "All posts - " ++ show pn) <>
                    paginateContext multiPages        <>
                    listField "posts" (postCxt postList) (mapM postLink posts) <>
                    globalContext tags postList
            makeItem ""
                >>= loadAndApplyTemplate "templates/multipost.html"   pagesCxt
                >>= loadAndApplyTemplate "templates/default.html" pagesCxt

    tagsRules tags $ \tag pat -> do
        tagPages <- createPaginateWith postParPage
                    (\pn -> fromFilePath $ "tags" </> tag </> show pn <.> "html") pat
        paginateRules tagPages $ \pn pat' -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots pat' "raw_post"
                postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]

                let paginate = paginateField
                        (\p -> return . fromFilePath $ "tags" </> tag </> show p <.> "html")
                        tagPages pn
                let tagsCxt =
                        paginate <>
                        constField "title" (tag ++ " - " ++ show pn)    <>
                        listField "posts" (postCxt postList) (mapM postLink posts) <>
                        globalContext tags postList
                makeItem ""
                    >>= loadAndApplyTemplate "templates/multipost.html" tagsCxt
                    >>= loadAndApplyTemplate "templates/default.html"   tagsCxt

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCxt = bodyField "description" <> (postCxt [])
            posts <- loadAllSnapshots (postsPattern .&&. hasNoVersion) "raw_post"
            renderAtom feedConfig feedCxt posts
    
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCxt = bodyField "description" <> (postCxt [])
            posts <- loadAllSnapshots (postsPattern .&&. hasNoVersion) "raw_post"
            renderRss feedConfig feedCxt posts

tagOrder :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
tagOrder a@(_,ai) b@(_,bi) = case length bi `compare` length ai of
    EQ -> caseInsensitiveTags a b
    o  -> o

nPages :: Paginate -> Int
nPages = M.size . paginatePages

globalContext :: Tags -> [Item String] -> Context String
globalContext tags pl =
    listField "postList" defaultContext (take recentCount <$> recentFirst pl) <>
    field "taglist" (const $ renderTags tagFunc concat tags) <>
    defaultContext
  where tagFunc tag url n _ _ = TS.renderTags 
            [ TS.TagOpen "li" []
            , TS.TagOpen "a" [("href", url)] 
            , TS.TagText $ tag ++ " "
            , TS.TagClose "a"
            , TS.TagOpen "span" [("class", "badge")]
            , TS.TagText $ show n
            , TS.TagClose "span"
            , TS.TagClose "li"
            ]

postContext :: Tags -> Paginate -> [Item String] -> Context String
postContext tags pagenate pl =
    tagsField "tags" tags <>
    dateField "date" dateFormat <>
    paginateContext pagenate <>
    globalContext tags pl

postLink :: Item String -> Compiler (Item String)
postLink item = do
    mbr <- getRoute $ itemIdentifier item
    return $ case mbr of
        Nothing -> item
        Just r  -> fmap (withUrls $ process r) item
  where process r url =
          if isRelative url && not (isExternal url)
          then normalise $ "/" </> dropExtension r </> url
          else url

paginateField :: (Int -> Compiler Identifier) -> Paginate -> Int -> Context String
paginateField f paginate pn = listField "paginate"
    (field "active" (\i ->
        if itemBody i == show pn
        then return "active"
        else fail "") <> defaultContext)
    (mapM (\n -> f n >>= \i -> return $ Item i (show n)) [1 .. nPages paginate])

--------------------------------------------------------------------------------

createPaginateWith :: (MonadMetadata m, Functor m) => Int -> (PageNumber -> Identifier) -> Pattern -> m Paginate
createPaginateWith n makeId pattern = do
    idents <- map itemIdentifier <$> (recentFirst . map (`Item` undefined) =<< getMatches pattern)
    let pages          = flip unfoldr idents $ \xs ->
            if null xs then Nothing else Just (splitAt n xs)
        numPages       = length pages
        paginatePages' = zip [1..] pages
        pagPlaces'     =
            [(ident, idx) | (idx,ids) <- paginatePages', ident <- ids] ++
            [(makeId i, i) | i <- [1 .. numPages]]

    return $ Paginate (M.fromList paginatePages') (M.fromList pagPlaces') makeId
        (PatternDependency pattern idents)

createPaginate :: (MonadMetadata m, Functor m) => Pattern -> m Paginate
createPaginate pattern = do
    idents <- map itemIdentifier <$> (recentFirst . map (`Item` undefined) =<< getMatches pattern)
    let pagPages  = M.fromList $ zip [1 ..] (map return idents)
        pagPlaces = M.fromList $ zip idents [1 ..]
        makeId pn = case M.lookup pn pagPages of
            Just [id'] -> id'
            _          -> error $
                "Hakyll.Web.Paginate.buildPaginate: " ++
                "invalid page number: " ++ show pn

    return $ Paginate pagPages pagPlaces makeId
        (PatternDependency pattern idents)

