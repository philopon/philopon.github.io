--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import           Control.Applicative
import           Control.Monad
import           System.FilePath.Posix
import           System.Process
import           System.Exit
import           Hakyll

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Char (toLower)
import           Data.List(unfoldr,partition,mapAccumL)
import           Data.Monoid (Monoid, mempty, mconcat, (<>))

import qualified Text.HTML.TagSoup as TS
import qualified Text.Highlighting.Kate as Kate

import           SearchIndex
import qualified Bib
--------------------------------------------------------------------------------

rootAddress :: String
rootAddress = "http://philopon.github.io"

postParPage :: Int
postParPage = 5

recentCount :: Int
recentCount = 5

postsPattern :: Pattern
postsPattern = "posts/**/post.*"

dateFormat :: String
dateFormat = "%Y-%m-%d"

descriptionLength :: Int
descriptionLength = 100

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "About:Blank"
    , feedDescription = ""
    , feedAuthorName  = "philopon"
    , feedAuthorEmail = ""
    , feedRoot        = rootAddress
    }

postRoute :: Identifier -> FilePath
postRoute = ((<.> "html"). takeDirectory . toFilePath)

citationStyle :: Identifier
citationStyle = "citation-styles/nature.csl"

minifyJs :: Item String -> Compiler (Item String)
minifyJs = processCompiler "./node_modules/uglify-js/bin/uglifyjs" ["--unsafe", "-m", "-mf"]

main :: IO ()
main = hakyll $ do
    match "static/**" $ do
        route . customRoute $ joinPath . tail . splitDirectories . toFilePath
        compile $ copyFileCompiler

    match ("images/*" .||. "fonts/*" .||. "css/*.min.css") $ do
        route   idRoute
        compile copyFileCompiler

    match ("js/*/*" .&&. complement "js/*/*.min.js") $ do
        route . customRoute $ ("js" </>) . takeFileName . toFilePath
        compile $ getResourceString >>= minifyJs

    match "js/*/*.min.js" $ do
        route . customRoute $ ("js" </>) . (<.> "js") . dropExtensions . takeFileName . toFilePath
        compile getResourceString

    match ("css/*" .&&. complement "css/*.min.css") $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList [citationStyle]) $ compile cslCompiler
    match ("bibliography.*" .||. "posts/**/bibliography.*") $ compile Bib.compiler

    create ["css/highlight.css"] $ do
        route idRoute
        compile $ makeItem (Kate.styleToCss Kate.kate)

    match "templates/*" $ compile templateCompiler

    match "posts/**" $ version "raw" $ do
        route idRoute
        compile copyFileCompiler

    tags <- sortTagsBy tagOrder <$> 
            buildTags (postsPattern .&&. hasNoVersion) (\tag -> fromFilePath $ "tags" </> tag </> "1.html")

    singlePages <- createPaginate (postsPattern .&&. hasNoVersion)

    multiPages  <- createPaginateWith postParPage 
        (\pn -> if pn == 1 
                then "index.html" 
                else fromFilePath $ "pages" </> show pn <.> "html")
        (postsPattern .&&. hasNoVersion)

    match postsPattern $ version "post list" $ do
        route mempty
        compile $ postCompiler
            >>= loadAndApplyTemplate "templates/post-list.html"
                ( field "url" (return . ("/" </>) . postRoute . itemIdentifier) <>
                  dateField "date" dateFormat <> 
                  defaultContext)

    let postCxt = postContext tags singlePages

    paginateRules singlePages $ \_ _ -> do
        route   $ customRoute postRoute
        compile $ do 
            postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]

            items <- postCompiler

            void $ saveSnapshot "plain" (fmap stripTags items)

            void $ dropAfterMore items >>= postLink >>= saveSnapshot "summary"

            saveSnapshot "raw_post" items
                >>= loadAndApplyTemplate "templates/post.html"    (postCxt postList)
                >>= loadAndApplyTemplate "templates/default.html" (postCxt postList)
                >>= postLink

    paginateRules multiPages $ \pn pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst . filter (matches pat . setVersion Nothing . itemIdentifier) =<<
                     loadAllSnapshots (postsPattern .&&. hasNoVersion) "summary" :: Compiler [Item String]
            postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]

            let paginate = paginateField
                    (\p -> return . fromFilePath $ 
                        if p == 1
                        then "index.html"
                        else "pages" </> show p <.> "html") multiPages pn
                    
                pagesCxt =
                    paginate <>
                    field "title" (\i ->  
                        return $ 
                            if itemIdentifier i == "index.html"
                            then "Home"
                            else "All posts - " ++ show pn) <>
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
                posts <- recentFirst . filter (matches pat' . setVersion Nothing . itemIdentifier) =<<
                         loadAllSnapshots (postsPattern .&&. hasNoVersion) "summary" :: Compiler [Item String]
                postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]

                let paginate = paginateField
                        (\p -> return . fromFilePath $ "tags" </> tag </> show p <.> "html")
                        tagPages pn
                let tagsCxt =
                        constField "subbrand" ("Tag:" ++ tag) <>
                        paginate <>
                        constField "title" (tag ++ " - " ++ show pn)    <>
                        listField "posts" (postCxt postList) (mapM postLink posts) <>
                        globalContext tags postList
                makeItem ""
                    >>= loadAndApplyTemplate "templates/multipost.html" tagsCxt
                    >>= loadAndApplyTemplate "templates/default.html"   tagsCxt

    create ["search.json"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots (postsPattern .&&. hasNoVersion) "plain" :: Compiler [Item String]
            let cxt = field "url" (return . ("/" </>) . postRoute . itemIdentifier) <> postCxt []
            Item "search.json" <$> searchIndex cxt descriptionLength [] posts

    create ["search.html"] $ do
        route idRoute
        compile $ do
            postList <- loadAll (postsPattern .&&. hasVersion "post list") :: Compiler [Item String]
            let template = readTemplate "<script type=\"text/javascript\" src=\"$url$\"></script>"
            scripts  <- loadAll "js/search/*" >>= applyJoinTemplateList "\n" template defaultContext

            let cxt = constField "inheader" scripts    <>
                      constField "subbrand" "Search: " <>
                      postCxt postList
            makeItem ""
                >>= loadAndApplyTemplate "templates/searchResult.html" cxt
                >>= loadAndApplyTemplate "templates/default.html" cxt

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots (postsPattern .&&. hasNoVersion) "raw_post" :: Compiler [Item String]
            let template = readTemplate $ "<url><loc>" ++ rootAddress ++ "$url$</loc></url>"
            fmap (Item "sitemap.xml") (applyJoinTemplateList "\n" template defaultContext posts)
                >>= loadAndApplyTemplate "templates/sitemap.xml" defaultContext

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCxt = bodyField "description" <> (postCxt [])
            posts <- recentFirst =<< loadAllSnapshots (postsPattern .&&. hasNoVersion) "raw_post"
            renderAtom feedConfig feedCxt posts
    
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCxt = bodyField "description" <> (postCxt [])
            posts <- recentFirst =<< loadAllSnapshots (postsPattern .&&. hasNoVersion) "raw_post"
            renderRss feedConfig feedCxt posts

tagOrder :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
tagOrder a@(_,ai) b@(_,bi) = case length bi `compare` length ai of
    EQ -> caseInsensitiveTags a b
    o  -> o

nPages :: Paginate -> Int
nPages = M.size . paginatePages

globalContext :: Tags -> [Item String] -> Context String
globalContext tags pl =
    constField "inheader" "" <>
    constField "subbrand" "" <>
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
  where
    process _ [] = []
    process r url@(h:_)
        | h == '#'                               = "/" </> r ++ url
        | isRelative url && not (isExternal url) = normalise $ "/" </> dropExtension r </> url
        | otherwise                              = url

paginateField :: (Int -> Compiler Identifier) -> Paginate -> Int -> Context String
paginateField f paginate pn = listField "paginate"
    (field "active" (\i ->
        if itemBody i == show pn
        then return "active"
        else fail "") <> defaultContext)
    (mapM (\n -> f n >>= \i -> return $ Item i (show n)) [1 .. nPages paginate])

addClass :: String -> TS.Tag String -> TS.Tag String
addClass cls (TS.TagOpen name attr) = case partition ((== "class") . fst) attr of
    ([],         _)     -> TS.TagOpen name $ ("class", cls) : attr
    ((_,cls'):_, attr') -> TS.TagOpen name $ ("class", cls ++ ' ': cls') : attr'
addClass _ tag = tag

postCompiler :: Compiler (Item String)
postCompiler = cached "site.postCompiler" $ do
    csl <- load citationStyle :: Compiler (Item CSL)
    global <- load "bibliography.bib" :: Compiler (Item Bib.Bib)
    pat    <- fromGlob . (</> "bibliography.*") . takeDirectory . toFilePath <$> getUnderlying
    local  <- mconcat . map itemBody <$> loadAll (pat .&&. hasNoVersion)
    let bib = Item "" $ local <> itemBody global

    fmap ( demoteHeaders . renderTags'
         . concat . snd . mapAccumL addImageLink False
         . map addTableClass . TS.parseTags) <$>
         ( getResourceBody
            >>= readPandocBiblio defaultHakyllReaderOptions csl (Bib.cast bib)
            >>= return . writePandoc)

  where addTableClass tag | TS.isTagOpenName "table" tag = addClass "table" tag
                          | otherwise                    = tag
        addImageLink a i
            | TS.isTagOpenName   "a" i          = (True,  [i])
            | TS.isTagCloseName  "a" i          = (False, [i])
            | TS.isTagOpenName "img" i && not a = (False, addLink i)
            | otherwise                         = (a, [i])
        addLink img@(TS.TagOpen _ attr) = case lookup "src" attr of
            Nothing  -> [img]
            Just src -> [TS.TagOpen "a" [("href", src)], img, TS.TagClose "a"]
        addLink _ = error "addLink: only TagOpen expected."

dropAfterMore :: Item String -> Compiler (Item String)
dropAfterMore item = do
    StringField url <- unContext (urlField "url") "url" item
    return $ renderTags'. (process url). TS.parseTags <$> item
  where
    process _   []                                      = []
    process url (TS.TagComment c:_) | strip c == "more" = [ TS.TagOpen "a" [("href", url), ("class", "readmore")]
                                                          , TS.TagText "read more"
                                                          , TS.TagClose "a"]
    process url (o:ts)                                  = o:process url ts

strip :: String -> String
strip = f . f
  where f = dropWhile (`elem` (" \t" :: String)) . reverse

processCompiler :: FilePath -> [String] -> Item String -> Compiler (Item String)
processCompiler prog opts (Item idnt str) = do
    r <- unsafeCompiler $ readProcessWithExitCode prog opts str
    case r of
        (ExitSuccess  , o, _) -> return $ Item idnt o
        (ExitFailure c, _, e) -> fail $ "processCompiler: " ++ prog ++ ": " ++ e ++ "(exit code: " ++ show c ++ ")"

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

renderTags' :: [TS.Tag String] -> String
renderTags' = TS.renderTagsOptions TS.renderOptions
    { TS.optRawTag   = (`elem` ["script", "style"]) . map toLower
    , TS.optMinimize = (`S.member` minimize) . map toLower
    }
  where
    -- A list of elements which must be minimized
    minimize = S.fromList
        [ "area", "br", "col", "embed", "hr", "img", "input", "meta", "link"
        , "param"
        ]
