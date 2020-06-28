--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


import           Control.Monad                   (liftM)
import           Data.Char                       (isSpace)
import           Data.List                       (dropWhileEnd, sortBy, isPrefixOf, isSuffixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Ord                        (comparing)
import           Hakyll
import           Hakyll.Images                   (compressJpgCompiler,
                                                  loadImage,
                                                  resizeImageCompiler,
                                                  scaleImageCompiler)

import qualified Data.Text                       as T

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109
import qualified GHC.IO.Encoding                 as E

import           Text.Pandoc.Definition          (Pandoc)
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options
import qualified Text.Pandoc.Templates           as Template
import           Text.Pandoc.Walk                (walkM)

import           System.IO
import           System.FilePath                 (takeFileName)

import qualified Data.ByteString.Lazy            as B
import qualified Text.Blaze.Html.Renderer.String as St
import           Text.Blaze.Html.Renderer.Utf8   (renderHtml)

import           BulmaFilter                     (bulmaTransform)
import           Template                        (NavigationLink (..), Schema,
                                                  mkDefaultTemplate,
                                                  tocTemplate)

schema :: Schema
schema = [
    -- Navigation link to home (/index.html) is done over the logo
    -- So no need to include it in the schema
      NavLink "/people.html"           "People"
    , NavLink "/research.html"         "Research"
    , NavLink "/publications.html"     "Publications"
    , Waypoint "Technology"
        [ NavLink "/hardware.html"     "Hardware"
        , NavLink "/software.html"     "Software"
        ]
    , Waypoint "External Links" [
          Title "Institutional Links"
        , ExternalLink "http://www.physics.mcgill.ca/"       "McGill Department of Physics"
        , ExternalLink "https://www.mcgill.ca/chemistry/"    "McGill Department of Chemistry"
        , ExternalLink "http://www.physics.mcgill.ca/cpm/"   "McGill Center for the Physics of Materials"
        , ExternalLink "https://www.mcgill.ca/femr/"         "Facility for Electron Microscopy Research"
        , ExternalLink "http://www.rqmp.ca/?lang=en"         "Regroupement québécois sur les matériaux de pointe"
        , Divider
        , Title "Ultrafast at McGill"
        , ExternalLink "http://www.physics.mcgill.ca/~cooke/Cooke_Lab/News.html" "Ultrafast Terahertz Science (D. Cooke)"
        , ExternalLink "http://kambhampati-group.mcgill.ca/"                     "Ultrafast Spectroscopy (P. Kambhampati)"
        , ExternalLink "https://spm.physics.mcgill.ca/"                          "Nanoscience and Scanning Probe Microscopy (P. Grutter)"
        , Divider
        , Title "Software Documentation"
        , ExternalLink "https://iris-ued.rtfd.io"    "iris"
        , ExternalLink "https://scikit-ued.rtfd.io"  "scikit-ued"
        , ExternalLink "https://npstreams.rtfd.io"   "npstreams"
        , ExternalLink "https://crystals.rtfd.io"    "crystals"
        , Divider
        , Title "GitHub Organization"
        , ExternalLink "https://github.com/Siwick-Research-Group" "Siwick Research Group"
        ]
    , NavLink "/internal/index.html"   "Internal"
    ]

-- Website schema for the internal pages
schemaInternal :: Schema
schemaInternal = [
      NavLink "/internal/index.html"            "Internal"
    , NavLink "/internal/documentation.html"    "Documentation"
    , NavLink "/internal/projects.html"         "Projects Management"
    , NavLink "/internal/reports.html"          "Student reports"
    ]

-- We match images down to two levels
-- Images/* and images/*/**
profiles = "images/profiles/*.jpg" .||. "images/profiles/*.jpeg"
jpgImages = foldr1 (.||.) [ "images/*.jpg"
                          , "images/*/**.jpg"
                          , "images/*.jpeg"
                          , "images/*/**.jpeg"
                          ]
            .&&. (complement profiles)
nonJpgImages = ( "images/*/**" .||. "images/*" ) .&&. complement jpgImages

config :: Configuration
config = defaultConfiguration { destinationDirectory = "_rendered"
                              , ignoreFile = ignoreFile' 
                              }
    where
        ignoreFile' path
            -- Special case for the password protection of internal section
            | fileName == ".htpasswd"      = False
            | fileName == ".htaccess"      = False
            -- Default behavior from Hakyll
            -- https://jaspervdj.be/hakyll/reference/src/Hakyll.Core.Configuration.html#Configuration
            | "."    `isPrefixOf` fileName = True
            | "#"    `isPrefixOf` fileName = True
            | "~"    `isSuffixOf` fileName = True
            | ".swp" `isSuffixOf` fileName = True
            | otherwise                    = False
            where
                fileName = takeFileName path


--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Hakyll can trip on characters like apostrophes
    -- https://github.com/jaspervdj/hakyll/issues/109
    E.setLocaleEncoding E.utf8

    hakyllWith config $ do

        preprocess $ do
            -- We generate the default templates
            B.writeFile "templates/default.html"          $ renderHtml $ mkDefaultTemplate schema ""
            B.writeFile "templates/default-internal.html" $ renderHtml $ mkDefaultTemplate schemaInternal ""

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        -- Personal profiles should be limited in size
        match profiles $ do
            route idRoute
            compile $ loadImage
                >>= scaleImageCompiler 256 256
                >>= compressJpgCompiler 50

        -- JPG images are special: they can be compressed
        -- We exclude profiles
        match jpgImages $ do
            route   idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50

        -- Most other things can be copied directly
        match (nonJpgImages .||. "js/*" .||. "files/**") $ do
            route   idRoute
            compile copyFileCompiler

        -- These are static pages, like the "research" page
        -- Note that /static/index.html is a special case and is handled below
        match ("static/**.md" .&&. complement "static/internal/**.md") $ do
            route $ (setExtension "html") `composeRoutes` staticRoute
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- Internal pages are defined with a different template
        match ("static/internal/**.md") $ do
            route $ (setExtension "html") `composeRoutes` staticRoute
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/default-internal.html" defaultContext
                >>= relativizeUrls
        
        match (fromList ["static/internal/.htaccess", "static/internal/.htpasswd"]) $ do
            route   staticRoute
            compile copyFileCompiler

        match (fromGlob "static/internal/files/**") $ do
            route   staticRoute
            compile copyFileCompiler

        --------------------------------------------------------------------------------
        -- Compile all profiles
        -- If this is not done, we cannot use the metadata in HTML templates
        match ("people/**.md") $ compile $ pandocCompiler_ >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Create a page for all MGAPS executives and officers
        create ["people.html"] $ do
            route idRoute
            compile $ do
                people   <- sortedPositions =<< loadAll (fromGlob "people/*.md")
                students <- sortedPositions =<< loadAll (fromGlob "people/students/*.md")
                alumni   <- recentFirst     =<< loadAll (fromGlob "people/alumni/*.md")

                let profileListCtx = mconcat [
                          listField "people"    defaultContext (return people)
                        , listField "students"  defaultContext (return students)
                        , listField "alumni"    defaultContext (return alumni)
                        , constField "title"    "People"
                        , defaultContext
                        ]

                makeItem ""
                    >>= loadAndApplyTemplate "templates/profiles.html" profileListCtx
                    >>= loadAndApplyTemplate "templates/default.html" profileListCtx
                    >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Home page
        match "static/index.html" $ do
            route staticRoute
            compile $ do
                getResourceBody
                    >>= applyAsTemplate defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Create a sitemap for easier search engine integration
        -- Courtesy of Robert Pearce <https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html>
        create ["sitemap.xml"] $ do
            route   idRoute
            compile $ do
                -- Gather all other pages
                pages <- loadAll (fromGlob "static/*.md")
                let sitemapCtx = listField "pages" defaultContext (return pages)

                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        --------------------------------------------------------------------------------
        match "templates/*" $ compile templateCompiler

-- Sort lists of profiles by position
-- most importantly, Professor should be first
sortedPositions :: MonadMetadata m => [Item a] -> m [Item a]
sortedPositions = sortByM (getPosition . itemIdentifier)
    where
        sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
        sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                        mapM (\x -> liftM (x,) (f x)) xs

        -- Extract the "position: " string from the item
        getPosition :: MonadMetadata m => Identifier -> m Position
        getPosition id' = do
            metadata <- getMetadata id'
            return $ (fromString . fromMaybe "") $ lookupString "position" metadata

-- Using derived Enum instance for ordering
data Position = AssociateProfessor
              | Postdoc
              | PhdStudent
              | MscStudent
              | Undergraduate
              | International
              | Unknown
    deriving (Eq, Enum)

instance Ord Position where
    compare x y = compare (fromEnum x) (fromEnum y)

-- Parse a position string
-- By design, there is no fallback for an unparseable string
fromString :: String -> Position
fromString = (fromString' . trim)
    where
        trim = dropWhileEnd isSpace . dropWhile isSpace
        fromString' "Associate Professor" = AssociateProfessor
        fromString' "Postdoc"             = Postdoc
        fromString' "Ph.D. Student"       = PhdStudent
        fromString' "M.Sc. Student"       = MscStudent
        fromString' "Undergraduate"       = Undergraduate
        fromString' "International"       = International
        fromString' s                     = error ("Unknown position " <> s)

-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
-- Note that the Bulma pandoc filter is always applied last
pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ = do
    ident <- getUnderlying
    toc <- getMetadataField ident "withtoc"
    tocDepth <- getMetadataField ident "tocdepth"
    template <- unsafeCompiler $ (either error id) <$> 
                    Template.compileTemplate mempty (T.pack . St.renderHtml $ tocTemplate)
    let extensions = defaultPandocExtensions
        writerOptions = case toc of
            Just _ -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                , writerTableOfContents = True
                , writerTOCDepth = read (fromMaybe "3" tocDepth) :: Int
                , writerTemplate = Just template
                }
            Nothing -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                }

    pandocCompilerWithTransform defaultHakyllReaderOptions writerOptions bulmaTransform

-- Pandoc extensions used by the compiler
defaultPandocExtensions :: Extensions
defaultPandocExtensions = 
    let extensions = [ 
            -- Pandoc Extensions: http://pandoc.org/MANUAL.html#extensions
            -- Math extensions
              Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros
                -- Code extensions
            , Ext_fenced_code_blocks
            , Ext_backtick_code_blocks
            , Ext_fenced_code_attributes
            , Ext_inline_code_attributes        -- Inline code attributes (e.g. `<$>`{.haskell})
                -- Markdown extensions
            , Ext_implicit_header_references    -- We also allow implicit header references (instead of inserting <a> tags)
            , Ext_definition_lists              -- Definition lists based on PHP Markdown
            , Ext_yaml_metadata_block           -- Allow metadata to be speficied by YAML syntax
            , Ext_superscript                   -- Superscripts (2^10^ is 1024)
            , Ext_subscript                     -- Subscripts (H~2~O is water)
            , Ext_footnotes                     -- Footnotes ([^1]: Here is a footnote)
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions

    in foldr enableExtension defaultExtensions extensions
-- Move content from static/ folder to base folder
staticRoute :: Routes
staticRoute = (gsubRoute "static/" (const ""))
