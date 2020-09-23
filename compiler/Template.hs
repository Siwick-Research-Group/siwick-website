{-# LANGUAGE OverloadedStrings #-}

module Template ( mkDefaultTemplate
                , tocTemplate
                , Schema
                , NavigationLink(..)
                ) where

import           Control.Monad               (forM_)
import           Data.List                   (intersperse)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Text.Blaze                  (toMarkup, toValue)

fontAwesomeURL  = "https://use.fontawesome.com/releases/v5.8.2/css/all.css"
bulmaURL        = "/css/siwick-style.css"
-- Although the link below refers to a specific version, the latest.js file will load the most 
-- current version regardless of the one you specified.
-- Note that we only support LaTeX math syntax
mathJaxURL      = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-AMS_CHTML"

type Icon = String
type Link = String

type SocialLink = (Icon, Link, String)

data NavigationLink = NavLink            Link String                  -- ^ Regular link to a page
                    | ExternalLink       Link String                  -- ^ External link that should be opened in a new page
                    | Title                   String                  -- ^ Dropdown title
                    | Waypoint                String [NavigationLink] -- ^ Waypoint to other links
                    | Divider                                         -- ^ Visual divider

type Schema = [NavigationLink]

styleSheets :: [AttributeValue]
styleSheets =
    [ bulmaURL
    , fontAwesomeURL
    ]

-- Wrap the content of a page with a table of content
tocTemplate :: H.Html
tocTemplate = do
    H.div ! class_ "message is-light" $ do
        H.div ! class_ "message-header" $
            H.p $ "On this page:"

        H.div ! class_ "message-body" $
            H.p $ "$toc$"

    "$body$"

defaultHead :: H.Html
defaultHead = H.head $ do
    H.meta ! charset "utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "$title$ - Siwick Research Group"
    -- Style sheets
    forM_ styleSheets (\link -> H.link ! rel "stylesheet" ! type_ "text/css" ! href link)
    -- Bulma helpers
    H.script ! type_ "text/javascript" ! src "/js/navbar-onclick.js" $ mempty
    -- MathJax
    H.script ! type_ "text/javascript" ! src mathJaxURL $ mempty


navigationBar :: Schema -> H.Html
navigationBar links = H.nav ! class_ "navbar is-primary is-fixed-top" $
        H.div ! class_ "container" $ do
            H.div ! class_ "navbar-brand" $ do
                H.a ! class_ "navbar-item" ! href "/index.html" $ H.strong $ "Siwick Research Group"

                -- toggleBurger function defined in js/navbar-onclick.js
                H.span ! class_ "navbar-burger burger" ! A.id "burger" ! A.onclick "toggleBurger()"$ do
                    H.span $ mempty
                    H.span $ mempty
                    H.span $ mempty

            H.div ! class_ "navbar-menu" ! A.id "navbarMenu" $
                H.div ! class_ "navbar-start" $
                    forM_ links renderLink

    where
        renderLink :: NavigationLink -> H.Html
        renderLink (NavLink link title) = H.a ! class_ "navbar-item" ! href (toValue link) $ toMarkup title
        renderLink (Title title)        = H.div ! class_ "navbar-item" $ H.strong $ toMarkup title
        renderLink (ExternalLink link title) = H.a ! class_ "navbar-item" ! href (toValue link) $ toMarkup title
        renderLink (Divider)            = H.hr ! class_ "navbar-divider"
        renderLink (Waypoint title sublinks) = do
                H.div ! class_"navbar-item has-dropdown is-hoverable" $ do
                    -- A Waypoint does not have a liink of its own
                    -- But an anchor <a class="navbar-link">...</a> is still required
                    H.a ! class_ "navbar-link" $ toMarkup title
                    H.div ! class_ "navbar-dropdown is-boxed" $ -- is-boxed makes it easier to see hovering if navbar is transparent
                        forM_ sublinks renderLink


defaultFooter :: String -> H.Html
defaultFooter s = H.footer ! class_ "footer" $
    H.div ! class_ "content has-text-centered" $ do
        H.p $ (mconcat . intersperse " · ") $ [
              "Siwick Research Group"
            , H.a ! href "http://www.mcgill.ca/" $ "McGill University"
            ]
        H.p $ mconcat [
            "To know more about this how this site was created, click "
            , H.a ! href "/about-this-website.html" $ "here"
            , "."
            ]

    where
        renderLink (icon, link, name) = do
            H.span ! class_ "icon" $ H.i ! class_ (toValue icon) $ mempty
            H.a ! target "_blank" ! href (toValue link) $ toMarkup name

-- | Full default template
-- The schema is used to render the navigation bar
-- The templateFooter will be adorned with the message @s@
mkDefaultTemplate :: Schema -> String -> H.Html
mkDefaultTemplate schema s = H.docTypeHtml $ do
    defaultHead
    -- Need to have the following class to HTML body
    -- https://bulma.io/documentation/components/navbar/#fixed-navbar
    H.body ! class_ "has-navbar-fixed-top" $ do
        navigationBar schema
        H.div ! class_ "section" $
            H.div ! class_ "container" $ do
            -- Note : the "content" class handles all barebones HTML tags
            --      https://bulma.io/documentation/elements/content/
                H.h1 ! class_ "title is-1 has-text-primary has-text-centered" $ "$title$"
                H.div ! class_ "content" $ "$body$"

                H.figure ! class_ "image" $ H.img ! src "/images/logo.svg"
        defaultFooter s
