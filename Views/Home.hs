{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView,
                   blogView,
                   gameView) where

import           Data.Monoid                 (mempty)
import           Data.Text.Lazy              (toStrict)
import           Prelude                     hiding (div, head, id, span)
import           Text.Blaze.Html 
import           Text.Blaze.Html5 
import           Text.Blaze.Html5.Attributes hiding (title, label, form, span)
import           Views.Utils                 (blaze, pet)
import           Web.Scotty                  (ActionM)

layout :: Html -> Html -> Html
layout title' content' = docTypeHtml $ do
    head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        title title'
        link ! rel "stylesheet" ! href "css/screen.css"
        script ! src "js/vendor/modernizr.js" $ mempty
    body $ do
        siteLinks
        div ! class_ "row" $ div ! class_ "large-12 columns" $ h1 title'
        content'
        script ! src "js/vendor/jquery.js" $ mempty
        script ! src "js/vendor/fastclick.js" $ mempty

siteLinks :: Html
siteLinks = ul ! class_ "main-menu" $ do
                li $ do
                    a ! href "/" $ "Home"
                    a ! href "/game" $ "Game"
                    a ! href "/blog" $ "Blog"

panelBuilder :: Html -> Html
panelBuilder content' =
        div ! class_ "row" $ div ! class_ "large-12 columns" $ div ! class_ "panel" $ do
            content'

navigableBuilder :: Html
navigableBuilder =
        div ! class_ "row" $ do
            div ! class_ "large-8 medium-8 columns" $ do
                h5 "Here’s your basic grid:"
                div ! class_ "row" $ div ! class_ "large-12 columns" $ div ! class_ "callout panel" $ p $ do
                    strong "This is a twelve column section in a row."
                    "Each of these includes a div.panel element so you can see where the columns are - it's not required at all for the grid."
                div ! class_ "row" $ do
                    div ! class_ "large-6 medium-6 columns" $ div ! class_ "callout panel" $ p "Six columns"
                    div ! class_ "large-6 medium-6 columns" $ div ! class_ "callout panel" $ p "Six columns"
                div ! class_ "row" $ do
                    div ! class_ "large-4 medium-4 small-4 columns" $ div ! class_ "callout panel" $ p "Four columns"
                    div ! class_ "large-4 medium-4 small-4 columns" $ div ! class_ "callout panel" $ p "Four columns"
                    div ! class_ "large-4 medium-4 small-4 columns" $ div ! class_ "callout panel" $ p "Four columns"
                hr
                h5 "We bet you’ll need a form somewhere:"
            div ! class_ "large-4 medium-4 columns" $ do
                h5 "Try one of these buttons:"
                p $ do
                    a ! href "#" ! class_ "small button" $ "Simple Button"
                    br
                    a ! href "#" ! class_ "small radius button" $ "Radius Button"
                    br
                    a ! href "#" ! class_ "small round button" $ "Round Button"
                    br
                    a ! href "#" ! class_ "medium success button" $ "Success Btn"
                    br
                    a ! href "#" ! class_ "medium alert button" $ "Alert Btn"
                    br
                    a ! href "#" ! class_ "medium secondary button" $ "Secondary Btn"
                div ! class_ "panel" $ do
                    h5 "So many components, girl!"
                    p "A whole kitchen sink of goodies comes with Foundation. Checkout the docs to see them all, along with details on making them your own."
                    a ! href "http://foundation.zurb.com/docs/" ! class_ "small button" $ "Go to Foundation Docs"

homeView :: ActionM ()
homeView =  blaze $ layout "home" $ do
                panelBuilder $ do
                    h3 "Welcome to my homepage!"
                    p "This is site hosts miscellaneous toys and trinkets that I find interesting."
                    p "The links are available in the site-nav, but there's three basic parts of the site:"
                    div ! class_ "row" $ do
                        div ! class_ "large-4 medium-4 columns" $ p $ do
                            a ! href "/home" $ "Home page"
                            br
                            "Updates to the site will be listed here."
                        div ! class_ "large-4 medium-4 columns" $ p $ do
                            a ! href "/game" $ "Game"
                            br
                            "Awesome toys and trinkets. Machine learning, maybe an MMO."
                        div ! class_ "large-4 medium-4 columns" $ p $ do
                            a ! href "/blog" $ "Blog"
                            br
                            "Mathematical ranting."

blogView :: ActionM ()
blogView =  blaze $ layout "blog" $ do
                div ! class_ "row" $ do
                    h3 "Welcome to the blog."
                   

gameView :: ActionM ()
gameView =  blaze $ layout "game" $ do
                div ! class_ "row" $ do
                    h1 "game"
                    div ! class_ "game" $ mempty
                script ! src "//cdnjs.cloudflare.com/ajax/libs/phaser/1.1.5/phaser.min.js" $ mempty
                script ! src "js/script.min.js" $ mempty

