{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView,
                   blogView,
                   gameView) where

import           Data.Monoid                 (mempty)
import           Data.Text.Lazy              (toStrict)
import           Prelude                     hiding (div, head, id)
import           Text.Blaze.Html             (Html, toHtml)
import           Text.Blaze.Html5            (Html, a, body, button,
                                              dataAttribute, div, docTypeHtml,
                                              form, h1, h2, h3, h4, head, header,
                                              input, li, link, meta, nav, p,
                                              script, section, style, title,
                                              ul, (!), footer)
import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                              httpEquiv, id, media, name,
                                              placeholder, rel, src, type_)
import           Views.Utils                 (blaze, pet)
import           Web.Scotty                  (ActionM)

layout :: Html -> Html -> Html
layout title' content' = docTypeHtml $ do
        head $ do
            title title'
            meta ! charset "utf-8"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
            meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
            link ! href "css/style.min.css" ! rel "stylesheet"
            script ! src "js/vendor/modernizr.js" $ mempty
            link ! rel "shortcut icon" ! href "favicon.ico"
            meta ! name "description" ! content "Inspire Text"
            meta ! name "viewport" ! content "initial-scale=1"
        body $ do
            div ! class_ "row" $ do
                div ! class_ "large-12 columns" $ do
                    h1 title'
            div ! class_ "row" $ do
                div ! class_ "large-12 columns" $ do
                    div ! class_ "panel" $ do
                        content'
            footer ! class_ "global-footer" $ do
                siteLinks
            script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
             
siteLinks :: Html
siteLinks = p $ do
                a ! href "/home" $ "Home "
                a ! href "/blog" $ "Blog "
                a ! href "/game" $ "Game "

homeView :: ActionM ()
homeView =  blaze $ layout "home" $ do
                h3 "Welcome to the homepage!" $ do
                    p "At the bottom, you'll find a list of site links."

blogView :: ActionM ()
blogView =  blaze $ layout "blog" $ do
               h3 "Welcome to the blog." $ do
                    p "This is mostly empty for the moment."
                    p "Dust balls."
                   

gameView :: ActionM ()
gameView =  blaze $ layout "game" $ do
                script ! src "//cdnjs.cloudflare.com/ajax/libs/phaser/1.1.5/phaser.min.js" $ mempty
                script ! src "js/script.min.js" $ mempty
                h1 "game"
