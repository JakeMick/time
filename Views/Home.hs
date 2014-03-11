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
                                              form, h1, h2, h4, head, header,
                                              input, li, link, meta, nav, p,
                                              script, style, title, ul, (!), footer)
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
            meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
            link ! href "css/screen.css" ! rel "stylesheet"
            link ! rel "shortcut icon" ! href "favicon.ico"
            meta ! name "description" ! content "Inspire Text"
            meta ! name "viewport" ! content "initial-scale=1"
        header ! class_ "global-header" $ do
            title'
        body $ do
            div ! class_ "site-content" $ do
                div ! class_ "hero" $ do
                    h1 title'
                content'
            footer ! class_ "global-footer" $ do
                siteLinks
            script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
             
siteLinks :: Html
siteLinks = sectionBuilder "Links" $ do
                p $ do
                    a ! href "/home" $ "Home "
                    a ! href "/blog" $ "Blog "
                    a ! href "/game" $ "Game "

sectionBuilder :: Html -> Html -> Html
sectionBuilder title' content' = 
        div ! class_ "row-parent" $ do
            div ! class_ "row" $ do
                div ! class_ "row__colspaced" $ do
                    h4 title'
                    content'
                                   


homeView :: ActionM ()
homeView =  blaze $ layout "home" $ do
                div ! class_ "row-parent" $ do
                    div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                        div ! class_ "col__module--cta" $ do
                            h1 "home"                    

blogView :: ActionM ()
blogView =  blaze $ layout "blog" $ do
                div ! class_ "row-parent" $ do
                    div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                        div ! class_ "col__module--cta" $ do
                            h1 "blog"
                   

gameView :: ActionM ()
gameView =  blaze $ layout "game" $ do
                script ! src "//cdnjs.cloudflare.com/ajax/libs/phaser/1.1.5/phaser.min.js" $ mempty
                script ! src "js/script.min.js" $ mempty
                    div ! class_ "row-parent" $ do
                        div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                            div ! class_ "col__module--cta" $ do
                                h1 "game"
