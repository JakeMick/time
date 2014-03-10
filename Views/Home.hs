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
                                              form, h1, h2, head, input, li,
                                              link, meta, p, script, style,
                                              title, ul, (!))
import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                              httpEquiv, id, media, name,
                                              placeholder, rel, src, type_)
import           Views.Utils                 (blaze, pet)
import           Web.Scotty                  (ActionM)

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
           pet "<!--[if lt IE 9 ]> <html lang='en-GB' class='no-js lt-ie9'> <![endif]-->"
           pet "<!--[if IE 9 ]> <html lang='en-GB' class='no-js ie9'> <![endif]-->"
           pet "<!--[if (gt IE 9)|!(IE)]><!--> <html lang='en-GB' class='no-js'> <!--<![endif]-->"
           head $ do
             title t
             meta ! charset "utf-8"
             meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
             link ! href "css/screen.css" ! rel "stylesheet"
             link ! href "css/demo.css" ! rel "stylesheet"
             link ! rel "shortcut icon" ! href "favicon.ico"
             meta ! name "description" ! content "Inspire Text"
             meta ! name "viewport" ! content "initial-scale=1"
           body $ do
             div ! class_ "hero" $ do
                h1 t
             b
             siteLinks
             script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
             
siteLinks :: Html
siteLinks = div ! class_ "links" $ do
                link ! href "/home"
                link ! href "/blog"
                link ! href "/game"


homeView :: ActionM ()
homeView = blaze $ layout "home" $ do
             div ! class_ "row-parent" $ do
               div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                 div ! class_ "col__module--cta" $ do
                    h1 "home"                    

blogView :: ActionM ()
blogView = blaze $ layout "blog" $ do
             div ! class_ "row-parent" $ do
               div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                 div ! class_ "col__module--cta" $ do
                    h1 "blog"
                   

gameView :: ActionM ()
gameView = blaze $ layout "game" $ do
             script ! src "//cdnjs.cloudflare.com/ajax/libs/phaser/1.1.5/phaser.min.js" $ mempty
             script ! src "js/script.min.js" $ mempty
             div ! class_ "row-parent" $ do
               div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                 div ! class_ "col__module--cta" $ do
                    h1 "game"

