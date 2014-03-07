{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView) where

import           Client.CSS                  (layoutCss)
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
             style $ pet $ toStrict layoutCss
           body $ do
             div ! class_ "hero" $ do
                h1 t
             b
             script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
             script ! src "js/script.min.js" $ mempty

links :: Html
links = p "derp"


homeView :: ActionM ()
homeView = blaze $ layout "Welcome" $ do
             --div ! class_ "how-to is-typeset" $ do
             div ! class_ "row-parent" $ do
               div ! class_ "push12-2 colspan8-5 colspan6-4 colspan2-2 as-grid with-gutter" $ do
                 div ! class_ "col__module--cta" $ do
                   h2 "links"
                   links
                    


