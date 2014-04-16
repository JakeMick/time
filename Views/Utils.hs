{-# LANGUAGE OverloadedStrings #-}

module Views.Utils
    ( blaze
    , pet
    , layout
    , siteLinks
    ) where

import           Data.Monoid                   (mempty)
import           Prelude                       hiding (div, head, id)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (a, body, div, docTypeHtml, h1,
                                                head, header, img, li, link, meta, nav,
                                                script, title, ul, (!))
import           Text.Blaze.Html5.Attributes   (charset, class_, content, href, id,
                                                name, rel, src)
import           Text.Blaze.Internal           (preEscapedText)
import           Web.Scotty                    (ActionM, html)

pet = preEscapedText

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout title' content' = docTypeHtml $ do
    head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        title title'
        link ! rel "stylesheet" ! href "css/screen.css"
        script ! src "js/vendor/modernizr.js" $ mempty
    body $ do
        div ! class_ "title" $ h1 title'
        header ! class_ "navigation" $ do
            div ! class_ "menu-wrapper" $ do
                a ! href "javascript:void(0)" ! class_ "logo" $
                    img ! src "img/ico.png"
                siteLinks
        div ! class_ "content" $ do
                content'
        script ! src "js/site-script.js" $ mempty
        script ! src "js/vendor/jquery.js" $ mempty
        script ! src "js/vendor/fastclick.js" $ mempty



siteLinks :: Html
siteLinks = div ! class_ "nav" $ do
                ul ! id "navigation-menu" $ do
                    li $ a ! href "/" $ "Home"
                    li $ a ! href "/blog" $ "Blog"
                    li $ a ! href "/kata" $ "Katas"
