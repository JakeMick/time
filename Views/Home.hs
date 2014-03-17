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
        link ! rel "stylesheet" ! href "css/style.min.css"
        script ! src "js/vendor/modernizr.js" $ mempty
    body $ do
        div ! class_ "row" $ div ! class_ "large-12 columns" $ h1 title'
        div ! class_ "row" $ div ! class_ "large-12 columns" $ div ! class_ "panel" $ do
            h3 "We’re stoked you want to try Foundation!"
            p "To get going, this file (index.html) includes some basic styles you can modify, play around with, or totally destroy to get going."
            p "Once you've exhausted the fun in this document, you should check out:"
            div ! class_ "row" $ do
                div ! class_ "large-4 medium-4 columns" $ p $ do
                    a ! href "http://foundation.zurb.com/docs" $ "Foundation Documentation"
                    br
                    "Everything you need to know about using the framework."
                div ! class_ "large-4 medium-4 columns" $ p $ do
                    a ! href "http://github.com/zurb/foundation" $ "Foundation on Github"
                    br
                    "Latest code, issue reports, feature requests and more."
                div ! class_ "large-4 medium-4 columns" $ p $ do
                    a ! href "http://twitter.com/foundationzurb" $ "@foundationzurb"
                    br
                    "Ping us on Twitter if you have questions. If you build something with this we'd love to see it (and send you a totally boss sticker)."
        div ! class_ "row" $ do
            div ! class_ "large-8 medium-8 columns" $ do
                h5 "Here’s your basic grid:"
                --  Grid Example 
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
                form $ do
                    div ! class_ "row" $ div ! class_ "large-12 columns" $ do
                        label "Input Label"
                        input ! type_ "text" ! placeholder "large-12.columns"
                    div ! class_ "row" $ do
                        div ! class_ "large-4 medium-4 columns" $ do
                            label "Input Label"
                            input ! type_ "text" ! placeholder "large-4.columns"
                        div ! class_ "large-4 medium-4 columns" $ do
                            label "Input Label"
                            input ! type_ "text" ! placeholder "large-4.columns"
                        div ! class_ "large-4 medium-4 columns" $ div ! class_ "row collapse" $ do
                            label "Input Label"
                            div ! class_ "small-9 columns" $ input ! type_ "text" ! placeholder "small-9.columns"
                            div ! class_ "small-3 columns" $ span ! class_ "postfix" $ ".com"
                    div ! class_ "row" $ div ! class_ "large-12 columns" $ do
                        label "Select Box"
                        select $ do
                            option ! value "husker" $ "Husker"
                            option ! value "starbuck" $ "Starbuck"
                            option ! value "hotdog" $ "Hot Dog"
                            option ! value "apollo" $ "Apollo"
                    div ! class_ "row" $ do
                        div ! class_ "large-6 medium-6 columns" $ do
                            label "Choose Your Favorite"
                            input ! type_ "radio" ! name "pokemon" ! value "Red" ! id "pokemonRed"
                            label ! for "pokemonRed" $ "Radio 1"
                            input ! type_ "radio" ! name "pokemon" ! value "Blue" ! id "pokemonBlue"
                            label ! for "pokemonBlue" $ "Radio 2"
                        div ! class_ "large-6 medium-6 columns" $ do
                            label "Check these out"
                            input ! id "checkbox1" ! type_ "checkbox"
                            label ! for "checkbox1" $ "Checkbox 1"
                            input ! id "checkbox2" ! type_ "checkbox"
                            label ! for "checkbox2" $ "Checkbox 2"
                    div ! class_ "row" $ div ! class_ "large-12 columns" $ do
                        label "Textarea Label"
                        textarea ! placeholder "small-12.columns" $ mempty
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
        script ! src "js/vendor/jquery.js" $ mempty
        script ! src "js/foundation.min.js" $ mempty
        script "$(document).foundation();"

siteLinks :: Html
siteLinks = p $ do
                a ! href "/home" $ "Home "
                a ! href "/blog" $ "Blog "
                a ! href "/game" $ "Game "

homeView :: ActionM ()
homeView =  blaze $ layout "home" $ do
                h3 "Welcome to the homepage!"

blogView :: ActionM ()
blogView =  blaze $ layout "blog" $ do
               h3 "Welcome to the blog."
                   

gameView :: ActionM ()
gameView =  blaze $ layout "game" $ do
                script ! src "//cdnjs.cloudflare.com/ajax/libs/phaser/1.1.5/phaser.min.js" $ mempty
                script ! src "js/script.min.js" $ mempty
                h1 "game"
