{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView,
                   blogView,
                  ) where

import           Text.Blaze.Html             ((!))
import           Text.Blaze.Html5            (a, br, div, h3, p, section)
import           Text.Blaze.Html5.Attributes (class_, href)
import           Views.Utils                 (blaze, layout)
import           Views.Kata                  (kataView)
import           Web.Scotty                  (ActionM)
import           Prelude                     hiding (div, head)

homeView :: ActionM ()
homeView =  blaze $ layout "home" $ do
                h3 "Welcome to my Haskell Katas!"
                p "Browse around and try out Haskell."
                section $ do
                    section ! class_ "item" $ p $ do
                        a ! href "/" $ "Home page"
                        br
                        "head haskatas"
                    section ! class_ "item" $ p $ do
                        a ! href "/blog" $ "Blog"
                        br
                        "There has to be a blog."
                    section ! class_ "item" $ p $ do
                        a ! href "/kata" $ "Haskatas"
                        br
                        "Katas for Haskell."


blogView :: ActionM ()
blogView =  blaze $ layout "blog" $ do
                div ! class_ "item" $ do
                    h3 "Welcome to the blog."
