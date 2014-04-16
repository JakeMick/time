{-# LANGUAGE OverloadedStrings #-}

module Views.Kata (kataView,
                  ) where
import           Prelude                     hiding (div)
import           Text.Blaze.Html             ((!), toHtml)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (class_)
import           Views.Utils                 (blaze, layout)
import           Web.Scotty                  (ActionM)
import           Data.Monoid                 (mempty)

kataView :: ActionM()
kataView =  blaze $ layout "kata" $ do
                H.div ! class_ "row" $ do
                    H.h3 "Welcome to katas."
                    H.section $ do 
                        H.aside "left"
                        H.article "right"
                        
