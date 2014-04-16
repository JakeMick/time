{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    , blog
    , kata
    ) where

import           Views.Home (homeView, blogView)
import           Views.Kata (kataView)
import           Web.Scotty (ScottyM, get, html)

home :: ScottyM ()
home = get "/" homeView

blog :: ScottyM()
blog = get "/blog" blogView

kata :: ScottyM()
kata = get "/kata" kataView
