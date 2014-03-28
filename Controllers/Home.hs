{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home,
      blog,
--      game
    ) where

import           Views.Home (homeView, blogView)--, gameView)
import           Web.Scotty (ScottyM, get, html)

home :: ScottyM ()
home = get "/" homeView

blog :: ScottyM()
blog = get "/blog" blogView

--game :: ScottyM()
--game = get "/game" gameView
