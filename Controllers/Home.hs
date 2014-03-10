{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home
    ( home
    ) where

import           Views.Home (homeView)
import           Web.Scotty (ScottyM, get, html)

home :: ScottyM ()
home = get "/" homeView
