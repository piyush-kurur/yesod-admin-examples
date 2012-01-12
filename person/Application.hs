{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Application
    ( withSite
    , withDevelAppPort
    ) where


import Yesod
import Yesod.Auth
import Database.Persist.Sqlite
import Data.Dynamic (Dynamic, toDyn)
import Persist

import Settings
import Site
import Handlers

mkYesodDispatch "Site" resourcesSite


withSite f = withConnPool $ \ p -> do
               runConnPool (runMigration migrateAll) p
               app <- toWaiApp (Site p)
               f app

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn go
    where go :: ((Int,Application) -> IO()) -> IO ()
          go f = withSite $ \ app -> f (port,app)