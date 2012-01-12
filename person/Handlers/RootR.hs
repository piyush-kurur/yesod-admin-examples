{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE IncoherentInstances        #-}


module Handlers.RootR where

import Yesod
import Yesod.Auth
import Yesod.Admin
import Text.Hamlet
import Site

getRootR :: Handler RepHtml

getRootR = do muser <- maybeAuthId
              user <- case muser of
                           Nothing -> return Nothing
                           Just x  -> fmap Just $ inlineDisplay x
              defaultLayout $ addWidget $(whamletFile "templates/root.hamlet")
