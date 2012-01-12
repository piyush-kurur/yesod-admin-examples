{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE IncoherentInstances        #-}

module Site where

import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB
import Yesod.Admin
import Yesod.Admin.TH
import Persist


import Settings

data Site = Site { connPool :: ConnectionPool}

instance YesodPersist Site where
    type YesodPersistBackend Site = SqlPersist
    runDB f = liftIOHandler $ do
              pool <- fmap connPool getYesod
              runConnPool f pool


-- Admin stuff starts here

instance HasAdminUser Site where
         isSuperUser _ = return True -- every one is a super user.

instance HasAdminLayout Site where  -- The default layout is used

mkYesodAdmin "Site" groupAdmin
mkYesodAdmin "Site" personAdmin



-- and admin stuff ends here.

mkYesodData "Site" $(parseRoutesFile "routes")

instance Yesod Site where
         approot _ = ""
         authRoute _ = Just $ AuthR LoginR




instance HashDBUser (PersonGeneric backend) where
         userPasswordHash = personHash
         userPasswordSalt = personSalt
         setUserHashAndSalt s h p = p { personHash = Just h
                                      , personSalt = Just s
                                      }



instance RenderMessage Site FormMessage where
         renderMessage _ _ = defaultFormMessage

instance YesodAuth Site where
         type AuthId Site  = PersonId
         loginDest  _      = RootR
         logoutDest _      = RootR
         getAuthId         = getAuthIdHashDB AuthR (Just . UniqueUserName)
         authPlugins       = [authHashDB (Just . UniqueUserName)]