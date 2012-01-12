{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
module Persist where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Time

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod.Admin
import Yesod


share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "models")

personAdmin  :: AdminInterface Person
groupAdmin :: AdminInterface Group

groupAdmin = (simpleAdmin "Name")
                         { singular = "Group"
                         , plural   = "Groups"
                         , listing  = [ "Name"
                                      , "Gid"
                                      ]
                         }

personAdmin = (simpleAdmin "userInlineDisplay")
                        { singular   = "Person"
                        , plural     = "Persons"
                        , listing    = [ "Name"
                                       , "UserName"
                                       , "Uid"
                                       , "Group"
                                       ]
                        , attributeTitleOverride = [("Uid","User Id")]
                        }
userInlineDisplay :: Person -> GHandler sub master Text
userInlineDisplay v = return $ Text.unwords [ personName v
                                            , Text.concat [ "("
                                                          , personUserName v
                                                          , ")"
                                                          ]
                                            ]