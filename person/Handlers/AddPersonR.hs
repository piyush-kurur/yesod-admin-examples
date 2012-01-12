{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handlers.AddPersonR
       ( getAddPersonR
       , postAddPersonR
       ) where

import Yesod
import Yesod.Auth.HashDB
import Data.Text(Text)
import Site
import Persist
import Yesod.Form
import Control.Applicative

getAddPersonR  :: GHandler Site Site RepHtml
postAddPersonR :: GHandler Site Site RepHtml



validatePassword (x,y) | x == y = Left "Password do not match"
                       | otherwise = Right x



personForm groups = renderDivs $ mkPerson
                        <$> areq textField "Name" Nothing
                        <*> areq textField "User name" Nothing
                        <*> areq intField "User ID" Nothing
                        <*> areq (selectField groups) "Group" Nothing
                        <*> areq passwordField "Password" Nothing

mkPerson :: Text 
         -> Text
         -> Int
         -> GroupId
         -> Text
         -> (Person,Text)
mkPerson n un uid g p = (Person { personName = n
                                , personUserName = un
                                , personUid      = uid
                                , personGroup    = g
                                , personShell    = "/bin/bash"
                                },p)
groupEntry (gid,grp) = (groupName grp,gid)
getGroupList :: GHandler Site Site [(Text,GroupId)]
getGroupList =  runDB $ fmap (map groupEntry) $ selectList [] []


getAddPersonR = do groups <- getGroupList
                   ((_,widget),enc) <- generateFormPost $ personForm groups
                   renderWidget widget enc False
                  
renderWidget widget enc isError = 
             defaultLayout [whamlet|
                <h1> Add Person
                $if isError 
                    Bad input
                <form method=post action=@{AddPersonR} enctype=#{enc}>
                      ^{widget}
                      <input type=submit>
               |]


postAddPersonR = do groups <- getGroupList
                    ((res,widget),enc) <- runFormPost $ personForm groups
                    case res of
                       FormSuccess (u,p) -> do user <- setPassword p u
                                               runDB $ insert user
                                               defaultLayout [whamlet|Added user|]
                       _ -> renderWidget widget enc True