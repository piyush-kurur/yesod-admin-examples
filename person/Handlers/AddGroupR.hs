{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handlers.AddGroupR where

import Yesod
import Site
import Persist
import Yesod.Form
import Control.Applicative

getAddGroupR :: GHandler Site Site RepHtml
postAddGroupR :: GHandler Site Site RepHtml

groupForm = renderDivs
               $ Group
              <$> areq textField "Group Name" Nothing
              <*> areq intField  "Group Id" Nothing

getAddGroupR = do ((_,widget),enc) <- generateFormPost groupForm
                  renderWidget widget enc False
                  
renderWidget widget enc isError = 
             defaultLayout [whamlet|
                <h1> Add Group
                $if isError 
                    Bad input
                <form method=post action=@{AddGroupR} enctype=#{enc}>
                      ^{widget}
                      <input type=submit>
               |]


postAddGroupR = do ((res,widget),enc) <- runFormPost groupForm
                   case res of
                        FormSuccess group -> do runDB $ insert group
                                                defaultLayout [whamlet|Added group|]
                        _ -> renderWidget widget enc True