{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Handlers.AddGroupR where

import Yesod
import Site
import Persist
import Yesod.Form
import Control.Applicative
import Data.Text(Text)

getAddGroupR :: GHandler Site Site RepHtml
postAddGroupR :: GHandler Site Site RepHtml

groupForm = renderDivs
               $ Group
              <$> areq textField "Group Name" Nothing
              <*> areq intField  "Group Id" Nothing

getAddGroupR = do ((_,widget),enc) <- generateFormPost groupForm
                  renderWidget widget enc False
                  
renderWidget widget encoding isError =
             let postRoute = AddGroupR
                 entity    = "Group" :: Text
             in defaultLayout $(whamletFile "templates/form.hamlet")

postAddGroupR = do ((res,widget),enc) <- runFormPost groupForm
                   case res of
                        FormSuccess group -> do runDB $ insert group
                                                defaultLayout $(whamletFile "templates/done.hamlet")
                        _ -> renderWidget widget enc True
