module Handler.Blog (getBlogR, postBlogR) where

import Import
-- To use Html in forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)

instance YesodNic App

-- View showing list of all articles
getBlogR :: Handler Html
getBlogR = do
    -- Get list of articles from DB
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")

postBlogR :: Handler Html
postBlogR = do
    ((res, articleWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess article -> do
            -- Add article to DB and get its ID
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
        _ -> defaultLayout $ do
            setTitle "Please correct your entry form"
            $(widgetFile "articleAddError")

-- areq is for required form input:
-- areq type label defaultValue
entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq textField "title" Nothing
    <*> areq nicHtmlField "Content" Nothing
