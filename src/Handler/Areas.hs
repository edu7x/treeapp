{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Areas where

import Import
import Text.Julius

formAreas :: Form Areas
formAreas = renderBootstrap $ Areas
    <$> areq textField "Descricao: " Nothing
    <*> areq textField "Latitude 1: " Nothing
    <*> areq textField "Longitude 1: " Nothing
    <*> areq textField "Latitude 2: " Nothing
    <*> areq textField "Longitude 2: " Nothing
    <*> areq textField "Latitude 3: " Nothing
    <*> areq textField "Longitude 3: " Nothing
    
getAreasCadastrarR :: Handler Html
getAreasCadastrarR = do 
    (widgetForm, enctype) <- generateFormPost formAreas
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        toWidgetHead $(juliusFile "templates/areasCadastrar.julius")
        $(whamletFile "templates/areasCadastrar.hamlet") 

postAreasCadastrarR :: Handler Html 
postAreasCadastrarR = do 
    ((res,_),_) <- runFormPost formAreas
    case res of 
        FormSuccess (area) -> do
            are <- runDB $ selectFirst [AreasDescricao ==. areasDescricao area
                                        ] []
            case are of
                Just _ -> do                            
                    setMessage [shamlet|
                        <h1>
                            Area já cadastrada!
                    |]
                    redirect AreasCadastrarR
                Nothing-> do 
                    _ <- runDB $ insert area
                    setMessage [shamlet|
                        <h1>
                            Area cadastrada com sucesso!
                    |]
                    redirect AreasCadastrarR
        _ -> redirect AreasCadastrarR

getAreasR :: Handler Html
getAreasR = do 
    areas <- runDB $ selectList [] [Asc AreasDescricao]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/areas.hamlet")
        
postAreasDeletarR :: AreasId -> Handler Html 
postAreasDeletarR area_id = do
    runDB $ delete area_id
    redirect AreasR