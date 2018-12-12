{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Arvores where

import Import
import Text.Read (read)

formArvores :: Html -> MForm Handler (FormResult Arvores, Widget)
formArvores = renderBootstrap $ Arvores
    <$> areq doubleField "Latitude" Nothing
    <*> areq doubleField "Longitude" Nothing
    <*> areq (selectField espLista) "Especie: " Nothing
    <*> areq intField "srid: " Nothing
    <*> areq textField "Descricao: " Nothing
    <*> areq textField "Acuracia: " Nothing
    <*> areq textField "Altitude: " Nothing
    <*> areq textField "Acuracia da Altitude: " Nothing

espLista = do
       entidades <- runDB $ selectList [] [Asc EspeciesNomeCientifico] 
       optionsPairs $ fmap (\ent -> (especiesNomeCientifico $ entityVal ent, entityKey ent)) entidades

getArvoresCadastrarR :: Handler Html
getArvoresCadastrarR = do 
    (widget, enctype) <- generateFormPost formArvores
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/arvoresCadastrar.hamlet")
        
postArvoresCadastrarR :: Handler Html
postArvoresCadastrarR = do
    ((res,_),_) <- runFormPost formArvores
    case res of 
        FormSuccess (arvore) -> do
            _ <- runDB . insert $ arvore
            setMessage [shamlet|
                <h1>
                    Arvore cadastrada!
            |]
            redirect ArvoresR
            
getArvoresR :: Handler Html
getArvoresR = do
    arvores <- runDB $ selectList [] []
    prodids <- return $ fmap (\m -> arvoresEspecie $ entityVal m) arvores
    especies <- runDB $ selectList [EspeciesId <-. prodids][]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/arvores.hamlet")
        
postArvoresDeletarR :: ArvoresId -> Handler Html 
postArvoresDeletarR arvore_id = do
    runDB $ delete arvore_id
    redirect ArvoresR