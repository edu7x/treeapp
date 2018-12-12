{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Especies where

import Import

formEspecies :: Form Especies
formEspecies = renderBootstrap $ Especies 
    <$> areq textField "Nome Popular: " Nothing
    <*> areq textField "Nome Científico: " Nothing

getEspeciesCadastrarR :: Handler Html
getEspeciesCadastrarR = do 
    --gera formulario na widgetForm
    (widgetForm, enctype) <- generateFormPost formEspecies
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/especiesCadastrar.hamlet")    

postEspeciesCadastrarR :: Handler Html 
postEspeciesCadastrarR = do 
    -- ler a informacao digitada
    ((res,_),_) <- runFormPost formEspecies
    case res of 
        FormSuccess (especie) -> do
            esp <- runDB $ selectFirst [EspeciesNomeCientifico ==. especiesNomeCientifico especie,
                                            EspeciesNomePopular ==. especiesNomePopular especie] []
            case esp of
                Just _ -> do
                    setMessage [shamlet|
                        <h1>
                            Especie já cadastrada!
                    |]
                    redirect EspeciesCadastrarR
                Nothing -> do 
                    _ <- runDB $ insert especie
                    setMessage [shamlet|
                        <h1>
                            Especie cadastrada com sucesso!
                    |]
                    redirect EspeciesR
        _ -> redirect EspeciesR
        
getEspeciesR :: Handler Html
getEspeciesR = do 
    especies <- runDB $ selectList [] [Asc EspeciesNomeCientifico]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/especies.hamlet")
        
postEspeciesDeletarR :: EspeciesId -> Handler Html 
postEspeciesDeletarR espid = do
    runDB $ delete espid
    redirect EspeciesR