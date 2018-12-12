{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Equipamentos where

import Import

formEquipamentos :: Form Equipamentos
formEquipamentos = renderBootstrap $ Equipamentos 
    <$> areq textField "Descricao: " Nothing
    <*> areq textField "modelo: " Nothing
    <*> areq textField "Sistema Operacional: " Nothing
    <*> areq textField "Versao: " Nothing
    
getEquipamentosCadastrarR :: Handler Html
getEquipamentosCadastrarR = do 
    (widgetForm, enctype) <- generateFormPost formEquipamentos
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/equipamentosCadastrar.hamlet") 

postEquipamentosCadastrarR :: Handler Html 
postEquipamentosCadastrarR = do 
    ((res,_),_) <- runFormPost formEquipamentos
    case res of 
        FormSuccess (equipamento) -> do
            equip <- runDB $ selectFirst [EquipamentosModelo ==. equipamentosModelo equipamento,
                                        EquipamentosVersaoSO ==. equipamentosVersaoSO equipamento,
                                        EquipamentosSistemaOperacional ==. equipamentosSistemaOperacional equipamento,
                                        EquipamentosDescricao ==. equipamentosDescricao equipamento] []
            case equip of
                Just _ -> do                            
                    setMessage [shamlet|
                        <h1>
                            Equipamento já cadastrado!
                    |]
                    redirect EquipamentosCadastrarR
                Nothing-> do 
                    _ <- runDB $ insert equipamento
                    setMessage [shamlet|
                        <h1>
                            Equipamento cadastrado com sucesso!
                    |]
                    redirect EquipamentosR
        _ -> redirect EquipamentosR
        
getEquipamentosR :: Handler Html
getEquipamentosR = do 
    equipamentos <- runDB $ selectList [] [Asc EquipamentosModelo]
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/equipamentos.hamlet")
        
postEquipamentosDeletarR :: EquipamentosId -> Handler Html 
postEquipamentosDeletarR equipid = do
    runDB $ delete equipid
    redirect EquipamentosR