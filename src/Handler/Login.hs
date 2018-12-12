{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import

formLogin :: Form (Text,Text)
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    (widgetForm, enctype) <- generateFormPost formLogin
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
    
postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [UsuariosEmail ==. email,
                                          UsuariosSenha ==. senha] []
            case logado of
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (tshow usrid)
                    setMessage [shamlet|
                        <h1>
                            Usuario logado
                    |]
                    redirect HomeR
                Nothing -> do 
                    setMessage [shamlet|
                        <h1>
                            Usuario e senha n encontrados!
                    |]
                    redirect LoginR
        _ -> redirect LoginR
        
postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_USR"
    redirect HomeR