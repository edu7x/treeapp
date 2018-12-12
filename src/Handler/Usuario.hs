{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import

formUsuario :: Form (Usuarios,Text)
formUsuario = renderBootstrap $ (,) 
    <$> (Usuarios 
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing
            <*> areq passwordField "Senha: " Nothing
        )
    <*> areq passwordField "Confirmacao de senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widgetForm, enctype) <- generateFormPost formUsuario
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")
    
postUsuarioR :: Handler Html 
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of 
        FormSuccess (usuario,confirmacao) -> do
           if (usuariosSenha usuario == confirmacao) then do 
                runDB $ insert usuario
                setMessage [shamlet|
                    <h1>
                        Usuario cadastrado com sucesso
                |]
                redirect HomeR
           else do 
                setMessage [shamlet|
                    <h1>
                        Senha e confirmacao não conferem
                |]
                redirect UsuarioR
        _ -> redirect UsuarioR
        