{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Produtox json
   nome Text
   valor Double
   deriving Show
   
Aluno json
   nome Text 
   sobrenome Text
   cidade Text
   estado Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/produto/cadastro ProdutoR GET POST
/produto/checar/#ProdutoxId ChecarProdR GET
/erro ErroR GET
/aluno/cadastro AlunoR GET POST
/aluno/checar/#AlunoId ChecarAlunoR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------

-- Sempre que preciso um form, sera ncessario
-- funcoes deste tipo
formProd :: Form Produtox
formProd = renderDivs $ Produtox <$>
           areq textField "Nome: " Nothing <*> 
           areq doubleField "Valor: " Nothing
           
formAluno :: Form Aluno
formAluno = renderDivs $ Aluno <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Sobrenome: " Nothing <*>
           areq textField "Cidade: " Nothing <*>
           areq textField "Estado: " Nothing
          

getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProd
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:red;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{ProdutoR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]
        
getAlunoR :: Handler Html
getAlunoR = do
           (widget, enctype) <- generateFormPost formAluno
           defaultLayout $ do
           toWidget [cassius|
               label
                   color:blue;
           |]
           [whamlet|
               <form method=post enctype=#{enctype} action=@{AlunoR}>
                   ^{widget}
                   <input type="submit" value="Enviar">
           |]

postProdutoR :: Handler Html
postProdutoR = do
           ((result, _), _) <- runFormPost formProd
           case result of 
               FormSuccess prod -> (runDB $ insert prod) >>= \piid -> redirect (ChecarProdR piid)
               _ -> redirect ErroR
               
postAlunoR :: Handler Html
postAlunoR = do
           ((result, _), _) <- runFormPost formAluno
           case result of 
               FormSuccess aluno -> (runDB $ insert aluno) >>= \piid -> redirect (ChecarAlunoR piid)
               _ -> redirect ErroR
           
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getChecarProdR :: ProdutoxId -> Handler Html
getChecarProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> #{produtoxNome produto}  
        <p><b> #{show $ produtoxValor produto}
    |]
getChecarAlunoR :: AlunoId -> Handler Html
getChecarAlunoR pid = do
    aluno <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> #{alunoNome aluno}  
        <p><b> #{alunoSobrenome aluno}
        <p><b> #{alunoCidade aluno}  
        <p><b> #{alunoEstado aluno}
    |]



getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]

connStr = "dbname=dd3fcsg1clbh8p host=ec2-54-163-240-97.compute-1.amazonaws.com user=aisjfpndmzxgtn password=1ety-CDDvw9wQzuXH1ssyH4RjN port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
