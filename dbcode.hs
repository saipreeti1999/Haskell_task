{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative 
import Crypto
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Crypto (encrypt)
data User = User {username :: String, email :: String, password :: String , userid :: Int } deriving (Show)
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field 
main :: IO ()
main = do
    Prelude.putStrLn" Enter the username , email and password "
    uname <- getLine
    ename <- getLine
    pname <- getLine
    let name = encrypt ename 
    let pass = encrypt pname
    --print name
    --print pass

    conn <-
     connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "052119"
        }
    
    execute conn "insert into public.login (username , email ,password ) values (?,?,?)"  (uname :: String , name :: String, pass :: String)  
    
    
    
    
    --query_ conn "select * from public.login where uname =?" Query
    mapM_ print =<< (query_ conn "select * from public.login" :: IO [User]) 