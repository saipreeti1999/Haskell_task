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
    query,
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
    
    dataB <- (query conn "select email from public.login where email = ?" [name]) :: IO [Only String]

    if not(null dataB)

      then putStrLn "Already exists"

    else do
        insertR <- execute conn  "insert into public.login (username, email, password) values (?, ?, ?)"(uname :: String, name :: String, pass :: String)

        if(insertR == 1)
            then putStrLn $ show insertR ++ " new user created " 

        else print "Unable to create user"