{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative 
import Control.Monad
import qualified Data.Text as Text
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
import Data.Time (UTCTime)
data Address = Address {address:: Maybe String,pincode::Maybe String,state :: Maybe String,city :: Maybe String,addresstype:: String,createddate :: UTCTime,modifieddate::UTCTime}deriving (Show)
data User = User {username :: String, userid ::Int,addrs::[Address] } deriving (Show)

main :: IO ()
main = do
    
 
    
    name <- getLine
    conn <-
     connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "052119"
        }
    

    resp <- query conn
             "select username,login.userid,address,pincode,state,city,addresstype,createddate,modifieddate from login inner join address on login.userid = address.userid where username = ?" [name]
    let addres=fmap(\(_,_,address,pincode,state,city,addresstype,createddate,modifieddate)->Address address pincode state city addresstype createddate modifieddate) resp
    let user =( \(username,userid,_,_,_,_,_,_,_)->User username userid addres) $ head resp
    forM_ (address,user) $ \addr ->print addr
    --print addres
    --print user
    