{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DeriveAnyClass #-}
import Data.Aeson
import qualified Control.Monad.IO.Class as M
import Network.HTTP.Req
import qualified Data.Text as T 
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.Types (Only(Only, fromOnly))
import GHC.Generics
import Database.PostgreSQL.Simple.ToRow
import Data.Aeson.Types
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    fromOnly,
    connect,
    defaultConnectInfo,
    query_,
    execute,
    executeMany,
    query, withTransaction,
  )
import GHC.Cmm.Opt (cmmMachOpFoldM)

data Address = Address {addresstype::String,address::String,pin::Int} deriving Show

data Login = Login {
                     name :: String,
                     email::String,
                     age :: Int,
                     phone::Int,
                     designation::String,
                     addresses :: [Address]
}deriving (Show,Generic)

instance FromJSON Login where
    parseJSON (Object v) =
        Login <$> v .: "name"
              <*> v .: "email"
              <*> v .: "age"
              <*> v .: "phone"
              <*> v .: "designation"
              <*> v .: "addresses"
instance ToRow Login where
  toRow(Login name email age phone designation addresses) = 
    [toField name , toField email, toField age, toField phone , toField designation] 

instance FromJSON Address where
  parseJSON(Object v)=
    Address <$> v .: "addresstype"
            <*> v .: "address"
            <*> v .: "pin"


main::IO()
main = do 
    result <- runReq defaultHttpConfig $ do 
        r <- req GET (https "jsonkeeper.com" /: "b" /: "N32E" ) NoReqBody jsonResponse  mempty 
        return (responseBody r :: [Login])
    --print  result

    conn <-
      connect
         defaultConnectInfo
          { connectDatabase = "postgres",
            connectUser = "postgres",
            connectPassword = "052119"
         }
    withTransaction conn $ mapM_ (func conn)result

    --inp<-executeMany  conn "insert into loginjson (name,email,age,phone,designation) values (?,?,?,?,?)"  (result) 
    --print inp
--func :: Connection -> Login -> IO()
func conn user = do
    r <- query conn "insert into loginjson(name,email,age,phone,designation) values (?,?,?,?,?)returning userid" user
    print(r::[Only Int]) 
    let userID = fromOnly (head r)
    print userID

    mm<- executeMany conn "insert into addressjson(userid,addresstype,address,pin)values(?,?,?,?)"$ Prelude.map (addresswith userID)(addresses user)
    print mm 

addresswith :: Int -> Address -> (Int,String,String,Int)
addresswith userID (Address addresstype address pin) = (userID,addresstype,address,pin)

     --let something = parse (.: "users") result :: Result Array
     --mapM_ print something
    