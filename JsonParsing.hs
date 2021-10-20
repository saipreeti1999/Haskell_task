{-# LANGUAGE OverloadedStrings #-}
module JsonParsing where
import qualified Data.Map as M
import Crypto 
import Data.Aeson
import Control.Applicative
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as B

data Users = Users {
      name :: String
    , message  :: String 
    } deriving Show

data Containers = Containers [Users] deriving Show

instance FromJSON Users where
 parseJSON (Object v) =
    Users <$> v .: "name"
          <*> v .: "message"

instance ToJSON Users where
 toJSON (Users name message) =
    object [ "name"  .= name
           , "message"   .= message    
         ]

recur li | (Prelude.null li) = []
         | otherwise = message (Prelude.head li) : recur (Prelude.tail li)

names li | (Prelude.null li) = []
         | otherwise = name (Prelude.head li) : names (Prelude.tail li)

jsonFile :: FilePath
jsonFile = "firsttask.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


main :: IO()
main = do 
   user <- B.readFile jsonFile
   decodemessage (user)
decodemessage  m  = do 
   let deco =   decode m :: Maybe(M.Map String [Users] )
   --print deco
   let just = Prelude.map (\(Just x)->x) [deco] 
   --print just 
   let y = Prelude.head(just) 
   --print y 
   let z = M.lookup "users" y
   --print z 
   let just1 = Prelude.map (\(Just x)->x) [z]
   --print just1 
   let z1 = Prelude.head(just1)
   --print z1 
   let z2 = Prelude.head z1
   --print z2
   let z3 = recur z1
   --print z3 
   let z4 = names z1
   --print z4 
   let z5 = M.fromList (Prelude.zip z4 z3)
   --print z5 
   input <- getLine
   let look = M.lookup input z5
   --print look 
   let look1 = (\(Just x)->x) look
   --print look1 
   let result = decrypt look1
   print result 

