{-# LANGUAGE OverloadedStrings #-}



module HttpJson where


import JsonParsing
import qualified Control.Monad.IO.Class as M
import Data.Aeson
import Network.HTTP.Req
import Data.Aeson.Types


main' :: IO()
main' = do
    result <- runReq defaultHttpConfig $ do 
        r <- req GET (https "jsonkeeper.com" /: "b" /: "HBJE" ) NoReqBody jsonResponse  mempty 
        return (responseBody r :: Object)
    --print result
    let something = parse (.: "users") result :: Result Array  
    mapM_ print something 
    --decodemessage result 
    
