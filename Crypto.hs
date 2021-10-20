module Crypto(encrypt,pipeFormat, decrypt, parseInput )where

import qualified Data.Map as Map 
import qualified Data.Maybe as Maybe
import Util
import Text.Read ( readMaybe )


digit = [2,4..]
alpha = ['a'..'z']++['A'..'Z']++ [' ']
 
abc  = zip alpha digit

encryptmap = Map.fromList abc 
decryptmap = Map.fromList (zip digit alpha)


-- to get value of each alphabet 
keyalpha :: Char -> Maybe Int 
keyalpha n = Map.lookup n encryptmap  

encrypt msg  |null encryptList = ""
             | otherwise = foldr1 pipeFormat  encryptList 
  where encryptList = map show $ Maybe.mapMaybe keyalpha msg 
  
pipeFormat string1 string2 = string1 ++"|"++string2

-- DECRYPTION
-- | split string by  eg: | "2|4|6|?" to [2,4,6]
extractCharacters :: String -> [String]
extractCharacters  = splitBy '|'
-- | convert [String] to [Int]
readInteger::String->Maybe Int
readInteger inpString = readMaybe inpString :: Maybe Int
decryptChar :: Int -> Maybe Char
decryptChar c = Map.lookup c decryptmap
-- | return decrypted equivalent of input string
decrypt :: String -> [Char]
decrypt "" = ""
decrypt encryptedString = Maybe.mapMaybe decryptChar listOfTokens
                            where
                                listOfTokens =
                                    Maybe.mapMaybe readInteger (extractCharacters encryptedString)
parseInput :: String -> [Int]
parseInput inpString = Maybe.mapMaybe readInteger (extractCharacters inpString)



 