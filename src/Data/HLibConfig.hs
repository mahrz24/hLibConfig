{-# LANGUAGE RankNTypes #-}

module Data.HLibConfig
       ( Setting(..)
       , Value(..)
       , ScalarValue(..)
       , readConfig
       , justScalar
       , valueAtPath
       , boolValue
       , intValue   
       , floatValue
       , stringValue
       ) where

import Data.HLibConfig.Grammar
import Text.Printf
import Data.List.Split
import Data.List

readConfig :: String -> Configuration
readConfig = configParse . lexer

boolValue :: ScalarValue -> Bool
boolValue (ScalarBool a) = a
boolValue (ScalarInt a) = (a /= 0)
boolValue (ScalarFloat a) = (a /= 0)
boolValue (ScalarString a) = (read $ a)

intValue :: ScalarValue -> Int
intValue (ScalarBool a) = if a then 1 else 0
intValue (ScalarInt a) = a
intValue (ScalarFloat a) = round a
intValue (ScalarString a) = (read $ a)

floatValue :: ScalarValue -> Float
floatValue (ScalarBool a) = if a then 1.0 else 0.0
floatValue (ScalarInt a) = fromIntegral a
floatValue (ScalarFloat a) = a
floatValue (ScalarString a) = (read $ a)

stringValue :: ScalarValue -> String
stringValue (ScalarBool a) = show a
stringValue (ScalarInt a) = printf "%i" a
stringValue (ScalarFloat a) = printf "%f" a
stringValue (ScalarString a) = a

justScalar :: Maybe Value -> ScalarValue
justScalar Nothing = ScalarBool False
justScalar (Just (ScalarValue a)) = a

valueAtPath :: Configuration -> String -> Maybe Value 
valueAtPath conf path = 
  let p = (splitOn "." path)
  in  valueAt (lookupSetting conf $ head p) $ tail p
         
lookupSetting :: [Setting] -> String -> Maybe Value
lookupSetting [] _ = Nothing
lookupSetting ((Setting name v):cs) n = 
  if n == name then
    Just v
  else lookupSetting cs n

valueAt :: Maybe Value -> [String] -> Maybe Value
valueAt Nothing _ = Nothing
valueAt (Just val) (p:ps) = 
  case val of 
    ScalarValue _ -> Just val 
    Array a -> Just (ScalarValue $ a !! (index p)) 
    List a -> valueAt (Just (a !! index p)) ps 
    Group g -> valueAt (lookupSetting g p) ps
valueAt (Just val) [] = Just val

index s = read $ delete '[' $ delete ']' s
          


