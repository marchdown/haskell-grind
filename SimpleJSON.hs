module SimpleJSON (
                  JValue (..)
                  , getString
                  , getInt
                  , getDouble
                  , getBool
                  , getObject
                  , getArray
                  , isNull
                  ) where
                      
data JValue 
    = JString String
    | JBool Bool
    | JNumber Double
    | JNull
    | JObject [(String, JValue)]
    | JArray [JValue]
    deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber i) = Just (truncate i)
getInt _ = Nothing
-- copypaste
getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull
