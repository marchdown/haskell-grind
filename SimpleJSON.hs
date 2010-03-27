module SimpleJSON where

data JValue 
    = JString String
    | JBool Bool
    | JNumber Double
    | JNull
    | JObject [(String, JValue)]
    | JArray [JValue]
    deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString JString s = Just s
getString _ = Nothing

getBool :: JValue -> Maybe Bool
getBool JBool b = Just b
getBool _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble JNumber d = Just d
getDouble _ = Nothing




