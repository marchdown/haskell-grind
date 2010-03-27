data JValue 
    = JString String
    | JBool Bool
    | JNumber Double
    | JNull
    | JObject [(String, JValue)]
    | JArray [JValue]
deriving (Eq, Ord, Show)