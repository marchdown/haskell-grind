module Main () where

import SimpleJSON

--wicked var
o1 = JObject [ ("foo", JNumber 1), ("bar", JBool True) ]

main = print (o1)