import Control.Monad

newtype MaybeT m a = MaybeT { runMaybe :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where 
  return x = MaybeT $ return $ Just x
  m >>= f = MaybeT $ do
    x <- runMaybe m
    case x of
      Nothing -> return Nothing
      Just x  -> runMaybe $ f x

m = [[1,2,3,4]
    ,[2,2,3,4]
    ,[3,4,5,2]] 

index :: (Int,Int) ->(Int,Int) ->[[a]] -> [(Int,Int,a)]
index (y0,y1) (x0,x1) ps = do
    (i,xs) <- zip [y0..y1] ps
    (j,x) <- zip [x0..x1] xs
    return (i,j,x)

zipM ::[a] ->[b] ->Maybe [(a,b)]
zipM [] [] = Just []
zipM (a:aa) (b:bb) = case zipM aa bb of
        Nothing ->Nothing
        Just x ->Just $ (a,b):x
zipM _ _ = Nothing

{-
zipM1 (x:xs) (y:ys) = do
    ps <- zipM xs ys
    return $ (x,y):ps
-}

indexM ::(Int,Int) ->(Int,Int) ->[[a]] ->Maybe [(Int,Int,a)]
indexM (y0,y1) (x0,x1) ps = runListT $ do
    (i,xs) <- ListT $ zipM [y0..y1] ps
    (j,x)  <- ListT $ zipM [x0..x1] xs
    return (i,j,x)

  
--zip :: [a] -> [b] -> [(a,b)]
--zip (x:xs) (y:ys) = (x,y) : zip xs ys

newtype ListT m a = ListT { runListT ::m [a] }

{-mapM (a ->m b) ->[m a] ->m [b]
mapM f (m:ms) = do
    s  <- m
    s1 <- f s
    ss <- mapM f xs
    return (s1:ss)

sequence ::[m a] ->m [a]
sequence (m:ms) = do
    x <- m
    xs <- sequence ms
    return (x:xs)

mapM f = sequence . map (\m ->do {s <- m; return (f s)})
-}

instance (Monad m) => Monad (ListT m) where
    return x = ListT $ return [x]
    m >>= f = ListT $ do
        xs <- runListT m
        bs <- forM xs $ \x ->runListT $ f x
        return $ concat bs




