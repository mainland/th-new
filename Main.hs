{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Compile

type TExpQ a = Q (TExp a)

main :: IO ()
main = do
  powTExp :: (TExp (Double -> Double)) <- runQ (mkPow 23)
  putStrLn . pprint . unType $ powTExp
  pow <- compile powTExp
  print $ pow 2.0
  print $ pow 10.0

compose :: TExpQ (b -> c) -> TExpQ (a -> b) -> TExpQ (a -> c)
compose f g = [|| $$f . $$g ||]

-- | The staged power function. Given an integer @n@, return a computation in
-- the Q monad that will compute a type Template Haskell expression that is a
-- function that raises its argument to the power @n@.
mkPow :: Num a => Int -> TExpQ (a -> a)
mkPow n_ = [|| \x -> $$(go n_ [|| x ||]) ||]
  where
    go :: Num a => Int -> TExpQ a -> TExpQ a
    go 0 _ = [|| 1 ||]
    go n x = [|| $$x * $$(go (n-1) x) ||]

-- | A more efficient version of the staged power function.
mkPow' :: Num a => Int -> Q (TExp (a -> a))
mkPow' n_ = [|| \x -> $$(go n_ [|| x ||]) ||]
  where
    go :: Num a => Int -> TExpQ a -> TExpQ a
    go 0 _ = [|| 1 ||]
    go 1 x = x
    go n x | even n    = [|| let y = $$(go (n `div` 2) x) in y * y ||]
           | otherwise = [|| let y = $$(go ((n-1) `div` 2) x) in $$x * y * y ||]
