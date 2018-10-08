#!/usr/bin/env stack
-- stack script --resolver lts-12.10

{-# LANGUAGE OverloadedStrings,BangPatterns #-}


import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.Types


-- Define a 4 element vector type]
data Vec4 = Vec4 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
    deriving Show


instance Storable Vec4 where
    sizeOf _ = sizeOf (undefined :: CFloat) * 4
    alignment _ = alignment (undefined :: CFloat)

    {-# INLINE peek #-}
    peek p = do
        a <- peekElemOff q 0
        b <- peekElemOff q 1
        c <- peekElemOff q 2
        d <- peekElemOff q 3
        return $ Vec4 a b c d
      where
        q = castPtr p

    {-# INLINE poke #-}
    poke p (Vec4 a b c d) = do
        pokeElemOff q 0 a
        pokeElemOff q 1 b
        pokeElemOff q 2 b
        pokeElemOff q 3 b
      where
        q = castPtr p

-- Functions with Vec4
add :: Vec4 -> Vec4 -> Vec4
{-# INLINE add #-}
add (Vec4 a1 b1 c1 d1) (Vec4 a2 b2 c2 d2) =
    Vec4 (a1+a2) (b1+b2) (c1+c2) (d1+d2)

mult :: Vec4 -> Vec4 -> Vec4
{-# INLINE mult #-}
mult (Vec4 a1 b1 c1 d1) (Vec4 a2 b2 c2 d2) =
    Vec4 (a1*a2) (b1*b2) (c1*c2) (d1*d2)

vsum :: Vec4 -> CFloat
{-# INLINE vsum #-}
vsum (Vec4 a b c d) = a+b+c+d

multList :: Int -> Vector Vec4 -> Vector Vec4
multList !count !src
    | count <= 0 = src
    | otherwise  = multList (count-1) $ V.map (\v -> add (mult v m) a) src
  where
    a = Vec4 0.2 0.1 0.6 1.0
    m = Vec4 0.99 0.7 0.8 0.6


main :: IO ()
main = do
    let v = Vec4 0.1 0.2 0.3 0.4
    putStrLn $ show v



