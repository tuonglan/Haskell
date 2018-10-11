#!/usr/bin/env stack
-- stack script --resolver lts-12.10

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

newtype SimpleFunc a b = SimpleFunc { funF :: (a -> b) }

instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
      where mapFst g (a, b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
      where mapSnd g (a, b) = (a, g b)

instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
    id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x , x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b ,c) d
unsplit = arr . uncurry


main :: IO ()
main = do
    putStrLn "Hello Arrow"
    let f = arr (`div` 2)
    let g = arr (\x -> x*3 + 1)
    
