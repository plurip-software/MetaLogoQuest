module Main(main) where

import Data ()
import Codec.Picture ( readImage )
import Path (path, Extention (PNG))
import Codec.Picture.Types ( DynamicImage )

main :: IO (Either String DynamicImage)
main = do
    img  <- readImage . show $ path ["imgs"] "logo" PNG
    return img
