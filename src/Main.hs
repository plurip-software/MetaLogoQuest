module Main (main) where

import           Codec.Picture       (readImage)
import           Codec.Picture.Types (DynamicImage)
import           Path                (Extention (PNG), path)

main :: IO (Either String DynamicImage)
main = do
    img <- readImage . show $ path ["imgs"] "logo" PNG
    return img
