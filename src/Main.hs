module Main (main) where

import           Codec.Picture       (readImage)
import           Codec.Picture.Types (DynamicImage)
import           Path                (Extension (..), path)

main :: IO (Either String DynamicImage)
main = do
    img <- readImage . show $ path ["imgs"] "logo" PNG
    return img
