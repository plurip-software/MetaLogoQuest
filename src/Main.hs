module Main (main) where

import           Codec.Picture       (readImage)
import           Codec.Picture.Types (DynamicImage)
import           Data                (readIMG, writeIMG)
import           Path                (Extension (..), Path (..), path)

main :: IO ()
main = do
    img <- readIMG pathR
    case img of
        Left _     -> return ()
        Right img' -> writeIMG pathW img'
    where
        pathR :: Path
        pathR = path ["imgs"] "logo" PNG

        pathW :: Path
        pathW = path ["imgs"] "logo_written" PNG
