module Functions (showWithLabel, createImage) where

import Data.Types (Quest(..), Logo(..), Meta(..), Label(..), Image (..), Pixel8, logoFromList)

showWithLabel :: Label -> [String] -> String
showWithLabel label' types' =
    foldl (<>) "" $ show label' : types'

createImage :: Dimensions -> Data -> Image
createImage (Dimensions (Width width') (Height height')) (Data data') =
    ImageBasic $
        CP.Image
            { CP.imageWidth  = width'
            , CP.imageHeight = height'
            , CP.imageData   = data'
            }

logoFromList :: Dimensions -> [Pixel8] -> Logo
logoFromList dimensions' =
    Logo . createImage dimensions' . Data . fromList