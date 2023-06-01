{-# LANGUAGE FlexibleInstances #-}

module Types (Quest(..), Logo(..), Meta(..), Label(..), Image (..), Pixel8, logoFromList) where

import           Codec.Picture        (Pixel8, PixelBaseComponent (..),
                                       readImage)
import qualified Codec.Picture        as CP
import Data.Vector.Storable ( Vector(..), fromList )
import           Data.List            (intercalate)
import           Path                 (Path (..), path)

data Quest
    = Quest Logo Meta

instance Show Quest where
    show (Quest logo meta) = showWithLabel (Label "Quest") [show logo, show meta]

newtype Logo = Logo Image

logoFromList :: Dimensions -> [Pixel8] -> Logo
logoFromList dimensions' =
    Logo . createImage dimensions' . Data . fromList

createImage :: Dimensions -> Data -> Image
createImage (Dimensions {width = width', height = height'}) (Data data')  =
    ImageBasic $
        CP.Image
            { CP.imageWidth  = width'
            , CP.imageHeight = height'
            , CP.imageData   = data'
            }

data Dimensions =
    Dimensions 
        { width  :: Int
        , height :: Int
        }

newtype Data = Data (Vector (PixelBaseComponent Pixel8))

instance Show Logo where
    show (Logo image') = showWithLabel (Label "Logo") [show image']

newtype Meta = Meta [Image]

instance Show Meta where
    show (Meta images) =
        showWithLabel (Label "Meta") [show images]

data Image
    = ImageFull Path (CP.Image Pixel8)
    | ImageBasic (CP.Image Pixel8)

instance Show Image where
  show image' =
    showWithLabel (Label "Image") [show image']

newtype Label = Label String

instance Show Label where
    show (Label label') = label' <> " => "

showWithLabel :: Label -> [String] -> String
showWithLabel label' types' =
    foldl (<>) "" $ show label' : types'
