{-# LANGUAGE FlexibleInstances #-}

module Types (Quest(..), Logo(..), Meta(..), Label(..), Image (..), Pixel8, logoFromList) where

import           Codec.Picture        (Image (..), Pixel8,
                                       PixelBaseComponent (..), readImage)
import           Data.List            (intercalate)
import           Data.Vector.Storable (fromList)
import           Path                 (Path, path)

data Quest
    = Quest Logo Meta

instance Show Quest where
    show (Quest logo meta) = showWithLabel (Label "Quest") [show logo, show meta]

newtype Logo = Logo Image'

logoFromList :: [Pixel8] -> Logo
logoFromList pixels =
    Logo $
        Image
            { imageWidth  = 1
            , imageHeight = length pixels
            , imageData   = fromList (pixels :: [Pixel8])
            }

instance Show Logo where
    show (Logo image') = showWithLabel (Label "Logo") [show image']

newtype Meta = Meta [Image']

instance Show Meta where
    show (Meta images) =
        showWithLabel (Label "Meta") [show images]

type Image' = Image Pixel8

instance Show Image' where
  show image' =
    showWithLabel (Label "Image") [show image']

newtype Label = Label String

instance Show Label where
    show (Label label') = label' <> " => "

showWithLabel :: Label -> [String] -> String
showWithLabel label' types' =
    foldl (<>) "" $ show label' : types'
