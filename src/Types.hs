{-# LANGUAGE FlexibleInstances #-}

module Types (Quest(..), Logo(..), Image(..), Meta(..)) where

import           Codec.Picture        (Image (..), Pixel8,
                                       PixelBaseComponent (..), readImage)
import           Data.List            (intercalate)
import qualified Data.Vector.Storable as V
import           Path                 (Path, path)

data Quest
    = Quest Logo Meta

instance Show Quest where
    show (Quest logo meta) = "Quest " <> show logo <> " " <> show meta

newtype Logo = Logo Image'

instance Show Logo where
    show (Logo (Image _ _ image')) =
        "Logo : " <>
        show image'

newtype Meta = Meta [Image']

instance Show Meta where
    show (Meta images) =
        "Meta : " <>
        intercalate "," (map show images)

type Image' = Image Pixel8

instance Show Image' where
  show (Image _ _ image') =
    "Image : " <>
    V.foldl (\acc pixel -> acc <> show pixel) "" image'
