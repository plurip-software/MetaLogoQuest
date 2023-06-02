module Data (Quest(..), Logo(..), Meta(..), Label(..), Image (..), Pixel8, logoFromList) where

import           Codec.Picture        (Pixel8, PixelBaseComponent (..),
                                       readImage)
import qualified Codec.Picture        as CP
import           Data.Vector.Storable (Vector (..), fromList)
import           Path                 (Path (..))

data Quest
    = Quest Logo Meta

instance Show Quest where
    show (Quest logo meta) = showWithLabel (Label "Quest") [show logo, show meta]

newtype Logo = Logo Image

data Dimensions
    = Dimensions Width Height

newtype Width = Width Int

newtype Height = Height Int

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