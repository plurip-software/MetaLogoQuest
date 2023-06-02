module Path (Path (..), Extention (..), path) where

import           Data.List (intercalate)

data Path
    = Path [Directory] File

path :: Dirs -> Filename -> Extention -> Path
path dirs filename =
    Path (map Directory dirs) . File (Name filename)

type Dirs = [String]

type Filename = String

instance Show Path where
    show (Path directories file) =
        intercalate "/" (map show directories) <> "/" <> show file

newtype Directory = Directory String

instance Show Directory where
    show (Directory directory) = directory

data File
    = File Name Extention

instance Show File where
    show (File name extention) =
        show name <> "." <> show extention

newtype Name = Name String

instance Show Name where
    show (Name name) = name

data Extention
    = PNG
    | JPG

instance Show Extention where
    show PNG = "png"
    show JPG = "jpg"