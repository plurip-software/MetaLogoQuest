module Path (Path (..), File (..), Extension (..), Quality(..), path) where

import           Data.List (intercalate)

data Path
    = Path [Directory] File

path :: Dirs -> Filename -> Extension -> Path
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
    = File Name Extension

instance Show File where
    show (File name extension) =
        show name <> "." <> show extension

newtype Name = Name String

instance Show Name where
    show (Name name) = name

data Extension
    = PNG
    | JPG Quality

instance Show Extension where
    show PNG     = "png"
    show (JPG _) = "jpg"

newtype Quality = Quality Int

instance Show Quality where
    show (Quality quality') = show quality'