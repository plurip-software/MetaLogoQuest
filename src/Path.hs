module Path (Path (..), File (..), Extension (..), Quality(..), path, readQL) where

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

data Quality
    = QL10
    | QL20
    | QL30
    | QL40
    | QL50
    | QL60
    | QL70
    | QL80
    | QL90
    | QL100

instance Show Quality where
    show QL10  = show 10
    show QL20  = show 20
    show QL30  = show 30
    show QL40  = show 40
    show QL50  = show 50
    show QL60  = show 60
    show QL70  = show 70
    show QL80  = show 80
    show QL90  = show 90
    show QL100 = show 100

readQL :: Quality -> Int
readQL = read . show
