module HaskellImage where

import qualified Data.Map.Strict as Map
import Data.Either
import Graphics.HsExif
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.FilePath.Find
import System.FilePath
import System.Directory
import Data.UUID
import Data.UUID.V4

output = "./output"

main = do
    fs <- find always (extension ==? ".jpg") "./images"
    xs <- images fs
    ys <- imagesByDate xs
    mapM_ (createDirectoryIfMissing True) $ Map.keys ys
    copyImages ys

copyImages :: Map.Map String [String] -> IO ()
copyImages x = mapM_ (>>= uncurry copyFile) ps
    where ps = Map.foldrWithKey (\k vs a -> map (fromToPaths k) vs ++ a) [] x

fromToPaths :: FilePath -> FilePath -> IO (FilePath, FilePath)
fromToPaths k v = (\ p -> (v, p ++ ext)) . replaceFileName currentName <$> nextUUID
    where currentName = replaceDirectory v k
          ext = takeExtension v

nextUUID :: IO String
nextUUID = toString <$> nextRandom

images :: [String] -> IO [(String, Either String (Map.Map ExifTag ExifValue))]
images = mapM (\p -> (\r -> (p, r)) <$> parseFileExif p)

imagesByDate :: [(String, Either String (Map.Map ExifTag ExifValue))] -> IO (Map.Map String [String])
imagesByDate xs = do
    let keys = map (normalise . (output </>) . timestamp) $ rights (map snd xs)
    values <- mapM (\(a, b) -> (:[]) <$> makeAbsolute a) xs
    return $ Map.fromListWith (++) (zip keys values)

timestamp :: Map.Map ExifTag ExifValue -> String
timestamp x = case getDateTimeOriginal x of
                  (Just a) -> formatTime defaultTimeLocale "%B, %Y" a
                  Nothing  -> "NA"