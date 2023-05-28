{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.ChessComJsonHelper where
import Control.Lens (folded, (^..))
import GHC.Generics (Generic)
import Data.Aeson
import Data.List.Split (splitOn)
import Data.Time
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (Value(..), decode)
import Data.HashMap.Strict (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Aeson.Types (Parser, (.:?), Value, parse,parseMaybe )
import Data.Aeson (Object, (.:?), withObject)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Control.Monad (join)

-- Define a data type that matches the structure of the JSON
newtype ChessAPIArchivesResponse
  = ChessAPIArchivesResponse {archives :: [String]}
  deriving (Show, Generic)

-- Define instances for parsing the JSON
instance FromJSON ChessAPIArchivesResponse

filterArchives :: Integer -> Integer -> Integer -> Integer -> String -> [String]
filterArchives yearFrom monthFrom yearTo monthTo inputString =
  case eitherDecode (pack inputString) of
    Left err ->
      []
    Right response ->
      -- Filter out the archive URLs within the date range
      let filteredUrls = filter isWithinDateRange (archives response)
      in filteredUrls
  where
    isWithinDateRange :: String -> Bool
    isWithinDateRange url =
      let (year, month) = extractYearAndMonthFromURL url
          date = fromGregorian year (fromInteger month) 1
          fromDate = fromGregorian yearFrom (fromInteger monthFrom) 1
          toDate = fromGregorian yearTo (fromInteger monthTo) 1
      in date >= fromDate && date <= toDate

extractYearAndMonthFromURL :: String -> (Integer,Integer)
extractYearAndMonthFromURL  url =
        let parts = splitOn "/" url
            year = read (parts !! 7) :: Integer
            month = read (parts !! 8) :: Integer
        in (year, month)


