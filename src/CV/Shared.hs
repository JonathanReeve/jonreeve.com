{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CV.Shared where

import qualified Data.Text as T
import PyF


data Date = Present | Date { year :: Int
                           , month :: Int
                           } deriving Show

data DateRange = DateRange { start :: Date
                           , end :: Date
                           } deriving Show

-- Validate the dates.
date :: Int -> Int -> Date
date y m | y < 1981 || y > 2100 = error "You're probably not alive in that year."
         | m < 1 || m > 12 = error "The month doesn't exist."
         | otherwise = Date y m

formatDate :: Date -> T.Text
formatDate d = let yy = year d
                   mm = month d
               in [fmt|{yy}-{mm}|]

formatDates :: [Date] -> T.Text
formatDates dates = T.intercalate ", " $ map formatDate dates

formatDateRange :: DateRange -> T.Text
formatDateRange dateRange = case dateRange of
  DateRange (Date _ _) Present -> [fmt|({startDate}–)|]
  DateRange (Date startY startM) (Date endY endM) ->
    -- Sometimes a date range starts and ends at the same time.
    -- In that case, we only need to show one of the dates.
    if startY == endY && startM == endM
    then [fmt|({startDate})|]
    else [fmt|({startDate}–{endDate})|]
  DateRange _ _ -> error "Error reading date range."
  where startDate = formatDate $ start dateRange
        endDate = formatDate $ end dateRange

formatDateRanges :: [DateRange] -> T.Text
formatDateRanges dateRanges = T.intercalate ", " $ map formatDateRange dateRanges

-- TODO: get this from Pandoc
type Markdown = T.Text

-- TODO: validate URIs
type URI = T.Text

data Link = Link { url :: URI, text :: T.Text } deriving Show

data Update = Update Date Event deriving Show

data Event = News Markdown
           | Award T.Text Venue
           | Talk Title URI Venue
           | Publication PublicationType Title URI Venue
           deriving Show

type Title = T.Text

data PublicationType = Tutorial | Article | Chapter | Abstract deriving Show

uni :: T.Text -> T.Text
uni abbr = case abbr of "nyu" ->  "New York U"
                        "cu" -> "Columbia U"
                        "bc"  -> "Brooklyn College, City U of New York"
                        "msu" -> "Montclair State U"
                        "ucb" -> "U of California, Berkeley"
                        "buffalo" -> "U at Buffalo"
                        "york" -> "York College, City U of New York"
                        "cuny" -> "City U of New York"
                        _ -> error "Error: unrecognized abbreviation"

data Venue = Venue { name :: T.Text,
                     venueUrl :: URI,
                     location :: T.Text } deriving Show
