-- | A simple interface to reading metrics out of a file.
module Hypersphere.Metric.Csv where

import Control.Monad
import Data.Csv
import qualified Data.Vector as V
import Hypersphere.Metric
import Safe
import qualified Data.ByteString.Lazy as LB

instance FromRecord Sample where
    parseRecord v
        | length v == 2 = do
            mts <- readMay <$> v .! 0
            case mts of
                Nothing -> fail "Failed to parse time stamp"
                Just ts -> Sample ts <$> v .! 1
        | otherwise = mzero

instance ToRecord Sample where
    toRecord (Sample ts v) = toRecord (show ts, v)


decodeMetricCsv :: LB.ByteString -> Either String Metric
decodeMetricCsv bs = do
    vec <- V.convert <$> decode NoHeader bs
    case fromVector vec of
        Nothing -> Left "You need at least one metric"
        Just m -> Right m

readMetricCsv :: FilePath -> IO Metric
readMetricCsv fp = do
    bs <- LB.readFile fp
    case decodeMetricCsv bs of
        Left s -> fail s
        Right m -> return m

-- TODO: readMetricCsv needs to have a builtin slicing mechanism for selecting
-- the time range rather than reading it all in to memory

readMetricCsvSlice :: FilePath -> TimeStamp -> TimeStamp -> IO Metric
readMetricCsvSlice fp start stop = do
    m <- readMetricCsv fp
    pure $ sliceMetric m start stop
