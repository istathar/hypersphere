-- |
-- Description: Health check functions and status types.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Hypersphere.Check
    ( check
    , Check()
    , Reason(..)
    , Status
    , runChecks
    , getReasons
    , formatReason
    , okStatus
    , CheckL(..)
    , TaggedCheck(..)
    , readCheckBool
    , runCheck
    , getRequiredMetricsNames
    , taggedCheck
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.Char (toLower, isAlpha)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as B
import Data.Word
import GHC.Generics
import Data.Attoparsec.Text as P
import Web.HttpApiData

-- | The status of a service is represented as a @Set@ of reasons why
-- the service is considered "down". An empty set corresponds to the
-- service being up.

newtype Status = Status { getReasons :: Set Reason }
    deriving (Eq, Ord, Show, Monoid, Generic)

instance ToJSON Status
instance FromJSON Status

data TaggedCheck = TaggedCheck Text (CheckL Bool)

taggedCheck :: Map Text Double -> TaggedCheck -> Check ()
taggedCheck m (TaggedCheck t c) = case runCheck m c of
    Nothing -> check "Can't run check" False
    Just b  -> check t b

instance ToHttpApiData TaggedCheck where
    toQueryParam (TaggedCheck t c) = toQueryParam c <> ":" <> toQueryParam t

instance FromHttpApiData TaggedCheck where
    parseQueryParam t = case Text.splitOn ":" t of
        [a,reason] -> TaggedCheck reason <$> parseQueryParam a
        _          -> Left "Failed to parse reason"
    

-- | A language for checks. This can be used to dynamically create checks
-- based on a `Map` of metric values.
data CheckL a where
    CheckAdd    :: CheckL Double -> CheckL Double -> CheckL Double
    CheckSub    :: CheckL Double -> CheckL Double -> CheckL Double
    CheckDiv    :: CheckL Double -> CheckL Double -> CheckL Double
    CheckLT     :: CheckL Double -> CheckL Double -> CheckL Bool
    CheckGT     :: CheckL Double -> CheckL Double -> CheckL Bool
    CheckAnd    :: CheckL Bool   -> CheckL Bool   -> CheckL Bool
    CheckConst  :: Double -> CheckL Double
    CheckBool   :: Bool   -> CheckL Bool
    CheckMetric :: Text   -> CheckL Double

instance ToHttpApiData (CheckL a) where
    toQueryParam = ppCheckL

instance ParseCheck (CheckL a) => FromHttpApiData (CheckL a) where
    parseQueryParam = first Text.pack . readCheck

execCheck :: Map Text Double -> Text -> Maybe Bool
execCheck metrics prog =
    let
        compiled = MaybeT $ pure $ case readCheckBool prog of
            Left  _ -> Nothing
            Right a -> Just a

    in runReader (runMaybeT (compiled >>= runCheckL)) metrics

runCheck :: Map Text Double -> CheckL a -> Maybe a
runCheck m c = runReader (runMaybeT (runCheckL c)) m

runCheckL :: CheckL a -> MaybeT (Reader (Map Text Double)) a
runCheckL (CheckAdd a b) = runBinOp (+) a b
runCheckL (CheckSub a b) = runBinOp (-) a b
runCheckL (CheckDiv a b) = runBinOp (/) a b
runCheckL (CheckLT a b)  = runBinOp (<) a b
runCheckL (CheckGT a b)  = runBinOp (>) a b
runCheckL (CheckAnd a b) = runBinOp (&&) a b
runCheckL (CheckBool a)  = pure a
runCheckL (CheckConst a) = pure a
runCheckL (CheckMetric t) = MaybeT $ do
    n <- ask
    pure $ Map.lookup t n

runBinOp :: (a -> b -> c) -> CheckL a -> CheckL b -> MaybeT (Reader (Map Text Double)) c
runBinOp f a b = f <$> runCheckL a <*> runCheckL b

getRequiredMetricsNames :: CheckL a -> [Text]
getRequiredMetricsNames = execWriter . getNames
  where
    getNames :: CheckL a -> Writer [Text] ()
    getNames (CheckAdd a b) = binOp a b
    getNames (CheckSub a b) = binOp a b
    getNames (CheckDiv a b) = binOp a b
    getNames (CheckLT  a b) = binOp a b
    getNames (CheckGT  a b) = binOp a b
    getNames (CheckAnd a b) = binOp a b
    getNames (CheckBool _)  = pure ()
    getNames (CheckConst _) = pure ()
    getNames (CheckMetric t) = tell [t]

    binOp a b = getNames a *> getNames b

ppCheckL :: CheckL a -> Text
ppCheckL (CheckLT a b) = ppBinOp a b "<"
ppCheckL (CheckGT a b) = ppBinOp a b ">"
ppCheckL (CheckMetric t) = t
ppCheckL (CheckAdd a b) = ppBinOp a b "+"
ppCheckL (CheckSub a b) = ppBinOp a b "-"
ppCheckL (CheckDiv a b) = ppBinOp a b "/"
ppCheckL (CheckAnd a b) = ppBinOp a b "&"
ppCheckL (CheckBool a)  = Text.pack $ toLower <$> show a
ppCheckL (CheckConst a) = Text.pack $ show a

ppBinOp :: CheckL a -> CheckL b -> Text -> Text
ppBinOp a b s = "(" <> ppCheckL a <> s <> ppCheckL b <> ")"


readCheck :: ParseCheck (CheckL a) => Text -> Either String (CheckL a)
readCheck = P.parseOnly (parseCheck <* P.endOfInput)

readCheckBool :: Text -> Either String (CheckL Bool)
readCheckBool = readCheck

type CheckParser = P.Parser

class ParseCheck a where
    parseCheck :: CheckParser a

instance ParseCheck (CheckL Bool) where
    parseCheck
        =   parseConst
        <|> parseBinOp CheckAnd "&"
        <|> parseBinOp CheckLT "<"
        <|> parseBinOp CheckGT ">"
      where
        parseConst = P.choice
            [ P.string "true" *> pure (CheckBool True)
            , P.string "false" *> pure (CheckBool False)
            ]

instance ParseCheck (CheckL Double) where
    parseCheck
        =   parseConst
        <|> parseMetricName
        <|> parseBinOp CheckAdd "+" 
        <|> parseBinOp CheckSub "-" 
        <|> parseBinOp CheckDiv "/" 
      where
        parseConst = CheckConst <$> P.rational

parseBinOp :: (ParseCheck (CheckL b), ParseCheck (CheckL a)) => (CheckL a -> CheckL b -> CheckL c) -> Text -> CheckParser (CheckL c)
parseBinOp f op = do
    P.char '('
    a <- parseCheck
    P.string op
    b <- parseCheck
    P.char ')'
    pure $ f a b

parseMetricName :: CheckParser (CheckL Double)
parseMetricName =
    CheckMetric <$> P.takeWhile isAlpha

-- | The value that represents the OK status.
okStatus :: Status
okStatus = Status mempty

-- | Create a @Status@ from a single @Reason@
singleton :: Reason -> Status
singleton = Status . Set.singleton

-- | A @Reason@ why the service is considered down. A reason has an ID which
-- corresponds to the order in which the health check was defined, and a
-- text describing the reason.
data Reason = Reason
    { reasonId   :: {-# UNPACK #-} !Word
    -- TODO: Check that the Text here is not causing us performance grief.
    , reasonText :: !Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Reason where
    toJSON = String . formatReason

instance FromJSON Reason where
    parseJSON = withText "Reason" $ \t -> do
        let
            r = parseOnly (
                    Reason
                        <$> decimal <* string " - "
                        <*> takeText <* endOfInput
                ) t
        case r of
            Right a -> return a
            Left err -> fail "A Reason should be formated as '<integer> - <reason>'"

-- | The Check monad allows us to collect health predicates and tag them.
newtype Check a = Check { unCheck :: StateT Word (Writer Status) a }
    deriving (Functor, Applicative, Monad)

-- | Perform a health check with the given name. The check fails if the @Bool@
-- is @False@
check :: Text -> Bool -> Check ()
check tag p = Check $ do
    index <- id <+= 1
    unless p $ tell $ singleton $ Reason index tag

-- | Run a @Check@ getting the @Status@
runChecks :: Check () -> Status
runChecks = execWriter . flip evalStateT 0  . unCheck

-- | Pretty print a @Reason@
formatReason :: Reason -> Text
formatReason Reason{..} = LText.toStrict . B.toLazyText
    $ B.fromString (show reasonId)
    <> B.fromText " - "
    <> B.fromText reasonText
