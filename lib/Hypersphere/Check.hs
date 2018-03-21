-- |
-- Description: Health check functions and status types.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Attoparsec.Text
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as B
import Data.Word
import GHC.Generics

-- | The status of a service is represented as a @Set@ of reasons why
-- the service is considered "down". An empty set corresponds to the
-- service being up.

newtype Status = Status { getReasons :: Set Reason }
    deriving (Eq, Ord, Show, Monoid, Generic)

instance ToJSON Status
instance FromJSON Status

-- | Range based checks
data RangedCheck a = Thresholds
    { red    :: a
    , orange :: a
    , green  :: a
    }


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
