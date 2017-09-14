{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.Ledger.Simple
  ( Account
  , Amount

  , Posting(..)
  , postingAmount
  , postingAccount
  , postingComment

  , Transaction(..)
  , transactionDay
  , transactionDescription
  , transactionPostings

  , renderPostings
  , renderPosting
  , renderTransaction
  , printTransaction
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (intercalate1)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO (hPutStr)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Format
import Formatting
import System.IO (Handle)

-- Data

type Account = Text
type Amount = Text


data Posting = Posting {
                   _postingAmount  :: Amount
                 , _postingAccount :: Account
                 , _postingComment :: Maybe Text
               }
               deriving (Show, Eq)

data Transaction = Transaction {
                     _transactionDay         :: Day
                   , _transactionDescription :: Text
                   , _transactionPostings    :: NonEmpty Posting
                 }
                 deriving (Show, Eq)

type Lens_ s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens s a = Lens_ s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens_ s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

postingAmount :: Lens Posting Amount
postingAmount  = lens _postingAmount  (\p s -> p { _postingAmount  = s })

postingAccount :: Lens Posting Account
postingAccount = lens _postingAccount (\p s -> p { _postingAccount = s })

postingComment :: Lens Posting (Maybe Text)
postingComment = lens _postingComment (\p s -> p { _postingComment = s })

transactionDay :: Lens Transaction Day
transactionDay = lens _transactionDay (\p s -> p { _transactionDay  = s })

transactionDescription :: Lens Transaction Text
transactionDescription =
  lens _transactionDescription (\p s -> p { _transactionDescription = s })

transactionPostings :: Lens Transaction (NonEmpty Posting)
transactionPostings =
  lens _transactionPostings (\p s -> p { _transactionPostings = s })

renderPostings :: NonEmpty Posting -> Builder
renderPostings postings = intercalate1 "\n    " (fmap renderPosting postings)

renderPosting :: Posting -> Builder
renderPosting (Posting amt acct note) =
  let comment = maybe (fromLazyText "") (bprint (" ;" % stext)) note
  in
    bprint (stext % "\t" % stext % builder) acct amt comment

renderTransaction :: Transaction -> Builder
renderTransaction (Transaction day' desc postings) =
  bprint (builder % " " % stext % "\n    " % builder % "\n\n")
         fdate desc renderedPostings
  where
    fdate = bprint string (formatTime defaultTimeLocale "20%_y/%-m/%-d" day')
    renderedPostings = renderPostings postings


-- Misc Posting/Amount operations

printTransaction :: Handle -> Transaction -> IO ()
printTransaction handle = hPutStr handle . toLazyText . renderTransaction
