
ledger-simple
=============

  Simple ledger-cli renderer for Haskell

Installation
------------

  Install with cabal

    cabal install ledger-simple

  It's one file, you could simply copy it into your tree `¯\_(ツ)_/¯`

Example
-------

```haskell
import Data.List.NonEmpty (NonEmpty(..))
import Text.Ledger.Simple
import System.IO (stdout)

transaction = Transaction day "Pizza!" (posting1 :| [posting2])
  where
    posting1 = Posting "-25 CAD" "assets:bank" Nothing
    posting2 = Posting "25 CAD" "expenses:pizza" Nothing
    day      = fromGregorian 2017 9 14

main :: IO ()
main = printTransaction stdout transaction
```



