module Test.Hspec.Persistent.Postgresql
    (
    )
    where

import qualified Database.Postgres.Temp as Temp
import Database.Persist.Postgresql
import Test.Hspec.Persistent
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Hooks
import Control.Exception

withTmpPostgres
    :: SpecWith Temp.DB
    -> Spec
withTmpPostgres =
    aroundAll \testAction -> do
        ea <- Temp.with testAction
        case ea of
            Left err ->
                throwIO err
            Right a ->
                pure a

