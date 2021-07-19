-- | This module defines helpers and a means of easily and quickly testing
-- database actions with the @persistent@ database library.
--
-- @since 0.1.0.0
module Test.Hspec.Persistent
    ( SqlSpec
    , SqlTestEnv
    , withDb
    ) where

import Test.Hspec.Persistent.Internal
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Hooks
import Database.Persist.Sql
import Database.Persist.TH

type SqlSpec =
    SpecWith SqlTestEnv

withDb
    :: IO ConnectionPool
    -> SqlSpec
    -> Spec
withDb createConnPool specs =
    beforeAll (mkSqlTestEnv <$> createConnPool) specs

withSqlTestEnv
    :: (SqlTestEnv -> SqlTestEnv)
    -> SqlSpec
    -> SqlSpec
withSqlTestEnv f =
    beforeAllWith
        do pure . f

withSqlTestEnvIO
    :: (SqlTestEnv -> IO SqlTestEnv)
    -> SqlSpec
    -> SqlSpec
withSqlTestEnvIO =
    beforeWith

withHook
    :: SqlAction
    -> SqlSpec
    -> SqlSpec
withHook action =
    withSqlTestEnv $
        modifyHooks \hooks ->
            hooks ++ [action]

withMigrations
    :: [EntityDef]
    -> SqlSpec
    -> SqlSpec
withMigrations entityDefs =
    withHook (runMigration (migrateModels entityDefs))
