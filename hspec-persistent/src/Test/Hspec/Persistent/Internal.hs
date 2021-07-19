{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Breaking changes to this module's API are not reflected in the PVP
-- version of this package. If you use something from here, please report
-- it to the issue tracker so we can support your need from the public API.
--
-- @since 0.1.0.0
module Test.Hspec.Persistent.Internal where

import Test.Hspec.Core.Spec

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Database.Persist.Sql

data SqlTestEnv = SqlTestEnv
    { sqlTestEnvPool :: ConnectionPool
    , sqlTestEnvHooks :: [SqlAction]
    }

mkSqlTestEnv :: ConnectionPool -> SqlTestEnv
mkSqlTestEnv pool =
    SqlTestEnv
        { sqlTestEnvPool =
            pool
        , sqlTestEnvHooks =
            []
        }

modifyHooks :: ([SqlAction] -> [SqlAction]) -> SqlTestEnv -> SqlTestEnv
modifyHooks f sqlTestEnv =
    sqlTestEnv
        { sqlTestEnvHooks =
            f (sqlTestEnvHooks sqlTestEnv)
        }

type SqlAction =
    SqlExample

type SqlExample =
    SqlPersistT IO ()

instance (m ~ IO, a ~ ()) => Example (SqlPersistT m a) where
    type Arg (SqlPersistT m a) =
        SqlTestEnv

    evaluateExample e p action =
        evaluateExample
            (action $ runWithState (const e) ())
            p
            ($ ())

instance (m ~ IO, a ~ ()) => Example (arg -> SqlPersistT m a) where
    type Arg (arg -> SqlPersistT m a) =
        (arg, SqlTestEnv)

    evaluateExample e p action =
        evaluateExample (action $ uncurry (runWithState e)) p ($ ())

runWithState :: (extra -> SqlPersistT IO a) -> extra -> SqlTestEnv -> IO a
runWithState action extra env = do
    flip runSqlPool (sqlTestEnvPool env) do
        sequence_ $ sqlTestEnvHooks env
        result <- action extra
        transactionUndo
        pure result
