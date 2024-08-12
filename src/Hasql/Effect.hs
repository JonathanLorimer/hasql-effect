-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasql.Effect where

import Prelude hiding (Monad(..), Applicative(..))
import Control.Monad qualified as M
import Control.Applicative qualified as A
import Data.Kind (Type)
import Hasql.Pool (Pool, use, UsageError)
import Hasql.Session (Session, SessionError(..))
import Hasql.Session qualified as S
import Hasql.Statement (Statement)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data (Proxy (..))
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as T
import Data.Functor.Contravariant
import Control.Monad.Free (Free (..), foldFree)

class (forall k. Functor (m k)) => EffA (m :: k -> Type -> Type) where
  type Empty m :: k
  type Append m (t :: k) (t' :: k) :: k

  pure  :: a -> m (Empty m) a
  (<*>) :: m t (a -> b) -> m t' a -> m (Append m t t') b

class EffA m => EffM (m :: k -> Type -> Type) where
  return :: a -> m (Empty m) a
  return = pure

  (>>=) :: m t a -> (a -> m t' b) -> m (Append m t t') b

  (>>) :: m t a -> m t' b -> m (Append m t t') b
  x >> y = x >>= const y

----

data ManipulationKind = Read | Write

data SessionMode = Command | Transaction

data HasqlGrade = HasqlGrade ManipulationKind SessionMode

class HasConnections m where
  readConn :: m Pool
  writeConn :: m Pool

newtype HasqlEff (k :: HasqlGrade) result = HasqlEff { getFree :: Free (Statement ()) result } 
  deriving newtype (Functor, A.Applicative, M.Monad)

instance EffA HasqlEff where
  type Empty HasqlEff = 'HasqlGrade 'Read 'Command

  -- TODO: Figure these semantics out lol
  type Append HasqlEff ('HasqlGrade 'Read s)  ('HasqlGrade 'Read s)  = 'HasqlGrade 'Read s
  type Append HasqlEff ('HasqlGrade 'Write s) ('HasqlGrade _ s)      = 'HasqlGrade 'Write 'Transaction
  type Append HasqlEff ('HasqlGrade _ s)      ('HasqlGrade 'Write s) = 'HasqlGrade 'Write 'Transaction

  pure :: a -> HasqlEff ('HasqlGrade 'Read 'Command) a
  pure = HasqlEff . Pure

  (<*>) :: HasqlEff (k :: HasqlGrade) (a -> b) -> HasqlEff (k' :: HasqlGrade) a -> HasqlEff (Append HasqlEff k k') b
  HasqlEff f <*> HasqlEff a = HasqlEff $ f A.<*> a

instance EffM HasqlEff where
  return :: a -> HasqlEff (Empty HasqlEff) a
  return = pure

  (>>=) :: HasqlEff t a -> (a -> HasqlEff t' b) -> HasqlEff (Append HasqlEff t t') b
  (HasqlEff a) >>= f = HasqlEff $ a M.>>= (getFree . f)

read
  :: forall (a :: SessionMode) result.
     Statement () result
  -> HasqlEff ('HasqlGrade 'Read a) result
read s = HasqlEff . Free $ Pure <$> s

write
  :: forall (a :: SessionMode) result.
     Statement () result
  -> HasqlEff ('HasqlGrade 'Write a) result
write s = HasqlEff . Free $ Pure <$> s

class GetManipulationKind (k :: HasqlGrade) where
  manipulationKind :: Proxy k -> ManipulationKind

class GetSessionMode (k :: HasqlGrade) where
  sessionMode :: Proxy k -> SessionMode

instance GetSessionMode ('HasqlGrade m 'Command) where
  sessionMode _ = Command

instance GetSessionMode ('HasqlGrade m 'Transaction) where
  sessionMode _ = Transaction

instance GetManipulationKind ('HasqlGrade 'Read s) where
  manipulationKind _ = Read

instance GetManipulationKind ('HasqlGrade 'Write s) where
  manipulationKind _ = Write

run
  :: forall k m r. 
    ( HasConnections m
    , MonadIO m
    , GetManipulationKind k
    , GetSessionMode k
    )
  => HasqlEff (k :: HasqlGrade) r
  -> m (Either UsageError r)
run (HasqlEff e) = do
  c <- case manipulationKind (Proxy @k) of
        Read -> readConn
        Write -> writeConn
  let natTrans :: forall x. Statement () x -> Session x
      natTrans = case sessionMode (Proxy @k) of
              Command -> S.statement ()
              Transaction -> T.transaction T.Serializable T.Write . T.statement ()
  liftIO . use c $ foldFree natTrans e


