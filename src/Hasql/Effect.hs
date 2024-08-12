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

import Prelude hiding (Monad(..), Applicative(..), read)
import Control.Monad qualified as M
import Control.Applicative qualified as A
import Data.Kind (Type)
import Hasql.Pool (Pool, use, UsageError)
import Hasql.Session (Session, SessionError(..), pipeline)
import Hasql.Session qualified as S
import Hasql.Statement (Statement)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data (Proxy (..))
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as T
import Data.Functor.Contravariant
import Control.Monad.Free (Free (..), foldFree)
import qualified Hasql.Pipeline as P
import Data.Word (Word8)

class (forall k. Functor (m k)) => EffA (m :: k -> Type -> Type) where
  type EmptyA m :: k
  type AppendA m (t :: k) (t' :: k) :: k

  pure  :: a -> m (EmptyA m) a

  infixl 4 <*>
  (<*>) :: m t (a -> b) -> m t' a -> m (AppendA m t t') b

class EffA m => EffM (m :: k -> Type -> Type) where
  type EmptyM m :: k
  type AppendM m (t :: k) (t' :: k) :: k

  return :: a -> m (EmptyM m) a

  (>>=) :: m t a -> (a -> m t' b) -> m (AppendM m t t') b

  (>>) :: m t a -> m t' b -> m (AppendM m t t') b
  x >> y = x >>= const y

----

data ManipulationKind = Read | Write

data SessionMode = Pipeline | Command | Transaction

data HasqlGrade = HasqlGrade ManipulationKind SessionMode

class HasConnections m where
  readConn :: m Pool
  writeConn :: m Pool

newtype HasqlEff (k :: HasqlGrade) result = HasqlEff { getFree :: Free (Statement ()) result } 
  deriving newtype (Functor, A.Applicative, M.Monad)


instance EffA HasqlEff where
  type EmptyA HasqlEff = 'HasqlGrade 'Read 'Pipeline

  type AppendA HasqlEff ('HasqlGrade 'Read s)  ('HasqlGrade 'Read s)  = 'HasqlGrade 'Read s
  type AppendA HasqlEff ('HasqlGrade 'Write s) ('HasqlGrade 'Read s)  = 'HasqlGrade 'Write s
  type AppendA HasqlEff ('HasqlGrade 'Read s)  ('HasqlGrade 'Write s) = 'HasqlGrade 'Write s

  pure :: a -> HasqlEff (EmptyA HasqlEff) a
  pure = HasqlEff . Pure

  (<*>) :: HasqlEff (k :: HasqlGrade) (a -> b) -> HasqlEff (k' :: HasqlGrade) a -> HasqlEff (AppendA HasqlEff k k') b
  HasqlEff f <*> HasqlEff a = HasqlEff $ f A.<*> a

type family AppendMHasqlEff (a :: HasqlGrade) (b :: HasqlGrade) :: k where
  AppendMHasqlEff ('HasqlGrade 'Read 'Transaction) ('HasqlGrade 'Read _) = 'HasqlGrade 'Read 'Transaction
  AppendMHasqlEff ('HasqlGrade 'Read _) ('HasqlGrade 'Read 'Transaction) = 'HasqlGrade 'Read 'Transaction
  AppendMHasqlEff ('HasqlGrade 'Read _) ('HasqlGrade 'Read _) = 'HasqlGrade 'Read 'Command
  AppendMHasqlEff ('HasqlGrade 'Write _) ('HasqlGrade _ _) = 'HasqlGrade 'Write 'Transaction
  AppendMHasqlEff ('HasqlGrade _ _) ('HasqlGrade 'Write _) = 'HasqlGrade 'Write 'Transaction

instance EffM HasqlEff where
  type EmptyM HasqlEff = 'HasqlGrade 'Read 'Command
  type AppendM HasqlEff g1 g2 = AppendMHasqlEff g1 g2

  return :: a -> HasqlEff (EmptyM HasqlEff) a
  return = HasqlEff . Pure

  (>>=) :: HasqlEff t a -> (a -> HasqlEff t' b) -> HasqlEff (AppendM HasqlEff t t') b
  (HasqlEff a) >>= f = HasqlEff $ a M.>>= (getFree . f)

read
  :: forall (a :: SessionMode) result.
     Statement () result
  -> HasqlEff ('HasqlGrade 'Read 'Pipeline) result
read s = HasqlEff . Free $ Pure <$> s

write
  :: forall (a :: SessionMode) result.
     Statement () result
  -> HasqlEff ('HasqlGrade 'Write 'Pipeline) result
write s = HasqlEff . Free $ Pure <$> s

class GetManipulationKind (k :: HasqlGrade) where
  manipulationKind :: Proxy k -> ManipulationKind

class GetSessionMode (k :: HasqlGrade) where
  sessionMode :: Proxy k -> SessionMode

instance GetSessionMode ('HasqlGrade m 'Command) where
  sessionMode _ = Command

instance GetSessionMode ('HasqlGrade m 'Transaction) where
  sessionMode _ = Transaction

instance GetSessionMode ('HasqlGrade m 'Pipeline) where
  sessionMode _ = Pipeline

instance GetManipulationKind ('HasqlGrade 'Read s) where
  manipulationKind _ = Read

instance GetManipulationKind ('HasqlGrade 'Write s) where
  manipulationKind _ = Write

run
  :: forall k m r. ( GetManipulationKind k, GetSessionMode k)
  => ( HasConnections m, MonadIO m)
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
              Pipeline -> pipeline . P.statement ()
  liftIO . use c $ foldFree natTrans e


test :: HasqlEff ('HasqlGrade 'Read 'Pipeline) (Int, Bool, String)
test = (,,) <$> r1 <*> r2 <*> r3
  where
    r1 :: HasqlEff ('HasqlGrade 'Read 'Pipeline) Int
    r1 = read undefined

    r2 :: HasqlEff ('HasqlGrade 'Read 'Pipeline) Bool
    r2 = read undefined

    r3 :: HasqlEff ('HasqlGrade 'Read 'Pipeline) String
    r3 = read undefined

test2a :: HasqlEff ('HasqlGrade 'Write 'Pipeline) (Maybe String)
test2a = f <$> test <*> write undefined
  where
    f :: (Int, Bool, String) -> Maybe String -> Maybe String
    f = undefined


test2b :: HasqlEff ('HasqlGrade 'Read 'Command) Word8
test2b = test >>= \(i, b, s) ->
  read (undefined s)

test3 :: HasqlEff ('HasqlGrade 'Write 'Transaction) (Maybe Int)
test3 = 
  test2b >>= \w -> write (undefined w)
