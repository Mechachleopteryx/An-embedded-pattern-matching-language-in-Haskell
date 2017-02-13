-- Coroutine module inspired by the great article:
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coroutines-for-streaming



{-# LANGUAGE InstanceSigs, LambdaCase, RankNTypes #-}

module Coroutine where

import Control.Monad (ap, (>=>))
import Control.Monad.Morph (MFunctor(..))  -- from the mmorph package
import Control.Monad.Trans (MonadTrans(..))



newtype Producing o i m r
    = Producing { resume :: m (ProducerState o i m r) }

data ProducerState o i m r  -- intended to serve as a suspension functor
    = Done r
    | Produced o (Consuming r m i o)

type Consuming r m i o = i -> Producing o i m r

instance Functor m => Functor (ProducerState o i m) where
    -- "Functor m =>" is needed here, because we rely on the nested "Producing o i m" 
    -- being a functor, which requires that as a prerequisite.
    fmap :: (a -> b) -> ProducerState o i m a -> ProducerState o i m b
    fmap f (Done a) = Done (f a)
    fmap f (Produced o c) = Produced o $ fmap f . c
        -- fmap here requires Producing o i m to be a functor.

instance Functor m => Functor (Producing o i m) where
    fmap :: (a -> b) -> Producing o i m a -> Producing o i m b  -- needs InstanceSigs
    fmap f = Producing . fmap (fmap f) . resume
        -- the first fmap is from the functor m and the second one, from the functor 
        -- "ProducerState o i m".

instance Monad m => Applicative (Producing o i m) where
    -- "Monad m =>" here is needed as we rely on the functions from the monad instance of 
    -- "Producing o i m".
    pure  = return
    (<*>) = ap

instance Monad m => Monad (Producing o i m) where
    return :: r -> Producing o i m r
    return = Producing . return . Done  -- return comes from the monad m.

    (>>=) :: Producing o i m a -> (a -> Producing o i m b) -> Producing o i m b
    m >>= k = Producing $ resume m >>= \case  -- needs LambdaCase
        -- (>>=) here is from the monad m.
        -- case :: ProducerState o i m a -> m (ProducerState o i m b)
        Done a -> resume (k a)
        Produced o c -> return $ Produced o $ (>>= k) . c
            -- (>>= k) recursively calls our >>= under definition, and the return refers 
            -- to that from the monad m.

    -- (>>=) is not for combining a Producing and a Consuming together to run both of 
    -- them while moving back and forth between them, which is exactly what ($$) does. 
    -- (>>=) is for combining two Producings together to run them in sequence, passing 
    -- the result from the first Producing to the second one. For example, (>>=) is used 
    -- to compile "do s <- lift getLine; lift (putStrLn s)", which is a Producing 
    -- coroutine but does not yield anything as not having yields inside it.

instance MonadTrans (Producing o i) where
    lift :: Functor m => m r -> Producing o i m r
    lift = Producing . fmap Done

yield :: Monad m => o -> Producing o i m i  -- embeds a Consuming coroutine
yield o = Producing $ return $ Produced o $ return
    -- The last return above is from the monad "Producing o i m".
    --or = Producing $ return $ Produced o $ lift . return

infixl 2 $$  -- higher than (>>=)
($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = resume producing >>= \case
    -- case :: ProducerState a b m r -> m r
    Done r -> return r  -- We're done if the Producer is done.
    Produced a c -> consuming a $$ c
        -- consuming :: a -> Producing b a m r
        -- c :: Consuming r m b a

    -- We can combine only those coroutines that are in opposite states, and have 
    -- opposite interfaces (i.e., if one has the a/b interface and the other has to have 
    -- the b/a interface), and the same result type (because ($$) don't know which one 
    -- will make the final result) and have the (almost) same underlying monad. We 
    -- consider any transformed (thus extended) monad is almost the same as its base 
    -- monad, because values of the base monad can be easily turned into ones of the 
    -- tranformed monad using lift, though not vice versa. In this respect, we can say 
    -- the forcibly lifted monad is a transformed monad but does not use the added 
    -- operations at all. So we can combine two coroutines that do not have exactly the 
    -- same inner monads but almost the same ones, because one can be made match the 
    -- other by simply extending it with lift.

-- MFunctor is a functor in the category of monads, using hoist as the analog of fmap.
instance MFunctor (Producing o i) where
    hoist :: forall o i m n b.  -- n is a scoped type variable.
             Functor m => (forall a. m a -> n a) -> Producing o i m b -> Producing o i n b
             -- needs RankNTypes
    -- lifts a monad morphism from m to n into a monad morphism from (t m) to (t n). Note 
    -- that lift is a monad morphism. Whereas a monad morphism can turn a monadic value 
    -- into another monadic value, hoist along with some monad morphism from m to n can 
    -- turn a monadic value of "t m a" into a "t n a", changing the "inner" monad from m 
    -- to n.

    -- Since lift from a monad transformer s is also a monad morphism, we can think of 
    -- "hoist lift" as converting the inner monad m of "t m a" to another monad n. But in 
    -- this case, n is a transformed monad "s m" rather than a simple monad, so "hoist 
    -- lift" maps a "t m a" into a "t (s m) a".

    hoist f = go where
        go = Producing . f . fmap map' . resume where  -- We are using m as a functor.
        -- Note, fmap works on the "elements" of m "horizontally" and does not go any 
        -- further into them. But map' does that and works "vertically".

        map' (Done b) = Done b
        map' (Produced o c) = Produced o $ go . c

-- This comes in handy for defining Consumers. This is similar to forever from 
-- Control.Monad, but works on monadic functions rather than monadic values.
foreverK :: Monad m => (a -> m a) -> a -> m r
foreverK k = k >=> foreverK k
