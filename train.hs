module Train where

import Data.Functor
import Data.Maybe
import Control.Applicative
import Control.Monad

-- data structure definition
data Train a = Engine | Car a (Train a) deriving (Eq, Show)

-- basic operations
push :: Train a -> a -> Train a
push Engine a    = Car a Engine
push (Car x t) a = Car x (push t a)

attach :: Train a -> a -> Train a
attach train a = Car a train

shift :: Train a -> Maybe a
shift Engine    = Nothing
shift (Car a _) = Just a

pop :: Train a -> Maybe a
pop Engine         = Nothing
pop (Car a Engine) = Just a
pop (Car _ tail)   = pop tail

tail :: Train a -> Train a
tail Engine    = Engine
tail (Car _ t) = t

-- the way trains normally merge on a platform - they
-- are "joined" by attaching a "reversed" train to another one
pmerge :: Train a -> Train a -> Train a
pmerge Engine train    = train
pmerge (Car a t) train = pmerge t (attach train a)

append :: Train a -> Train a -> Train a
append train Engine       = train
append Engine train       = train
append train (Car a tail) = append (push train a) tail

rev :: Train a -> Train a
rev Engine = Engine
rev (Car a t) = push (rev t) a

len :: Train a -> Int
len train = lrec train 0
  where lrec Engine n       = n
        lrec (Car _ tail) n = lrec tail (n + 1)
        
-- TODO add semigroup instance     

-- functor typeclass impl
instance Functor Train where
  fmap f Engine    = Engine
  fmap f (Car a t) = (Car (f a) (fmap f t))

-- applicative typeclass impl
instance Applicative Train where
  pure a                = Car a Engine
  (<*>) (Car f t) train = append (t <*> train) (fmap f train)
  (<*>) _ Engine        = Engine
  (<*>) Engine _        = Engine

instance Monad Train where
  (>>=) Engine f    = Engine
  (>>=) (Car a t) f = append (f a) ((>>=) t f)
