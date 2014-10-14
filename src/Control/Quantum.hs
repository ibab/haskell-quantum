{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Quantum
    ( Amplitude
    , WaveFunction
    , Quantum
    , quantize
    , MonadQuantum
    , runQuantum
    , runQuantum'
    , measure
    , measurements
    )
    where

import qualified Data.Map  as M
import           Data.Map (Map)
import           Data.Monoid
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Random as R
import           Data.Complex

nearZero :: Double -> Bool
nearZero x = abs x <= 1e-12

type Amplitude = Complex Double

-- | The Wave function is the distribution of amplitudes over the
-- possible states of the underlying data type
type WaveFunction a = [(a, Amplitude)]

class (Monad m) => MonadQuantum m where
    quantize  :: (Ord a) => WaveFunction a -> m a
    quantize' ::            WaveFunction a -> m a
    condition :: Bool -> m ()

liftQ  :: (Ord b, MonadQuantum m) => (a -> b) -> m a -> m b
liftQ f q1 = q1 >>= always . f

liftQ2 :: (Ord c, MonadQuantum m) => (a -> b -> c) -> m a -> m b -> m c
liftQ2 f p1 p2 = do x1 <- p1
                    x2 <- p2
                    always (f x1 x2)

liftQ3 :: (Ord d, MonadQuantum m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftQ3 f p1 p2 p3 = do x1 <- p1
                       x2 <- p2
                       x3 <- p3
                       always (f x1 x2 x3)

always a = quantize [(a, 1.0:+0.0)]
always' a = quantize' [(a, 1.0:+0.0)]

-- | Describes the quantized version of an arbitrary data type.
-- The underlying data type defines the basis that the state is measured in.
data Quantum a where
    Quantum :: Ord a => Map (Maybe a) Amplitude -> Quantum a
    QuantumAny :: [(Maybe a, Amplitude)]  -> Quantum a

noState  :: (Ord a) => Quantum a
noState  = Quantum (M.singleton Nothing (1.0:+0.0))

noState' :: Quantum a
noState' = QuantumAny [(Nothing, 1.0 :+ 0.0)]

deriving instance (Show a) => Show (Quantum a)

instance Functor Quantum where
    fmap = liftM

instance Applicative Quantum where
    pure  = return
    (<*>) = ap

instance Monad Quantum where
    return  = always'

    m >>= f = if unitary then next
                         else error "Non-unitary transformation applied to quantum state!"
        where
            unitary = nearZero $ l2norm (map snd $ toList' next) - 1.0
            next = collect [multAmpl q (go a) | (a, q) <- toList' m]
            go = maybe noState' f

multAmpl :: Amplitude -> Quantum a -> Quantum a
multAmpl q (Quantum x) = Quantum $ M.map (* conjugate q) x
multAmpl q (QuantumAny x) = QuantumAny [ (a, q * r) | (a, r) <- x ]

toList' :: Quantum a -> [(Maybe a, Amplitude)]
toList' (Quantum x) = M.toList x
toList' (QuantumAny x) = x

toList :: (Ord a) => Quantum a -> [(Maybe a, Amplitude)]
toList (Quantum x) = M.toList x
toList (QuantumAny x) = merge x

collect :: [Quantum a] -> Quantum a
collect [ ]        = QuantumAny []
collect [x]        = x
collect (Quantum x:t) = case collect t of
                        Quantum y -> Quantum (M.unionWith (+) x y)
                        QuantumAny y -> Quantum (M.unionWith (+) x (M.fromList y))
collect (QuantumAny x:t) = case collect t of
                        Quantum y -> Quantum (M.unionWith (+) (M.fromList x) y)
                        QuantumAny y -> QuantumAny (x ++ y)

merge :: (Ord a) => WaveFunction a -> WaveFunction a
merge = M.toList . M.fromListWith (+)

instance MonadQuantum Quantum where
    quantize    = Quantum . M.fromListWith (+) . normalize
    quantize'   = QuantumAny . normalize
    condition test = if test then always () else noState

instance (Ord a, Monoid a) => Monoid (Quantum a) where
    mempty  = always mempty
    mappend = liftQ2 mappend


normalize :: WaveFunction a -> [(Maybe a, Amplitude)]
normalize xs = map (\(a, q) -> (Just a, q / total)) xs
    where
        total = sum $ zipWith (*) (map conjugate ampl) ampl
        ampl  = map snd xs

-- | Remove all impossible states from the quantum state and renormalize
collapse :: [(Maybe a, Amplitude)] -> WaveFunction a
collapse xs = [ (x, q / (norm:+0)) | (Just x, q) <- xs ]
    where
        norm = l2norm [ q | (Just x, q) <- xs ]

l2norm :: [Amplitude] -> Double
l2norm xs = sqrt $ sum $ map (\x -> (magnitude x)**2) xs

-- | Extracts the wave function from a quantum state.
runQuantum :: (Ord a) => Quantum a -> WaveFunction a
runQuantum = collapse . toList

runQuantum' :: Quantum a -> WaveFunction a
runQuantum' = collapse . toList'

-- | Measures a quantum state. Returns a single realization of the underlying data type
-- with probability equal to the squared magnitude of the amplitude of that realization.
-- Note that if this Module would be backed by a real quantum processor, this would be
-- the only valid way to extract information from the Monad.
measure :: (Ord a) => Quantum a -> IO a
measure q = do
  x <- evalRandIO $ R.fromList $ map (\(a, b) -> (a, toRational b)) $ measurements q
  return x

-- | Converts a quantum state into a probability state through measurement.
-- Equivalent to performing repeated measurements on equally prepared quantum states
-- and noting the frequency of each possible realization.
measurements :: (Ord a) => Quantum a -> [(a, Double)]
measurements = f . runQuantum
    where f xs = [(x, (magnitude q)**2) | (x, q) <- xs]


