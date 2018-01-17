{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary
-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a = State
  { runState :: s -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec :: State s a -> s -> s
exec (State func) s = snd (func s)

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval (State func) s = fst (func s)

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put x = State (P.const ((), x))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  f_a <$> (State f_s) =
    State
      (\state ->
         let (producedValue, resultingState) = f_s state
         in (f_a producedValue, resultingState))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure value = State (\s -> (value, s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f_s_fab) <*> (State f_s_a) =
    State
      (\state ->
         let (f_a_b, resultingState) = f_s_fab state
             (producedValue, newResultingState) = f_s_a resultingState
         in (f_a_b producedValue, newResultingState))

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  f_a_state =<< (State f_s_sa) =
    State
      (\state ->
         let (producedValue, resultingState) = f_s_sa state
             (State f_s_bs) = f_a_state producedValue
         in f_s_bs resultingState)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const88 $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil = return Empty
findM predicate (x :. xs) = do
  boolValue <- predicate x
  if boolValue
    then return (Full x)
    else findM predicate xs

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat :: Ord a => List a -> Optional a
firstRepeat list = eval (findM isRepeated list) S.empty
  where
    isRepeated :: Ord a => a -> State (S.Set a) Bool
    isRepeated value =
      State
        (\discoveredValues ->
           if S.member value discoveredValues
             then (True, discoveredValues)
             else (False, S.insert value discoveredValues))

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: Ord a => List a -> List a
distinct list = eval (filtering (State . computeDistinct) list) S.empty

computeDistinct :: Ord a => a -> S.Set a -> (Bool, S.Set a)
computeDistinct value discoveredValues = if S.member value discoveredValues
                                                  then (False, discoveredValues)
                                                  else (True, S.insert value discoveredValues)

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy :: Integer -> Bool
isHappy value = contains 1 (firstRepeat (produce f value))
                where f :: Integer -> Integer
                      f n =  P.fromIntegral $ P.sum (P.map ((P.^2) . digitToInt) (show n))
