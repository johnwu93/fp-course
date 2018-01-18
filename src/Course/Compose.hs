{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.
newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  f_a_b <$> (Compose app1_app2_value) = Compose ((f_a_b <$>) <$> app1_app2_value)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose app1_app2_fun) <*> (Compose app1_app2_value) = Compose ((<*>) <$$> app1_app2_fun <*> app1_app2_value)

-- Implement the pure function for an Applicative instance for Compose
-- Implement the (<*>) function for an Applicative instance for Compose
-- Implement the (=<<) function for a Monad instance for Compose
instance (Monad f, Monad g) => Monad (Compose f g) where
  (=<<) = error "Impossible"
