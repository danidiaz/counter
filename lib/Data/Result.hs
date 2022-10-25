-- | Yes, this is basically Either.
module Data.Result (Result(Err,Ok)) where

data Result e a = 
    Err e
  | Ok a
