-- | Yes, this is basically Either.
module Data.Result (Result(Error,Ok)) where

data Result e a = 
    Error e
  | Ok a
