module Data.Result (Result(Error,Ok)) where

data Result e a = 
    Error e
  | Ok a
