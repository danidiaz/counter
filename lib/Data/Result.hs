module Data.Result where

data Result e a = 
    Error e
  | Ok a
