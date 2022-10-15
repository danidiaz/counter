module Data.Result where

data Result e a = 
    Problem e
  | Ok a
