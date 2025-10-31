-- Problems from https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/

module Main where

import qualified Easy
import qualified Medium

main :: IO ()
main = sequence_ [
    Easy.one,
    Easy.two,
    Easy.three,
    Easy.five,
    Easy.eight,
    Easy.nine,
    Easy.ten,
    Easy.eleven,
    Easy.twelve,
    Easy.fourteen,
    Easy.fifteen,
    Easy.sixteen,
    Easy.seventeen,
    Easy.eighteen,
    Easy.twentyOne,
    Easy.twentyTwo,
    Medium.one,
    Medium.eight
  ]
