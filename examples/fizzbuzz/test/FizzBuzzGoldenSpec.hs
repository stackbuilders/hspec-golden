module FizzBuzzGoldenSpec where

import           Test.Hspec
import           Test.Hspec.Golden
import           FizzBuzz

spec :: Spec
spec = 
    describe "fizzBuzz" $
      golden "Turns 3 multiples to fizz and 5 multiples to buzz" $
        return (show $ fizzBuzz [1,2,3,4,5,11,12,13,14,15])
