# hspec-golden
[![Build Status](https://travis-ci.org/stackbuilders/hspec-golden.svg?branch=master)](https://travis-ci.org/stackbuilders/hspec-golden)

## Description
Golden tests with hspec. Example:

```haskell
describe "myFunc" $
  it "generates the right output" $
     let output = show $ myFunc params
       in defaultGolden "myFunc" output
```
