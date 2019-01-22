# hspec-golden
[![Build Status](https://travis-ci.org/stackbuilders/hspec-golden.svg?branch=master)](https://travis-ci.org/stackbuilders/hspec-golden)

## Description
> Golden tests with hspec.

`hspec-golden` allows you to write golden tests using `hspec`.

## Getting started

You can write golden tests using the `defaultGolden` helper:

```haskell
describe "myFunc" $
  it "generates the right output with the right params" $
     let output = show $ myFunc params
       in defaultGolden "myFunc" output
```

The first parameter of `defaultGolden` is the function name. I recommend you to use
`show` and `'functionName` (enable it with `TemplateHaskellQuotes`) to always have a unique name.
Example: `show 'myFunc == MyModule.myFunc`.

In case your output isn't a `String` you can define your own `Golden` test
using the `Golden` data type:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import           Test.Hspec
import           Test.Hspec.Golden
import           Data.Text (Text)
import qualified Data.Text.IO as T

myGoldenTest :: String -> Text -> Golden Text
myGoldenTest name output_ =
  Golden {
    output = output_
    writeToFile = T.writeFile
    readFromFile = T.readFile
    testName = name
    directory = ".myGoldenTestDir"
  }

describe "myTextFunc" $
  it "generates the right output with the right params" $
     let textOutput = myTextFunc params
       in myGoldenTest (show 'myTextFunc) textOutput
```

## Installing CLI

You can install the `hspec-golden` command line interface (CLI) using `stack`:

```
$ stack install hspec-golden
```

or `cabal`:


```
$ cabal install hspec-golden
```

The CLI is called `hgold`:

```
$ hgold

Parameters:
  DIR    The testing directory where you're dumping your results (default: .golden/)

Flags:
  -u[DIR]  --update[=DIR]  Replaces `golden` files with `actual` files
  -v       --version       Displays the version of hgold
  -h       --help          Displays help information
```

## Licensing

MIT, see the [LICENSE file](./LICENSE).

## Contributing
Pull requests for modifications to this program are welcome! Fork and open a PR.

If you're looking for a place to start, you may want to check the [open issue](https://github.com/stackbuilders/hspec-golden/issues).
