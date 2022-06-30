# hspec-golden
[![Build Status](https://travis-ci.org/stackbuilders/hspec-golden.svg?branch=master)](https://travis-ci.org/stackbuilders/hspec-golden)

## Description
Golden tests store the expected output in a separated file. Each time a golden test
is executed the output of the subject under test (SUT) is compared with the
expected output. If the output of the SUT changes then the test will fail until
the expected output is updated.

`hspec-golden` allows you to write golden tests using the popular `hspec`.

## Usage

You can write golden tests using `defaultGolden` helper:

```haskell
describe "myFunc" $
  it "generates the right output with the right params" $
     let output = show $ myFunc params
       in defaultGolden "myFunc" output
```

The first parameter of `defaultGolden` is the golden file name. I recommend you to use
`show` and `'functionName` (enable `TemplateHaskellQuotes` for the quote) to
always have a unique name for your file. Example: `show 'myFunc == MyModule.myFunc`.
Although, you can name it as you like.

In case your output isn't a `String` you can define your own `Golden` test
using the `Golden` data type:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import           Test.Hspec
import           Test.Hspec.Golden
import           Data.Text (Text)
import qualified Data.Text.IO as T

myGoldenTest :: String -> Text -> Golden Text
myGoldenTest name actualOutput =
  Golden {
    output = actualOutput,
    encodePretty = prettyText,
    writeToFile = T.writeFile,
    readFromFile = T.readFile,
    goldenFile = name,
    actualFile = Just (name </> "-actual"),
    failFirstTime = False
  }

describe "myTextFunc" $
  it "generates the right output with the right params" $
     let textOutput = myTextFunc params
       in myGoldenTest (show 'myTextFunc) textOutput
```

## Install CLI

You can install the `hspec-golden` command line interface (CLI) with `stack`:

```
$ stack install hspec-golden
```

or `cabal`:


```
$ cabal install hspec-golden
```

The CLI is called `hgold`:

```
$ hgold -h

Update your golden files

Usage: hgold [-u|--update [DIR]] [-v|--version]

Available options:
  -u,--update [DIR]        The testing directory where you're dumping your
                           results. (default: ".golden")
  -v,--version             Show version
  -h,--help                Show this help text
```

Update the golden tests under `.golden` directory:

```
$ hgold
```

Update the golden tests under `.myGoldenTest` directory:

```
$ hgold -u .myGoldenTest
```

## License

MIT, see the [LICENSE file](./LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://www.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
