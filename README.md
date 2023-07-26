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

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://cristhianmotoche.github.io/"><img src="https://avatars.githubusercontent.com/u/8370088?v=4?s=100" width="100px;" alt="Cristhian Motoche"/><br /><sub><b>Cristhian Motoche</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=CristhianMotoche" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://www.christian-wuensche.com/"><img src="https://avatars.githubusercontent.com/u/3807237?v=4?s=100" width="100px;" alt="Christian Wünsche (ehem Heyn)"/><br /><sub><b>Christian Wünsche (ehem Heyn)</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=christianwish" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/bitc"><img src="https://avatars.githubusercontent.com/u/996487?v=4?s=100" width="100px;" alt="Bit Connor"/><br /><sub><b>Bit Connor</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=bitc" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://dbalseiro.github.io/"><img src="https://avatars.githubusercontent.com/u/2053849?v=4?s=100" width="100px;" alt="Diego Balseiro"/><br /><sub><b>Diego Balseiro</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=dbalseiro" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Jagl257"><img src="https://avatars.githubusercontent.com/u/27145248?v=4?s=100" width="100px;" alt="Jorge Guerra Landázuri"/><br /><sub><b>Jorge Guerra Landázuri</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=Jagl257" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ChickenProp"><img src="https://avatars.githubusercontent.com/u/57271?v=4?s=100" width="100px;" alt="Phil Hazelden"/><br /><sub><b>Phil Hazelden</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=ChickenProp" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="http://alexvieth.wordpress.com/"><img src="https://avatars.githubusercontent.com/u/1420272?v=4?s=100" width="100px;" alt="Alexander Vieth"/><br /><sub><b>Alexander Vieth</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=avieth" title="Code">💻</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/guibou"><img src="https://avatars.githubusercontent.com/u/9705357?v=4?s=100" width="100px;" alt="Guillaume Bouchard"/><br /><sub><b>Guillaume Bouchard</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=guibou" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/fefi95"><img src="https://avatars.githubusercontent.com/u/12057338?v=4?s=100" width="100px;" alt="Stefani Castellanos"/><br /><sub><b>Stefani Castellanos</b></sub></a><br /><a href="https://github.com/stackbuilders/hspec-golden/commits?author=fefi95" title="Code">💻</a></td>
    </tr>
  </tbody>
  <tfoot>
    <tr>
      <td align="center" size="13px" colspan="7">
        <img src="https://raw.githubusercontent.com/all-contributors/all-contributors-cli/1b8533af435da9854653492b1327a23a4dbd0a10/assets/logo-small.svg">
          <a href="https://all-contributors.js.org/docs/en/bot/usage">Add your contributions</a>
        </img>
      </td>
    </tr>
  </tfoot>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!