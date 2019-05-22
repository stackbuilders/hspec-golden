module HtmlSpec (spec) where

import           Html              (htmlRendered)
import           Test.Hspec
import           Test.Hspec.Golden


spec :: Spec
spec =
  describe "html" $
    context "given a valid generated html" $
      it "generates html" $
        defaultGolden "html" htmlRendered
