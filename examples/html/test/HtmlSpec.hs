module HtmlSpec (spec) where

import qualified Data.Text.Lazy     as T
import           Html               (htmlRendered)
import           Test.Hspec
import           Test.Hspec.Golden
import           Text.Pretty.Simple (pString)


spec :: Spec
spec =
  describe "html" $ do
    context "given a valid generated html" $
      it "generates html" $
        defaultGolden "html" htmlRendered

    context "given a valid generated html" $
      it "generates html (error encoded pretty)" $
        (defaultGolden "html_pretty" htmlRendered)
           { encodePretty = T.unpack . pString }
