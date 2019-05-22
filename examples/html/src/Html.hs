{-# LANGUAGE OverloadedStrings #-}

module Html (html, htmlRendered) where

import           Text.Blaze.Html                 (Html)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html4.Strict         as H


htmlRendered :: String
htmlRendered = renderHtml html


html :: Html
html = H.html $ do
  H.head $
    H.title "Stack Builders"
  H.body $
    H.div $
      H.p "Hello World!"
