module Html.Attribute where

import Lucid.Base

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"

-------------------------------------------------------------------------------
-- HTMX

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"
