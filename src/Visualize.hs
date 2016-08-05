module Visualize where

import Text.JSON
import Common

instance JSON Point where
  readJSON = undefined
  showJSON Point {px = px, py = py} = JSArray [JSRational True px, JSRational True py]

instance JSON SilhouettePoly where
  readJSON = undefined
  showJSON (PolyFill points) = showJSON points
  showJSON (PolyHole points) = JSArray []
  -- JSObject (JSArray [ (toJSString "type", toJSString "fill"), (toJSString "points", showJSON points) ])
  -- showJSON (PolyHole points) = toJSObject (JSObject [ ("type", "hole"), ("points", points) ]

instance JSON Problem where
  readJSON = undefined
  showJSON Problem {silhouette = silhouette} = showJSON silhouette
