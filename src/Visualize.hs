module Visualize where

import Text.JSON
import Common
import BoundingBox

instance JSON Point where
  readJSON = undefined
  showJSON Point {px = px, py = py} = JSArray [JSRational True px, JSRational True py]

instance JSON SilhouettePoly where
  readJSON = undefined
  showJSON (PolyFill points) = makeObj [("type", showJSON "fill"), ("points", showJSON points)]
  showJSON (PolyHole points) = makeObj [("type", showJSON "hole"), ("points", showJSON points)]

instance JSON Problem where
  readJSON = undefined
  showJSON Problem {silhouette = polygons, skeleton = skeleton} = makeObj [("polygons", showJSON polygons),
                                                                           ("skeleton", showJSON skeleton)]

makeProblemModel :: Problem -> JSValue
makeProblemModel problem@(Problem {silhouette = polygons, skeleton = skeleton})
  = makeObj [("polygons", showJSON polygons),
             ("skeleton", showJSON skeleton),
             ("bbox", showJSON $ bbox' polygons)]
