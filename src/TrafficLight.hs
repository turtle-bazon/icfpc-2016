module TrafficLight (countValid) where

data Direction
    = SouthToEast
    | SouthToNorth
    | SouthToWest
    | EastToNorth
    | EastToWest
    | EastToSouth
    | NorthToWest
    | NorthToSouth
    | NorthToEast
    | WestToNorth
    | WestToEast
    | WestToSouth


allowedPair :: Direction -> Direction -> Bool
allowedPair SouthToEast NorthToEast = False
allowedPair SouthToEast WestToEast = False
allowedPair SouthToEast _ = True
allowedPair SouthToNorth SouthToEast = True
allowedPair SouthToNorth SouthToWest = True
allowedPair SouthToNorth NorthToSouth = True
allowedPair SouthToNorth NorthToWest = True
allowedPair SouthToNorth WestToSouth = True
allowedPair SouthToNorth _ = False
allowedPair _ _ = False


countValid :: String
countValid = "dummy"
