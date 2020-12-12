type Point2D = (Int, Int)

data Direction = North | East | South | West
  deriving (Eq, Show)

newtype Ship = Ship Point2D

newtype Waypoint = Waypoint Point2D

data Move = MNorth Int | MEast Int | MSouth Int | MWest Int | MLeft Int | MRight Int | MForward Int
  deriving (Eq, Show)

rotate :: Int -> Waypoint -> Waypoint
rotate 0 (Waypoint (x, y)) = Waypoint (x, y)
rotate 90 (Waypoint (x, y)) = Waypoint (y, - x)
rotate 180 (Waypoint (x, y)) = Waypoint (- x, - y)
rotate 270 (Waypoint (x, y)) = Waypoint (- y, x)

move :: (Ship, Waypoint) -> Move -> (Ship, Waypoint)
move (s, Waypoint (wx, wy)) (MNorth n) = (s, Waypoint (wx, wy + n))
move (s, Waypoint (wx, wy)) (MSouth n) = (s, Waypoint (wx, wy - n))
move (s, Waypoint (wx, wy)) (MEast n) = (s, Waypoint (wx + n, wy))
move (s, Waypoint (wx, wy)) (MWest n) = (s, Waypoint (wx - n, wy))
move (s, w) (MLeft n) = (s, rotate (360 - n) w)
move (s, w) (MRight n) = (s, rotate n w)
move (Ship (x, y), Waypoint (wx, wy)) (MForward n) = (Ship (x + n * wx, y + n * wy), Waypoint (wx, wy))

manhattan :: Ship -> Int
manhattan (Ship (x, y)) = abs x + abs y

parseLine :: String -> Move
parseLine (x : xs) =
  ( case x of
      'N' -> MNorth
      'E' -> MEast
      'S' -> MSouth
      'W' -> MWest
      'L' -> MLeft
      'R' -> MRight
      'F' -> MForward
  )
    $ read xs

main = interact $ show . manhattan . fst . foldl move (Ship (0, 0), Waypoint (10, 1)) . map parseLine . lines