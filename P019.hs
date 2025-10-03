data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Ord, Enum)

nextDayOfWeek Sun = Mon
nextDayOfWeek day = succ day

data Day = Day
  { dYear :: Int
  , dMonth :: Int
  , dDay :: Int
  , dDayOfWeek :: DayOfWeek
  }
  deriving (Show, Eq)

isLeapYear y =
  y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isEndOfMonth day
  | dDay day == 31 && any (== dMonth day) [1, 3, 5, 7, 8, 10, 12] = True
  | dDay day == 30 && any (== dMonth day) [4, 6, 9, 11] = True
  | dDay day == 28 && dMonth day == 2 && not (isLeapYear (dYear day)) = True
  | dDay day == 29 && dMonth day == 2 && isLeapYear (dYear day) = True
  | otherwise = False

isEndOfYear day = dDay day == 31 && dMonth day == 12

nextDay day
  | isEndOfYear day = Day
    { dYear = dYear day + 1
    , dMonth = 1
    , dDay = 1
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }
  | isEndOfMonth day = day
    { dDay = 1
    , dMonth = dMonth day + 1
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }
  | otherwise = day
    { dDay = dDay day + 1
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }

allDays = iterate nextDay (Day 1900 1 1 Mon)

xxCenturyDays = takeWhile ((<= 2000) . dYear) $ dropWhile ((< 1901) . dYear) allDays

main = print $ length $ filter (\d -> dDayOfWeek d == Sun && dDay d == 1) xxCenturyDays
