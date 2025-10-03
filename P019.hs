data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Ord, Enum)

nextDayOfWeek Sun = Mon
nextDayOfWeek day = succ day

data Month
  = Jan | Feb | Mar
  | Apr | May | Jun
  | Jul | Aug | Sep
  | Oct | Nov | Dec
  deriving (Show, Eq, Ord, Enum)

data Day = Day
  { dYear :: Int
  , dMonth :: Month
  , dDay :: Int
  , dDayOfWeek :: DayOfWeek
  }
  deriving (Show, Eq)

isLeapYear y =
  y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isEndOfMonth day
  | dDay day == 31 && any (== dMonth day) [Jan, Mar, May, Jul, Aug, Oct, Dec] = True
  | dDay day == 30 && any (== dMonth day) [Apr, Jun, Sep, Nov] = True
  | dDay day == 28 && dMonth day == Feb && not (isLeapYear (dYear day)) = True
  | dDay day == 29 && dMonth day == Feb && isLeapYear (dYear day) = True
  | otherwise = False

isEndOfYear day = dDay day == 31 && dMonth day == Dec

nextDay day
  | isEndOfYear day = Day
    { dYear = dYear day + 1
    , dMonth = Jan
    , dDay = 1
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }
  | isEndOfMonth day = day
    { dDay = 1
    , dMonth = succ (dMonth day)
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }
  | otherwise = day
    { dDay = dDay day + 1
    , dDayOfWeek = nextDayOfWeek (dDayOfWeek day)
    }

allDays = iterate nextDay (Day 1900 Jan 1 Mon)

xxCenturyDays = takeWhile ((<= 2000) . dYear) $ dropWhile ((< 1901) . dYear) allDays

main = print $ length $ filter (\d -> dDayOfWeek d == Sun && dDay d == 1) xxCenturyDays
