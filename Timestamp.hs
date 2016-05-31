module Timestamp
	( isValidTimestamp
	, timestampToSec
	, timeDiff
	) where


type Timestamp = [Int]

-- Checks if a timestamp contains valid values
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp [] = False
isValidTimestamp [h, m, s]
	| h < 0 || h > 23 = False
	| m < 0 || m > 59 = False
	| s < 0 || s > 59 = False
	| otherwise = True

isValidTimestamp [m, s]
	| m < 0 || m > 59 = False
	| s < 0 || s > 59 = False
	| otherwise = True

isValidTimestamp [s]
	| s < 0 || s > 59 = False
	| otherwise = True

isValidTimestamp _ = False

-- Converts timestamp to seconds
timestampToSec :: Timestamp -> Int
timestampToSec ls 
	| not $ isValidTimestamp ls = error "Not valid timestamp"
	| otherwise = case ls of
					[h, m, s] -> s + m * 60 + h *3600
					[m, s]    -> s + m * 60
					[s]		  -> s

-- calculates time difference between two timestamps
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff ts1 ts2 = abs $ timestampToSec ts1 - timestampToSec ts2