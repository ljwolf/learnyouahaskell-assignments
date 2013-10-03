import qualified Data.Map as M

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockerlookup :: Int -> LockerMap -> Either String Code
lockerlookup lockernumber map = 
	case	M.lookup lockernumber map of
			Nothing -> Left $ "Locker number " ++ show lockernumber ++ " doesn't exist!"
			Just (state, code) -> if	state /= Taken
									then	Right code
									else Left $ "Locker " ++ show lockernumber ++ " is already taken!"
									
lockers :: LockerMap
lockers = M.fromList
	[(100, (Taken, "ZA39I")),
	(101, (Free, "JAH3I")),
	(103, (Free, "IQSA9")),
	(105, (Free,"QOTSA")),
	(109, (Taken, "893JJ")),
	(110, (Taken, "99292"))
	]
	
