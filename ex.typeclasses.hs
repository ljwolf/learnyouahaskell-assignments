
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red 			= True
	Green == Green		= True
	Yellow == Yellow	= True
	_ == _				= False

instance Show TrafficLight where
	show Red 	= "Red light"
	show Yellow	= "Yellow light"
	show Green	= "Green light"

--class constraint inside of a class instance
--instance (Eq m) => Eq (Maybe m) where
--	Just x == Just y = x == y
--	Nothing == Nothing = True
--	_ == _ = False

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True
	
instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id

instance YesNo TrafficLight where
	yesno Red = False
	yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
