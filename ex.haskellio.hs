
--main = putStrLn "hello, world"

--main = do
--	putStrLn "Hello, what's your name?"
--	name <- getLine
--	putStrLn ("Hey " ++ name ++ ", you rock!")
	
--import Data.Char

--main = do
--	putStrLn "What's your first name?"
--	firstName <- getLine
--	putStrLn "What's your last name?"
--	lastName <- getLine
--	let	bigFirstName = map toUpper firstName
--		bigLastName = map toUpper lastName
--	putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

--main = do
--	line <- getLine
--	if null line
--		then return ()
--		else do
--			putStrLn $ reverseWords line
--			main
--			
--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

--main = do
--	a <- getLine
--	b <- getLine
--	c <- getLine
--	print [a,b,c]

--import Control.Monad
--import Data.Char
--
--main = forever $ do
--	putStr "give me some input: "
--	l <- getLine
--	putStrLn $ map toUpper l

import Control.Monad

main = do
	colors <- forM [1,2,3,4] (\a -> do
		putStrLn $ "Which color do you associate with the number" ++ show a ++ "?"
		color <- getLine
		return color)
	putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
	mapM putStrLn colors

