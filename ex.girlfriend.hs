import System.IO
import Data.Char

--main = do
--	handle <- openFile "girlfriend.txt" ReadMode
--	contents <- hGetContents handle
--	putStr contents
--	hClose handle


--withfile definition, aka withFile in *Main
--withfile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--withfile path mode f = do
--	handle <- openFile path mode
--	result <- f handle
--	hClose handle
--	return result

--main = do
--	withfile "girlfriend.txt" ReadMode (\handle -> do
--		contents <- hGetContents handle
--		putStr contents)

main = do
	contents <- readFile "girlfriend.txt"
	writeFile "girlfriendcaps.txt" (map toUpper contents)
