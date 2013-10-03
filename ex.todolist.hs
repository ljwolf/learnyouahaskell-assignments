import System.IO
import System.Directory
import Data.List


--main = do
--	todoItem <- getLine
--	appendFile "todo.txt" (todoItem ++ "\n")


--using buffering in blockbuffering
--main = do
--	withFile "something.txt" ReadMode (\handle -> do
--		hsetBuffering handle $ BlockBuffering (Just 2048)
--		contents <- hGetContents handle
--		putStr contents)

main = do
	handle <- openFile "todo.txt" ReadMode
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let	todoTasks = lines contents
		numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
	putStrLn "These are your To-Do items:"
	putStr $ unlines numberedTasks
	putStrLn "Which one do you want to delete?"
	numberString <- getLine
	let	number = read numberString
		newTodoItems = delete (todoTasks !! number) todoTasks
	hPutStr tempHandle $ unlines newTodoItems
	hClose handle
	hClose tempHandle
	removeFile "todo.txt"
	renameFile tempName "todo.txt"
