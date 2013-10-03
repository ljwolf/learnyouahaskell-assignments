--respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
--	where	isPalindrome xs = xs == reverse xs

respondPalindromes2 = unlines . map (\xs -> if isPalindrome2 xs then "palindrome" else "not a fucking palindrome, brohan!") . lines
	where isPalindrome2 xs = xs == reverse xs

--main = interact respondPalindromes

main = interact respondPalindromes2
