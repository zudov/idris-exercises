module Main

-- 2. Write a `palindrome` function.
palindrome : String -> Bool
palindrome x = x == reverse x

-- 3. Modify your function so it's not case-sensitive.
palindrome' : String -> Bool
palindrome' x = toLower x == reverse (toLower x)

-- 4. Only return `True` for strings longer than 10.
palindrome'' : String -> Bool
palindrome'' x =
  x == reverse x
    &&
  length x > 10

-- 5. Only return `True` for strings longer than `n`.
palindrome''' : Nat -> String -> Bool
palindrome''' n s =
  s == reverse s
    &&
  length s > n

-- 6. Take a string and return (number of words, number of characters)
counts : String -> (Nat, Nat)
counts a =
  ( length $ words a
  , length a
  )

-- 7. Return ten largest values in a list.
top_ten : Ord a => List a -> List a
top_ten = take 10 . Prelude.List.reverse . sort

-- 8. Count strings in `xs` that are longer than given `n`.
over_length : Nat -> List String -> Nat
over_length n = length . filter ((> n) . length)

-- 9. Write a main function that REPLs `palindrome`.
main : IO ()
main = repl "palindrome > " eval
  where
    eval = (++ "\n") . show . palindrome
