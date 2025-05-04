
```haskell
next :: Parser Char 
next = Parser (\s -> case s of 
        [] -> Nothing
        (c : s') -> Just (c,s'))
```

