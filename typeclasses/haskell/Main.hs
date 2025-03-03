import TypingHaskellInHaskell

main :: IO ()
main = do
    a <- return (Tyvar "a" Star)
    b <- return (Tyvar "b" Star)
    x <- return (Tyvar "x" (Kfun Star Star))
    y <- return (Tyvar "y" (Kfun Star Star))
    s <- (match (TAp tList (TVar a)) (TAp tList (TVar b)))
    putStrLn ("s = " ++ (show s))
