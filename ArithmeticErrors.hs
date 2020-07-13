data Error = NegativeNumber
           | DivisionByZero
           | Remainder
             deriving (Show, Eq)

data Err a = Catch Error | Valid a
             deriving (Show, Eq)

check :: Int -> Err Int
check x = if x < 0
            then Catch NegativeNumber
            else Valid x

apply :: (a -> b -> c) -> Err a -> Err b -> Err c
apply o (Valid x) (Valid y) = Valid (o x y)
apply o (Valid x) (Catch error) = Catch error
apply o (Catch error) (Valid y) = Catch error

data Exp = Val Int
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp

eval :: Exp -> Err Int

eval (Val x) = check x

eval (Add (Val x) (Val y)) = if ((x+y)<0) 
                               then Catch NegativeNumber
                               else apply (+) (Valid x) (Valid y)

eval (Sub (Val x) (Val y)) = if ((x-y)<0) 
                               then Catch NegativeNumber
                               else apply (-) (Valid x) (Valid y)

eval (Div (Val x) (Val y)) = if (x == 0) || (y == 0) 
                               then Catch DivisionByZero
                               else if ((x `mod` y) /= 0)
                                      then Catch Remainder
                                      else apply (div) (Valid x) (Valid y)
