{-# LANGUAGE GADTs #-}

module ArithmeticExp where


-- Normal implementation.

data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Eq Expr Expr

eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just $ Left i
eval (B b) = Just $ Right b
eval (Add expr1 expr2) =
  case (evaledExpr1, evaledExpr2) of
    (Just (Left i1), Just (Left i2)) -> Just $ Left (i1 + i2)
    _ -> Nothing
  where
    evaledExpr1 = eval expr1; evaledExpr2 = eval expr2
eval (Mul expr1 expr2) =
  case (evaledExpr1, evaledExpr2) of
    (Just (Left i1), Just (Left i2)) -> Just $ Left (i1 * i2)
    _ -> Nothing
  where
    evaledExpr1 = eval expr1; evaledExpr2 = eval expr2
eval (Eq expr1 expr2) =
  case (evaledExpr1, evaledExpr2) of
    (Just (Left i1), Just (Left i2)) -> Just $ Right (i1 == i2)
    _ -> Nothing
    where
      evaledExpr1 = eval expr1; evaledExpr2 = eval expr2


-- Phatom types (incomplete).

data Expr a = I Int
             | B Bool
             | Add (Expr a) (Expr a)
             | Mul (Expr a) (Expr a)
             | Eq (Expr a) (Expr a)

i :: Int -> Expr Int
i = I
b :: Bool -> Expr Bool
b = B
add :: Expr Int -> Expr Int -> Expr Int
add = Add


-- GADTs.

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2

{-
To summarise, GADTs allows us to restrict the return types of constructors
and thus enable us to take advantage of Haskell's type system for our
domain specific languages. Thus, we can implement more languages and
their implementation becomes simpler.
-}
