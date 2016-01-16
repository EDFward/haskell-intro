module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name val toFind = if toFind == name then val else state toFind

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var name) =  state name
evalE _ (Val val) = val
evalE state (Op expr1 op expr2) = case op of
  Plus -> val1 + val2
  Minus -> val1 - val2
  Times -> val1 * val2
  Divide -> val1 `div` val2
  Gt -> fromEnum $ val1 > val2
  Ge -> fromEnum $ val1 >= val2
  Lt -> fromEnum $ val1 < val2
  Le -> fromEnum $ val1 <= val2
  Eql -> fromEnum $ val1 == val2
  where
    val1 = evalE state expr1
    val2 = evalE state expr2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name expr) = DAssign name expr
desugar (Incr name) = DAssign name $ Op (Var name) Plus (Val 1)
desugar (If expr thenStatement elseStatement) =
  DIf expr (desugar thenStatement) (desugar elseStatement)
desugar (While expr statement) = DWhile expr (desugar statement)
-- For loop to while loop
desugar (For initStatement conditionExpr updateStatement execStatement) =
  DSequence (desugar initStatement)
    (DWhile conditionExpr
      (DSequence (desugar execStatement) (desugar updateStatement)))
desugar (Sequence statement1 statement2) =
  DSequence (desugar statement1) (desugar statement2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
-- Assign
evalSimple state (DAssign name expr) = extend state name val
  where val = evalE state expr
-- If
evalSimple state (DIf expr thenStatement elseStatement) =
  if evalE state expr /= 0
    then evalSimple state thenStatement else evalSimple state elseStatement
-- While
evalSimple state whileStatement@(DWhile expr statement) =
  if evalE state expr == 0
    then state else evalSimple state (DSequence statement whileStatement)
-- Sequence
evalSimple state (DSequence statement1 statement2) =
  evalSimple newState statement2 where newState = evalSimple state statement1
-- Skip
evalSimple state DSkip = state

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
