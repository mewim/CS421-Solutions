module Solution where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import qualified Data.HashMap.Strict as H


-- Datatypes
-- ---------

-- Exp data type (hold parse results before evaluation)
data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         deriving (Show)

-- Val data type (for results of evaluation)
data Val = IntVal Integer
         | SymVal String
         | ExnVal String
         | PrimVal ([Val] -> Val)
         | Closure [String] Exp Env
         | DefVal String Val
         | ConsVal Val Val
         | Macro [String] Exp Env


-- Parsers
-- -------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

identFirst :: Parser Char
identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "-*+/:'?><=!"

identRest :: Parser Char
identRest = identFirst <|> adigit

identifier :: Parser String
identifier = do f <- identFirst
                r <- many identRest
                return $ f:r

whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

-- Grammaticals

anInt :: Parser Exp
anInt =  do d <- digits
            return $ IntExp (read d)

aSym :: Parser Exp
aSym =   do symbol <- identifier
            return $ SymExp symbol

aForm :: Parser Exp
aForm =  do oneOf "("
            whitespace
            exps <- many anExp
            whitespace
            oneOf ")"
            return $ SExp exps

aQuote :: Parser Exp
aQuote = do oneOf "'"
            whitespace
            exp <- anExp
            return $ SExp [SymExp "quote", exp]

aQQuote :: Parser Exp
aQQuote =   do oneOf "`"
               whitespace
               exp <- anExp
               return $ SExp [SymExp "quasiquote", exp]

anUnquote :: Parser Exp
anUnquote = do oneOf ","
               whitespace
               exp <- anExp
               return $ SExp [SymExp "unquote", exp]

-- By doing `whitespace` after the `exp <- ...` here, we cover whitespace after
-- any run of `aQuote <|> aQQuote <|> anUnquote <|> anInt <|> aSym <|> aForm`.
-- Because of that, we can take the `whitespace` off the end of each of those
-- individual parsers. It may be good to leave in the `whitespace` at the end of
-- each individual parser for maintainability (in case this one gets taken out).
anExp :: Parser Exp
anExp =  do whitespace
            exp <- aQuote <|> aQQuote <|> anUnquote <|> anInt <|> aSym <|> aForm
            whitespace
            return exp


-- Lifters/Lowerers
-- ----------------

-- Lift/Lower `Bool` to/from `Val`
liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

-- Lift functions of type `[Bool] -> Bool` to `[Val] -> Val`
liftBoolOp :: ([Bool] -> Bool) -> [Val] -> Val
liftBoolOp f = liftbool . f . map lowerbool

-- Original solution: This was unintuitive and did not provide enough
-- flexibility. Above we've added in the ability to control the base-case. We've
-- changed `liftIntOp` and `liftIntBoolOp` as well.
-- liftBoolOp :: (Bool -> Bool -> Bool) -> Val
-- liftBoolOp f = PrimVal (\vs -> liftbool $ case vs of
--                                             []  -> True
--                                             _   -> let vs' = map lowerbool vs
--                                                    in  and $ zipWith f vs' (tail vs')
--                        )

-- Lift/Lower `Integer` to/from `Val`
liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

-- Lift functions of type `Integer -> Integer -> Integer` to `[Val] -> Val`
liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> [Val] -> Val
liftIntOp _ z []    = liftint z
liftIntOp f z vs    = liftint . foldl1 f . map lowerint $ vs

-- Original solution:
-- liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> Val
-- liftIntOp f z vs = PrimVal (\vs -> liftint $ case vs of
--                                             []  -> z
--                                             _   -> let vs' = map lowerint vs
--                                                    in  foldl1 f vs'
--                            )

-- Lift functions of type `Integer -> Integer -> Bool` to type `[Val] -> Val`
-- Note that here we don't really need a `Bool` base-case because it's always
-- `liftbool True`
liftIntBoolOp :: (Integer -> Integer -> Bool) -> [Val] -> Val
liftIntBoolOp _ [] = liftbool True
liftIntBoolOp f vs = liftbool $ let is = map lowerint vs
                                in  and $ zipWith f is (tail is)

-- Original solution:
-- liftIntBoolOp :: (Integer -> Integer -> Bool) -> Val
-- liftIntBoolOp f = PrimVal (\vs -> liftbool $ case vs of
--                                             []  -> True
--                                             _   -> let vs' = map lowerint vs
--                                                    in  and $ zipWith f vs' (tail vs')
--                           )

-- Lift/Lower a Haskell list to/from a `Val`
liftlist :: [Val] -> Val
liftlist = foldr ConsVal (liftbool False)

lowerlist :: Val -> [Val]
lowerlist (ConsVal car cdr) = car : lowerlist cdr
lowerlist (SymVal "nil")    = []


-- Primitives/Initial Environment
-- ------------------------------

-- Primitive `eq?`
primEq :: [Val] -> Val
primEq []   = liftbool True
primEq vs   = liftbool . and $ zipWith eqVal vs (tail vs)
    where
        eqVal (IntVal i1)         (IntVal i2)           = i1 == i2
        eqVal (SymVal s1)         (SymVal s2)           = s1 == s2
        eqVal (ConsVal car1 cdr1) (ConsVal car2 cdr2)   = eqVal car1 car2 && eqVal cdr1 cdr2
        eqVal _                   _                     = False

-- Primitive unary operators
primUnary :: String -> (Val -> Val) -> [Val] -> Val
primUnary _ f [v]       = f v
primUnary opName _ _    = ExnVal $ "`" ++ opName ++ "` is a unary operator."

-- Primitive `not`
primNot :: Val -> Val
primNot x = liftbool . not . lowerbool $ x

-- Primitives `list`, `car`, and `cdr`
primList :: [Val] -> Val
primList = liftlist

primCar :: Val -> Val
primCar (ConsVal car cdr)   = car
primCar val                 = ExnVal $ "Not a cons cell: " ++ show val

primCdr :: Val -> Val
primCdr (ConsVal car cdr)   = cdr
primCdr val                 = ExnVal $ "Not a cons cell: " ++ show val

-- Pretty name for the Env type
type Env = H.HashMap String Val

-- The `runtime` is the initial environment for evaluation. Primitive operators
-- such as "+", "eq?", and "cdr" must be inserted into this runtime.
runtime :: Env
runtime = H.fromList    [ ("+",     PrimVal $ liftIntOp (+) 0)
                        , ("-",     PrimVal $ liftIntOp (-) 0)
                        , ("*",     PrimVal $ liftIntOp (*) 1)
                        , (">",     PrimVal $ liftIntBoolOp (>))
                        , ("<",     PrimVal $ liftIntBoolOp (<))
                        , (">=",    PrimVal $ liftIntBoolOp (>=))
                        , ("<=",    PrimVal $ liftIntBoolOp (<=))
                        , ("=",     PrimVal $ liftIntBoolOp (==))
                        , ("!=",    PrimVal $ liftIntBoolOp (/=))
                        , ("and",   PrimVal $ liftBoolOp and)
                        , ("or",    PrimVal $ liftBoolOp or)
                        , ("not",   PrimVal $ primUnary "not" primNot)
                        , ("eq?",   PrimVal primEq)
                        , ("list",  PrimVal primList)
                        , ("car",   PrimVal $ primUnary "car" primCar)
                        , ("cdr",   PrimVal $ primUnary "cdr" primCdr)
                        ]


-- Evaluation
-- ----------

-- Checks and returns parameter strings
paramStrs :: [Exp] -> Either String [String]
paramStrs = traverse paramStr
    where
        paramStr (SymExp p) = Right p
        paramStr _          = Left "Must use only `SymExp` for parameter names."

-- Quoting, unquoting, and quasiquoting
-- Notice that `quote` and `quasiquote` are just "special-case eval" functions -
-- they all have the same basic type of `Exp -> ... -> Val`.
quote :: Exp -> Val
quote (IntExp i)    = IntVal i
quote (SymExp s)    = SymVal s
quote (SExp exps)   = liftlist $ map quote exps

unquote :: Val -> Exp
unquote (IntVal i)      = IntExp i
unquote (SymVal s)      = SymExp s
unquote l@(ConsVal _ _) = SExp (map unquote $ lowerlist l)

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote (SExp [SymExp "unquote", exp]) env 1
    = eval exp env
quasiquote (SExp [SymExp "unquote", exp]) env d
    = liftlist $ [SymVal "unquote", quasiquote exp env (d-1)]
quasiquote (SExp [SymExp "quasiquote", exp]) env d
    = liftlist $ [SymVal "quasiquote", quasiquote exp env (d+1)]
quasiquote (SExp exps) env d
    = liftlist $ map (\exp -> quasiquote exp env d) exps
quasiquote e env _
    = quote e

-- This `eval` must handle every way an `Exp` could be constructed.
eval :: Exp -> Env -> Val
eval (IntExp i) env                                                     --- integers
    = IntVal i
eval (SymExp s) env                                                     --- symbols
    = case H.lookup s env of
        Just v  -> v
        Nothing -> ExnVal $ "Symbol " ++ s ++ " has no value."
eval (SExp []) env                                                      --- empty form
    = SymVal "nil"
eval (SExp [SymExp "define", SymExp f, SExp params, exp]) env           --- define form
    = case paramStrs params of
        Right ps    -> let closure = Closure ps exp env'
                           env'    = H.insert f closure env
                       in  DefVal f closure
        Left err    -> ExnVal err
eval (SExp [SymExp "def", SymExp f, exp]) env                           --- def form
    = DefVal f $ eval exp env
eval (SExp [SymExp "lambda", SExp params, exp]) env                     --- lambda form
    = case paramStrs params of
        Right ps    -> Closure ps exp env
        Left err    -> ExnVal err
eval (SExp [SymExp "quote", exp]) env                                   --- quote form
    = quote exp
eval (SExp [SymExp "cond", SExp body]) env                              --- cond form
    = case body of
        []                  -> liftbool False
        [_]                 -> liftbool False
        (cond : th : rest)  -> if (lowerbool $ eval cond env)
                                then eval th env
                                else eval (SExp [SymExp "cond", SExp rest]) env
eval (SExp [SymExp "let", SExp defs, body]) env                         --- let form
    = let defs' = map (\(SExp [SymExp var, val]) -> (var, eval val env)) defs
          env'  = H.union (H.fromList defs') env
      in  eval body env'
eval (SExp [SymExp "cons", car, cdr]) env                               --- cons form
    = ConsVal (eval car env) (eval cdr env)
eval (SExp [SymExp "eval", exp]) env                                    --- eval form
    = eval (unquote (eval exp env)) env
eval (SExp [SymExp "quasiquote", exp]) env                              --- quasiquote form
    = quasiquote exp env 1
eval (SExp [SymExp "unquote", exp]) env                                 --- unquote form
    = ExnVal "Cannot `unquote` more than `quasiquote`."
eval (SExp [SymExp "defmacro", SymExp f, SExp params, exp]) env         --- defmacro form
    = case paramStrs params of
        Right ps    -> let macro = Macro ps exp env'
                           env'  = H.insert f macro env
                       in  DefVal f macro
        Left err    -> ExnVal err
eval (SExp (f : as)) env                                                --- application form
    = let as' = map (\a -> eval a env) as
      in  case eval f env of
            PrimVal pf          -> pf as'                                   --- primitive operators
            Closure ps exp cenv -> let env' = H.union (H.fromList $ zip ps as') cenv
                                   in  eval exp env'                        --- defined functions
            Macro ps exp menv   -> let env' = H.union (H.fromList $ zip ps (map quote as)) menv
                                   in  eval (unquote (eval exp env')) env   --- defined macros
            val                 -> val                                      --- bare value


-- Printing
-- --------

-- This `show` must handle every way a `Val` could be constructed.
instance Show Val where
    -- show :: Val -> String
    show (IntVal i)         = show i
    show (SymVal s)         = s
    show (ExnVal s)         = "*** Scheme-Exception: " ++ s ++ " ***"
    show (PrimVal _)        = "*primitive*"
    show (Closure _ _ _)    = "*closure*"
    show (DefVal v _)       = v
    show l@(ConsVal _ _)    = "(" ++ showCons l ++ ")"
        where
            showCons (ConsVal car (SymVal "nil"))   = show car ++ " "
            showCons (ConsVal car l'@(ConsVal _ _)) = show car ++ " " ++ showCons l'
            showCons (ConsVal car cdr)              = show car ++ " . " ++ show cdr
    show (Macro _ _ _)      = "*macro*"


-- REPL
-- ----

repl :: Env -> IO ()
repl env =
    do  putStr "scheme> "
        l <- getLine                                                        -- READ
        case parse anExp "Expression" l of                                  -- READ
            Right exp -> case eval exp env of                               -- EVAL
                            (DefVal k v)    -> do   putStrLn k              -- PRINT
                                                    repl $ H.insert k v env -- LOOP
                            val             -> putStrLn $ show val          -- PRINT
            Left pe   -> putStrLn (show pe)                                 -- PRINT
        repl env                                                            -- LOOP

-- Another way
-- repl :: Env -> IO ()
-- repl env =
--     do  putStr "scheme> "
--         l <- getLine
--         case parse anExp "Expression" l of
--             Right exp   -> let val = eval exp env
--                             in do   putStrLn $ show val
--                                     case val of
--                                         DefVal k v  -> repl $ H.insert k v env
--                                         _           -> repl env
--             Left pe     -> putStrLn $ show pe
--         repl env

-- main function
main = repl runtime

