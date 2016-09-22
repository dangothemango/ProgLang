import PA1Helper
import Data.List

letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
-- data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) =  v
id' lexp@(Lambda (Atom _) _) = lexp
id' lexp@(Apply _ _) = lexp

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' _ u []    = u
foldr' f u (h:t) = f h (foldr' f u t)

filter' :: (a-> Bool) -> [a] -> [a]
filter' p = foldr' 
            (\h t ->  if p h 
                      then h:t 
                      else t) []

remove :: (Eq a) => a -> [a] -> [a]
remove x = filter' (\v -> v/=x)

freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda (Atom v) e) = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)

boundvars :: Lexp -> [String]
boundvars (Atom s) = []
boundvars (Lambda (Atom v) e) = v:(boundvars e)
boundvars (Apply a b) = (boundvars a)++(boundvars b)

reducer :: Lexp -> Lexp
reducer v@(Atom _) = v
reducer lexp@(Lambda (Atom v) e) = ret
                                where
                                    ret = (etaDriver (Lambda (Atom v) (reducer e)))
reducer lexp@(Apply a b) = betaDriver (alphaDriver(Apply (reducer a) (reducer b)))

etaDriver :: Lexp -> Lexp
--First two cannot be eta reduced. Ignore
etaDriver v@(Atom _) = v
etaDriver lexp@(Apply _ _) = lexp
--Can be eta reduced. Lets find out
etaDriver lexp@(Lambda (Atom v) e) = if caneta lexp
                                        then
                                            (doeta e)
                                        else
                                            lexp
                                        where

doeta :: Lexp -> Lexp
doeta lexp@(Apply a b) = a

caneta :: Lexp -> Bool
caneta lexp@(Lambda (Atom v) e) = case e of 
                                    (Apply a b) -> case b of 
                                                    (Atom c) -> if c==v
                                                                    then
                                                                     not (c `elem` freevars a)
                                                                    else
                                                                     False
                                                    otherwise -> False
                                    otherwise -> False

betaDriver :: Lexp -> Lexp
betaDriver lexp@(Apply a b) = case a of
                                (Atom v) -> lexp
                                (Lambda (Atom v) e) -> (beta (reducer e) v b)
                                (Apply c d) -> (Apply (betaDriver (Apply (reducer c) (reducer d))) b)
                            where x= lexPrinter lexp

beta :: Lexp -> String -> Lexp -> Lexp
beta lexp@(Atom v) s rlexp = if s==v
                                    then rlexp
                                    else lexp
beta lexp@(Lambda (Atom v) e) s rlexp = (Lambda (Atom v) (beta e s rlexp))
beta lexp@(Apply a b) s rlexp=(Apply (beta a s rlexp) (beta b s rlexp))
                                         


alphaDriver :: Lexp -> Lexp
alphaDriver lexp@(Apply a b) = (Apply (alphaIterate a (freevars b)) b)
                                    where x= lexPrinter lexp

alphaIterate :: Lexp -> [String] -> Lexp
alphaIterate lexp [] = lexp
alphaIterate lexp (h:t) = alphaIterate lexp2 t
                            where lexp2=alpha lexp "" lexp h

alpha :: Lexp -> String -> Lexp -> String -> Lexp
alpha lexp@(Atom v) r oLexp s 
  | not (v==s) = lexp
  | v==s = if r=="" 
           then (Atom r2)
           else (Atom r)
           where r2=(pickR oLexp)


alpha lexp@(Lambda (Atom v) e) r oLexp s
  | not (v==s) = (Lambda (Atom v) (alpha e r oLexp s))
  | v==s = if r=="" 
           then (Lambda (Atom r2) (alpha e (r2) oLexp s))
           else(Lambda (Atom r) (alpha e r oLexp s))
           where r2=(pickR oLexp)
                                                
alpha lexp@(Apply a b) r oLexp s= (Apply (alpha a r oLexp s) (alpha a r oLexp s))

pickR :: Lexp -> String
pickR lexp = head ((letters \\ boundvars lexp) \\freevars lexp)

lexPrinter :: Lexp -> IO() 
lexPrinter = do
                putStrLn ((show lexp))


-- Entry point of program
main = do
        putStrLn "Please enter a filename containing lambda expressions:"
        fileName <- getLine
        -- id' simply returns its input, so runProgram will result
        -- in printing each lambda expression twice. 
        runProgram fileName reducer
