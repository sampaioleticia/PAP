import Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]

parseType :: Parser Type     -- type: function | atom 
parseType = try parseArrow 
         <|> parseAtom
         
parseAtom :: Parser Type     -- atom: int | var | paren
parseAtom = parseInt
         <|> parseVar 
         <|> parseParen
         
parseInt :: Parser Type      -- int: "Int" -- OK
parseInt = do
  string "Int"
  return TypeInt

parseVar :: Parser Type      -- var: lowercase+
parseVar = do
     name <- many1 lowercase
     return (TypeVar name)
  
parseArrow :: Parser Type        --igual
parseArrow = do
    t1 <- parseAtom
    string "->"
    t2 <- parseType
    return (TypeArrow t1 t2)

parseParen :: Parser Type    -- paren: "(" type ")"
parseParen = between (symbol "(") (symbol ")") parseType
---------------------------------------------------------------------------
  main :: IO () 
main = do
  putStrLn "Digite um termo:"
  a <- getLine
  let Right terma = parse parseType "<stdin>" a
  putStrLn "Digite outro termo:"
  b <- getLine
  let Right termb = parse parseType "<stdin>" b
  putStrLn "Unificação:"
  print $ unify terma termb
---------------------------------------------------------------------------
unify :: Type -> Type -> Maybe Unifier
--(REFL)
unify (TypeVar x) (TypeVar y) | x == y =
  Just []
--(RIGHT) 
unify y (TypeVar x) =
  if occursCheck x y then
    Nothing
  else
    Just [(x, y)]
--(LEFT)
unify (TypeVar x) y =
  if occursCheck x y then
    Nothing
  else
    Just [(x, y)]
--(INT)
unify (TypeInt)(TypeInt) = 
  Just []
--(ARROW)
unify (TypeArrow x y)(TypeArrow a b) =      
    case unify a x of
        Just t1 ->
            case unify (t1 b) (t1 y) of
                Just t2 ->
                    Just (compose t1 t2)
        Nothing ->
            Nothing
--(NADA)
unify _ _ =
  Nothing
---------------------------------------------------------------------------
occursCheck :: Name -> Type -> Bool
--INTEIRO
occursCheck x (TypeInt) =
  False 
--VARIAVEL
occursCheck x (TypeVar y) =
  x == y 
--ARROW
occursCheck x (TypeArrow a b) =
  occursCheck x a || occursCheck x b
---------------------------------------------------------------------------
compose :: Unifier -> Unifier -> Unifier 
compose xs ys =
  xs ++ applyOnSubst xs ys
applyOnSubst :: Unifier -> Unifier -> Unifier
applyOnSubst xs ys =
  let substOnTuple (name, term) =
        (name, subst xs term)
  in fmap substOnTuple ys
---------------------------------------------------------------------------
subst :: Unifier -> Type -> Type 
-- INTEIRO
subst u (TypeInt) =
  TypeInt
-- VARIAVEL 
subst u (TypeVar a) =
  case lookup a u of
    Just b ->
      b
    Nothing ->
      TypeVar a
-- ARROW
subst u (TypeArrow a b) =
  TypeArrow (subst u a) (subst u b)
