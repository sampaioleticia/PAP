-- Trabalho Final da diciplina de PAP -- Trabalho feito por Leticia Capitani e Lucas Gesser
-- implementar algoritimo de unificação
import Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]
-- solicitar 2 tipos e informar se pode ser unificado 
-- fazer Parser em cada termo
parseType :: Parser Type     -- type: function | atom
parseType = do
parseType = try parseTypeFun 
         <|> parseTypeAtom

parseAtom :: Parser Type     -- atom: int | var | paren
parseAtom = int 
         <|> var 
         <|> paren

parseInt :: Parser Type      -- int: "Int" 
parseInt = do
  string "Int"
  return TypeInt
  
parseVar :: Parser Type      -- var: lowercase+ -- OK
parseVar = do
    name <- parseName
    remain <- many1 lowercase+
    TypeVar <- parseType
    return (Var (name TypeVar))
    
parseFun :: Parser Type      -- fun: atom "->" type
   type <- atom
   return TypeArrow
  
parseParen :: Parser Type    -- paren: "(" type ")"
parseParen = between (symbol "(") (symbol ")") parseType
----------------------------------------------------------------------------------------------------------------------------------------
-- estrutura principal
main :: IO () 
main = do
  putStrLn "Digite um termo:"
  a <- getLine
-- verificar se parsing deu certo
  let Right term_a = parse term "<stdin>" a
  putStrLn "Digite outro termo:"
-- verificar se parsing deu certo
  b <- getLine
  let Right term_b = parse term "<stdin>" b

  putStrLn "Unificação:"
  print $ unify term_a term_b
----------------------------------------------------------------------------------------------------------------------------------------
-- função de unificar -- recebe dois termos e retorna um unificador mais geral forma Just mgu ou Nothing caso não de pra unificar 
unify :: Type -> Type -> Maybe Unifier
-- duas variaveis iguais podem ser unificadas (REFL)
unify (TypeVar x) (TypeVar y) | x == y =
  Just []
-- uma variavel a esquerda pode ser unificada se não aparecer livre na direita (RIGHT) 
unify b (TypeVar a) =
  if occursCheck a b then
    Nothing
  else
    Just [(a, b)]
-- uma variavel a esquerda pode ser unificada se não aparecer livre na esquerda (LEFT)
unify (TypeVar a) b =
  if occursCheck a b then
    Nothing
  else
    Just [(a, b)]
-- pode unificar o tipo int com o tipo int retornando uma lista vazia (INT)
 unify (TypeInt n1) (TypeInt n2) | n1 == n2 = 
   Just []
-- pode unificar as funções compondo o resultado (ARROW) - 8 linhas
unify (TypeVar x) (TypeArrow a b) =
  if occursCheck x (TypeArrow a b) == True then 
  Nothing 
  else 
  Just [(x,a)]
-- não pode unificar 
unify _ _ =
  Nothing
----------------------------------------------------------------------------------------------------------------------------------------
--função para verificar se uma variavel aparece livre em um tipo e que compõe duas unificações distintas
occursCheck :: Name -> Type -> Bool
--occursCheck fazer com cada um 
--INTEIRO
occursCheck x (TypeInt y) =
  False 
--VARIAVEL
--verifica se a variável X existe na variável y
occursCheck x (TypeVar y) =
  x == y -- Apenas se forem iguais 
--ARROW
occursCheck x (TypeArrow y xs) = 
  if occursCheck x y then True 
  else 
  occursCheck x xs
----------------------------------------------------------------------------------------------------------------------------------------
compose :: Unifier -> Unifier -> Unifier -- corrigir
compose xs ys =
-- função mapear
  xs ++ applyOnSubst xs ys
-- aplicar subst xs em ys
applyOnSubst :: Unifier -> Unifier -> Unifier

applyOnSubst xs ys =
  let substOnTuple (name, term) =
        (name, subst xs term)

  in fmap substOnTuple ys
----------------------------------------------------------------------------------------------------------------------------------------
-- aplica uma substituição em um termo
-- função para testar o sistema (aplicar uma substituição a um tipo arbitrário retornando um novo tipo)
-- ?
subst :: Unifier -> Type -> Type -- corrigir 
-- INTEIRO
subst n1 == n2 = 
  Just []
-- VARIAVEL 
-- substituir uma variável de tipo a se ela existir dentro da substituição
subst xs (TypeVar a) =
  case lookup a xs of
    Just b -> b
    Nothing -> Var a
-- ARROW
subst (TypeVar x) (TypeArrow a xs) =
case lookup TypeArrow a x == True of
Just a -> a
Nothing -> Arrow x
