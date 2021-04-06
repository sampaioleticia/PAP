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
    name <- many1 lowercase
    return (TypeVar name)
    
parseFun :: Parser Type      -- fun: atom "->" type
parseFun = do
   left <- parseAtom
   string "->"
   right <- parseType
   return (TypeArrow left right)
  
parseParen :: Parser Type    -- paren: "(" type ")"
parseParen =
  between (symbol "(") (symbol ")") parseType

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
unify (TypeInt) (TypeInt) = 
  Just []
--
unify (TypeArrow a b) (TypeArrow x y) =
  -- Queremos verificar se (a->b) ~ (x->y)
  -- PASSOS:
  --   Precisamos que a~x, retornando Just t1
  --   Precisamos que (t1)b~(t1)y, retornando Just t2
  --   Se temos t1 e t2, retornamos um unificador com sucesso,
  --     sendo a composição de t2 e t1
  --   Se t1 ou t2 não existirem, não podemos unificar!
-- Caso geral, não pode unificar
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
occursCheck x (TypeArrow a b) =
  -- OU x aparece em a, OU x aparece em b?
  occursCheck x a || occursCheck x b

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

--
-- (u)int = int
--
-- (u)a   = b    se { ..., a |-> b, ... } em u
--        = a    do contrário
--
-- (u)(e1 -> e2) = (u)e1 -> (u)e2
--

subst :: Unifier -> Type -> Type -- corrigir 
-- INTEIRO
subst u (TypeInt) =
  TypeInt

-- VARIAVEL 
-- substituir uma variável de tipo a se ela existir dentro da substituição
subst u (TypeVar a) =
  case lookup a u of
    -- { ..., a |-> b, ... } existe!
    Just b ->
      b
    Nothing ->
      -- A variável permanece a mesma!
      TypeVar a

-- ARROW
subst u (TypeArrow a b) =
  -- Distribui a chamada
  TypeArrow (subst u a) (subst u b)
