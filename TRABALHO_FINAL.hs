-- implementar algoritimo de unificação
import Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]
-- solicitar 2 tipos e informar se pode ser unificado 
-- fazer Parser Term em cada termo

parseType :: Parser Type     -- type: function | atom
parseAtom :: Parser Type     -- atom: int | var | paren
parseInt :: Parser Type      -- int: "Int"
parseVar :: Parser Type      -- var: lowercase+ -- OK
parseFun :: Parser Type      -- fun: atom "->" type
parseParen :: Parser Type    -- paren: "(" type ")"

--tem o de variavel falta o de int e arrow
variable :: Parser Term
variable = do
  start <- upper
  remain <- many lower
  return (Variable (start:remain))
----------------------------------------------------------------------------------------------------------------------------------------
--estrutura principal
main :: IO () -- FEITO
main = do
  putStrLn "Digite um termo:"
  a <- getLine
  -- Assume que o parsing deu certo!
  let Right term_a = parse term "<stdin>" a
  putStrLn "Digite outro termo:"

  -- Bug do repl.it! Lê uma linha extra...
  getLine -- Remova se compilar localmente...

  b <- getLine
  -- Assume que o parsing deu certo!
  let Right term_b = parse term "<stdin>" b
  --
  putStrLn "Unificação:"
  -- print term_a
  -- print term_b
  print $ unify term_a term_b
----------------------------------------------------------------------------------------------------------------------------------------
-- função de unificar -- recebe dois termos e retorna um unificador mais geral forma Just mgu ou Nothing caso não de pra unificar 
unify :: Type -> Type -> Maybe Unifier

--duas variaveis iguais podem ser unificadas (REFL)
unify (Variable x) (Variable y) | x == y =
  Just []
--uma variavel a esquerda pode ser unificada se não aparecer livre na direita (RIGHT) 
unify b (Variable a) =
  if occursCheck a b then
    Nothing
  else
    Just [(a, b)]
--uma variavel a esquerda pode ser unificada se não aparecer livre na esquerda (LEFT)
unify (Variable a) b =
  if occursCheck a b then
    Nothing
  else
    -- Troque a por b!
    Just [(a, b)]
--pode unificar o tipo int com o tipo int retornando uma lista vazia (INT)

--pode unificar as funções compondo o resultado (ARROW) - 8 linhas

--não pode unificar 
unify _ _ =
  Nothing
----------------------------------------------------------------------------------------------------------------------------------------
--função para verificar se uma variavel aparece livre em um tipo e que compõe duas unificações distintas
occursCheck :: Name -> Type -> Bool
--occursCheck fazer com cada um 
--INTEIRO
--VARIAVEL
--verifica se a variável X existe na variável y
occursCheck x (Variable y) =
  x == y -- Apenas se forem iguais!
--ARROW
----------------------------------------------------------------------------------------------------------------------------------------
compose :: Unifier -> Unifier -> Unifier -- FEITO
compose xs ys =
  -- Usa a função "mapear" da lista! :D
  xs ++ applyOnSubst xs ys

-- Applica uma substituição em outra (xs em ys)
applyOnSubst :: Unifier -> Unifier -> Unifier

-- Exemplo:
--   { X |-> foo } * { Y |-> X } = { Y |-> foo }
applyOnSubst xs ys =
  -- Pra cada troca A |-> b, vire A |-> (xs)b...
  let substOnTuple (name, term) =
        (name, subst xs term)

  in fmap substOnTuple ys
----------------------------------------------------------------------------------------------------------------------------------------
--aplica uma substituição em um termo
--função para testar o sistema (aplicar uma substituição a um tipo arbitrário retornando um novo tipo)
subst :: Unifier -> Type -> Type
--INTEIRO
--VARIAVEL 
-- Devemos substituir uma variável A se ela existir dentro da substituição
subst xs (Variable a) =
  -- Usamos a função lookup da biblitoca padrão! Temos { ..., A |-> b, ...}?
  case lookup a xs of
    -- Se A |-> b existe em xs, retorna b...
    Just b ->
      b
    -- Se não, ficamos com A sem ser alterado!
    Nothing ->
      Variable a
--ARROW
