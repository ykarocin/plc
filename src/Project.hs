module InterpretacaoEAnaliseEstaticaDelinguagens where

-- Interpretadores recebem programas como entrada. Strings como
--
-- "def inc = (lambda x . + x 1); def v = + 3 2; def resultado = inc v"
--
-- são dadas como entrada, e a saída é o resultado da execução.
--
-- Para interpretar programas, precisamos representr os programas de forma
-- abstrata, como uma árvore resultante do processo de parsing. Para
-- representar os programas de uma linguagem funcional simples,
-- temos o seguinte (identificador, número, lambda exp, aplicação, definição
-- e programa).

type Id = String
type Numero = Double
data TermoLinFun = Identifier Id
                 | Literal Numero
                 | Lambda Id TermoLinFun
                 | Aplicacao TermoLinFun TermoLinFun
data Definicao = Def Id TermoLinFun
type Programa = [Definicao]


-- Aplicacao String TermoLinFun TermoLinFun
-- seria específico para aplicações binárias.
--
-- Melhor como acima do que como acima
-- type Definicao = (String,TermoLinFun)


-- Por exemplo, o programa abaixo
--
-- def inc = (lambda x . + x 1); def v = + 3 2; def resultado = inc v
--
-- seria representado como

def1 = Def "inc" (Lambda "x" (Aplicacao (Aplicacao (Identifier "+") (Identifier "x")) (Literal 1)))
def2 = Def "v" (Aplicacao (Aplicacao (Identifier "+") (Literal 3)) (Literal 2))
def3 = Def "resultado" (Aplicacao (Identifier "inc") (Identifier "v"))
prog1 = [def1,def2,def3]


-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções.

data ValorFun = Numero Double
              | Funcao (ValorFun -> ValorFun)
              | Excecao

instance Show ValorFun where
    show (Numero n) = show n
    show (Funcao f) = "Function definition cannot be printed!"
    show Excecao = "Excecao durante a execucao do interpretador"


-- A função que implementa o interpretador dos termos precisa receber como parâmetro um
-- ambiente, contendo as funções pré-definidas, e as definidas pelo programador.

type Ambiente = [(Id,ValorFun)]

-- No nosso caso, o ambiente teria apenas a definição de "+".

ambientesimples = [("+",Funcao (\x -> (Funcao (\y -> somaValorFun x y))))]
data Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro
somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao


-- Temos agora duas funções de interpretação, uma para termos e uma
-- para programas. A de termos simplesmente lê o ambiente. A de programa
-- propaga alterações no ambiente, para acumular as funções definidas.

intTermo a (Identifier i) = getValor i a
intTermo a (Literal l) = Numero l
intTermo a (Lambda i t) = Funcao (\v -> intTermo ((i,v):a) t)
intTermo a (Aplicacao t1 t2) = aplica v1 v2
                                where v1 = intTermo a t1
                                      v2 = intTermo a t2

intPrograma a [] = Excecao
intPrograma a [(Def i t)] = intTermo a t
intPrograma a ((Def i t):ds) = intPrograma ((i,v):a) ds
                               where v = intTermo a t

getValor i [] = Excecao
getValor i ((j,v):l) = if i == j then v else getValor i l

aplica (Funcao f) v = f v
aplica _ _ = Excecao


-- Exemplo de reescrita
--
-- intPrograma as [Def "x" (Aplicacao (Aplicacao (Identifier "+")
--                                               (Identifier "x"))
--                                    (Literal 1.0))] =
-- intTermo as (Aplicacao (Aplicacao (Identifier "+")
--                                   (Identifier "x"))
--                        (Literal 1.0)) =
-- aplica v1 v2
-- aplica (Funcao (\y -> somaValorFun Excecao y)) (Numero 1.0) =
-- (\y -> somaValorFun Excecao y) (Numero 1.0) =
-- somaValorFun Excecao (Numero 1.0) =
-- Excecao
--
-- v1 = intTermo as (Aplicacao (Identifier "+") (Identifier "x"))
--    = aplica v11 v12
--    = aplica (Funcao (\x -> (Funcao (\y -> somaValorFun x y))))) Excecao
--    = Funcao (\y -> somaValorFun Excecao y)
--
-- v11 = intTermo as Identifier "+" =
-- Funcao (\x -> (Funcao (\y -> somaValorFun x y))))
--
-- v12 = intTermo as Identifier "x" =
-- getValor "x" as =
-- Excecao
--data Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro
-- v2 = intTermo as (Literal 1.0)
-- = Numero 1.0

-- Nas linguagens com atribuição (o valor de uma variável pode mudar ao
-- longo da execução), precisamos lidar com a noção de estado. Além do
-- ambiente contendo definições imutáveis, precisamos de uma noção de estado, ou
-- memória, para armazenar os valores das variáveis em um determinado ponto da
-- execução. A função de interpretação não só recebe o estado como parâmetro.
-- Ela também retorna como resultado o valor da interpretação e o novo estado,
-- contendo as alterações nos valores das variáveis.

-- Para simplifcar, temos apenas termos na linguagem, incluindo atribuições
-- (que podem representar tanto definições no sentido do interpretador anterior
-- quanto mudanças nos valores de variáveis) e composição sequencial (como o ";"
-- em Java, que entre outras coisas faz o papel da lista de definições da linguagem
-- anterior). Representamos, por simplicidade, soma como um termo específico da
-- linguagem.

data Termo = Var Id
           | Lit Numero
           | Som Termo Termo
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Id Termo
           | Seq Termo Termo

-- A aplicação "(lambda x . + x 2) 3" seria
termo1 = (Apl (Lam "x" (Som (Var "x") (Lit 2))) (Lit 3))

-- A aplicação "(lambda x . + x y) 3" seria
termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit 3))

-- A composição sequencial "y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
termo3 = (Seq (Atr "y" termo2) termo2)

-- A composição sequencial "y := 3 ; (lambda x . + x y) 3" seria
sq1 = (Seq (Atr "y" (Lit 3)) termo2)

-- A composição sequencial "y := 3 ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
sq2 = (Seq (Atr "y" (Lit 3)) termo3)

-- A composição sequencial "y := (z := 5) + z ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit 5)) (Var "z"))) termo3)



-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções. Como as funções
-- podem acessar e modificar variáveis que mudam de valor ao longo
-- da execução, é necessário receber não só o argumento da função,
-- e retornar seu resultado. É preciso receber também o estado atual,
-- e retornar o novo estado modificado pela execução da função.

data Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro

type Estado = [(Id,Valor)]


-- int :: [(Id, Valor)] -> Termo -> [(Id, Valor)] -> (Valor, [(IVard, Valor)])
-- int :: Ambiente -> Termo -> Estado -> (Valor, Estado)
--

int a (Var x) e = (search x (a ++ e), e)

int a (Lit n) e = (Num n, e)

int a (Som t u) e = (somaVal v1 v2, e2)
                    where (v1,e1) = int a t e
                          (v2,e2) = idata Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erront a u e1

int a (Lam x t) e = (Fun (\v -> int ((x,v):a) t), e)

int a (Apl t u) e = app v1 v2 e2
                    where (v1,e1) = int a t e
                          (v2,e2) = int a u e1

int a (Atr x t) e = (v1, wr (x,v1) e1)
                    where (v1,e1) = int a t e

int a (Seq t u) e = int a u e1
                    where (_,e1) = int a t e


-- search :: Eq a => a -> [(a, Valor)] -> Valor

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

-- somaVal :: Valor -> Valor -> Valor

somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro data Valor = Num Double
           | Var Id
           | LitB Boolean
           | LitN Numero
           | LitS String
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro


-- app :: Valor -> Valor -> Estado -> (Valor, Estado)

app (Fun f) v e = f v e
app _ _ e = (Erro, e)

-- wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]

wr (i,v) [] = [(i,v)]VarVar

-------------------------------------------------------------------------
type Id = String
type Numero = Double

data Termo = Var Id
           | LitB Boolean
           | LitN Numero
           | LitS String
           --
           | Atr Termo Termo
           | Mul Termo Termo -- lucas g
           --
           | If Comparacao Termo Termo -- lucas g
           | While Comparacao Termo -- joao
           | For Atr Comparacao Termo Termo -- lucas o
           --Var
           | Fun Id [Id] Termo -- lucas o
           | Call Termo Id [Termo] -- ykaro
           | Lam Id Termo
           | Apl Termo Termo 
           | Seq Termo Termo
           --
           | Class Id [Fun] [Id] -- ykaro
           | New Id -- joao
           | InstanceOf Termo Id -- lucas g
           | This -- lucas o
           | FieldAccess Termo Id
           | Ref Termo

data Comparacao = Eq Termo Termo
               | Ne Termo Termo
               | Lt Termo Termo
               | Le Termo Termo
               | Gt Termo Termo
               | Ge Termo Termo Termo

-- Declaração de classes
type ClasseId = String
type CampoId = String
type MetodoId = String
type Classe = (ClasseId, [Metodo], [CampoId])

data Metodo = Metodo MetodoId [Id] Termo

data Valor = B Boolean
           | N Numero
           | S String
           | Class ([Metodo] [Id])
           | Fun (Valor -> Estado -> (Valor,Estado))
           | VVoid 
           | Erro

type Estado = [(String,Valor)]
type Ambiente = [(String,Termo)] -- Ambiente de variáveis e funções
type AmbienteClasse = [(ClasseId, Classe)] -- Ambiente de classes
type Heap = [(Integer, (String,Estado))]

-- int :: [(Integer, (String,Estado))] -> [(String, Termo)] -> [(ClasseId, Classe)] -> Termo -> [(String, Valor)] -> (Valor, [(String,Valor)], [(Integer, (String,Estado))], [(ClasseId, Classe)])
-- int :: Heap -> Ambiente -> AmbienteClasse -> Termo -> Estado -> (Valor, Estado, Heap, AmbienteClasse)
--

ambiente = [("*",Funcao (\x -> (Funcao (\y -> mulValor x y))))]
mulValor (Numero x) (Numero y) = Numero (x*y)
mulValor _ _ = Excecao

int h a ac (Var x) e = (search x (a ++ e), e, h, ac)

int h a ac (LitB b) e = (B b, e, h, ac)
int h a ac (LitS s) e = (S n, e, h, ac)
int h a ac (LitN n) e = (N n, e, h, ac)

int h a ac (Atr x t) e = (v1, wr (x,v1) e1, h1, ac1)
                    where (v1,e1,h1, ac1) = int a t e h ac

-- IMPLEMENTAÇÃO DE MULTIPLICAÇÃO - lgs4
int h a ac (Mul t1 t2) e = (mulValor v1 v2, e2, h2, ac2)
                    where (v1, e1, h1, ac1) = int h a ac t1 e
                          (v2, e2, h2, ac2) = int h a ac t2 e1

-- IMPLEMENTAÇÃO DE IF
int h a ac (If cond thenBranch elseBranch) e = 
    case vcond of
        B True  -> int h1 a ac1 thenBranch e1
        B False -> int h1 a ac1 elseBranch e1
        _       -> (Erro, e1, h1, ac1)
    where (vcond, e1, h1, ac1) = evalCond h a ac cond e

-- Função auxiliar para avaliar comparações
evalCond h a ac (Eq t1 t2) e = (B (v1 == v2), e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

evalCond h a ac (Ne t1 t2) e = (B (v1 /= v2), e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

evalCond h a ac (Lt t1 t2) e = 
    case (v1, v2) of
        (N n1, N n2) -> (B (n1 < n2), e2, h2, ac2)
        _ -> (Erro, e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

evalCond h a ac (Le t1 t2) e = 
    case (v1, v2) of
        (N n1, N n2) -> (B (n1 <= n2), e2, h2, ac2)
        _ -> (Erro, e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

evalCond h a ac (Gt t1 t2) e = 
    case (v1, v2) of
        (N n1, N n2) -> (B (n1 > n2), e2, h2, ac2)
        _ -> (Erro, e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

evalCond h a ac (Ge t1 t2) e = 
    case (v1, v2) of
        (N n1, N n2) -> (B (n1 >= n2), e2, h2, ac2)
        _ -> (Erro, e2, h2, ac2)
    where (v1, e1, h1, ac1) = int h a ac t1 e
          (v2, e2, h2, ac2) = int h a ac t2 e1

-- IMPLEMENTAÇÃO DE INSTANCEOF
int h a ac (InstanceOf t className) e = (resultado, e1, h1, ac1)
    where (v, e1, h1, ac1) = int h a ac t e
          resultado = case v of
              Ref addr -> case lookup addr h1 of
                  Just (objClass, _) -> B (objClass == className)
                  Nothing -> Erro
              _ -> B False
--lgs4

int h a ac (Lam x t) e = (Fun (\v -> int ((x,v):a) t), e, h, ac)

int h a ac (Apl t u) e = app v1 v2 e2 h2 ac2
                    where (v1,e1, h1, ac1) = int a t e h ac
                          (v2,e2, h2, ac2) = int a u e1 h1 ac1


int h a ac (Seq t u) e = int a u e1 h1 ac1 
                    where (_,e1, h1, ac1) = int a t e h ac

int h a ac (Ref t) e = resultado
  where (v, e1, h1, ac1) = int h a ac t e
        resultado
          | isRef v    = (v, e1, h1, ac1)
          | otherwise  = (Erro, e1, h1, ac1)

        isRef (Ref _) = True
        isRef _       = False

int h a ac (FieldAccess t campo) e = resultado
  where (v, e1, h1, ac1) = int h a ac t e
        resultado = case v of
          Ref addr -> case lookup addr h1 of
            Just (_, estadoObjeto) -> case lookup campo estadoObjeto of
              Just valorCampo -> (valorCampo, e1, h1, ac1)
              Nothing         -> (Erro, e1, h1, ac1)
            Nothing -> (Erro, e1, h1, ac1)
          _ -> (Erro, e, h, ac)

int h a ac (Class nome metodos campos) e = (VVoid, e, h, ac')
  where classe = (nome, metodos, campos)
        ac' = (nome, classe) : ac

-- IMPLEMENTAÇÃO DO FOR -lmlo
int h a ac (For inicializacao condicao atualizacao corpo) e = loopFor h_ini a ac_ini condicao atualizacao corpo e_ini
  where
    -- 1. Executa a inicialização uma única vez para obter o estado inicial do laço
    (_, e_ini, h_ini, ac_ini) = int h a ac inicializacao e

loopFor :: Heap -> Ambiente -> AmbienteClasse -> Comparacao -> Termo -> Termo -> Estado -> (Valor, Estado, Heap, AmbienteClasse)
loopFor h a ac condicao atualizacao corpo e =
  -- 2. Avalia a condição no estado atual
  let (v_cond, e_cond, h_cond, ac_cond) = evalCond h a ac condicao e
  in case v_cond of
      -- 3. Se a condição for VERDADEIRA
      B True ->
        -- 3a. Executa o corpo do laço
        let (_, e_corpo, h_corpo, ac_corpo) = int h_cond a ac_cond corpo e_cond
        -- 3b. Executa a expressão de atualização
        in let (_, e_atual, h_atual, ac_atual) = int h_corpo a ac_corpo atualizacao e_corpo
        -- 3c. Chama recursivamente o laço com o estado totalmente atualizado
        in loopFor h_atual a ac_atual condicao atualizacao corpo e_atual

      -- 4. Se a condição for FALSA, o laço termina.
      B False -> (VVoid, e_cond, h_cond, ac_cond) -- Retorna "void" e o estado atual.

      -- 5. Se a condição não resultar em um booleano, é um erro.
      _ -> (Erro, e_cond, h_cond, ac_cond)

-- IMPLEMENTAÇÃO DE FUN
int h a ac (Fun nome params corpo) e = (fval, wr (nome, fval) e, h, ac)
  where
    -- Cria o valor da função (a clausura)
    fval = Fun (\args exec_state ->
      -- Verifica se o número de argumentos passados é o correto
      if length args /= length params
      then (Erro, exec_state, h, ac) -- Erro de aridade
      else
        -- Cria o escopo local da função, zipando os nomes dos parâmetros com os valores dos argumentos
        let escopoLocal = zip params args
        -- O novo ambiente para o corpo da função é o escopo local mais o ambiente capturado 'a'
        in int h (escopoLocal ++ a) ac corpo exec_state
    )


-- IMPLEMENTAÇÃO DE THIS
int h a ac This e = (search "this" (a ++ e), e, h, ac)
-- lmlo


-- Interpretação do termo Call:
int :: Heap -> Ambiente -> AmbienteClasse -> Termo -> Estado -> (Valor, Estado, Heap, AmbienteClasse)
int h a ac (Call t nome args) e = resultado
  where (vt, e1, h1, ac1)       = int h a ac t e
        (valArgs, ef, hf, acf)  = avaliaArgs args h1 a ac1 e1

        resultado = if isRef vt
                then chamaMetodo (getRef vt) nome valArgs ef hf acf
                else chamaFuncao nome valArgs ef hf acf

          isRef (Ref _) = True
          isRef _       = False

          getRef (Ref i) = i
          getRef _       = error "Valor não é referência (Ref)"

-- Avaliação dos argumentos:
avaliaArgs :: [Termo] -> Heap -> Ambiente -> AmbienteClasse -> Estado -> ([Valor], Estado, Heap, AmbienteClasse)
avaliaArgs [] h _ ac e = ([], e, h, ac)
avaliaArgs (t:ts) h a ac e = (v1 : vs, ef, hf, acf)
  where (v1, e1, h1, ac1)       = int h a ac t e
        (vs, ef, hf, acf)       = avaliaArgs ts h1 a ac1 e1

-- Busca o método em uma lista:
buscaMetodo :: Id -> [Metodo] -> Maybe Metodo
buscaMetodo nome = find (\(Metodo nomeM _ _) -> nome == nomeM)

-- Chamada de método:
chamaMetodo :: Integer -> Id -> [Valor] -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse)
chamaMetodo i nome args e h ac = int h [] ac corpo (thisEnv : escopo ++ estadoObj)
  where Just (classeNome, estadoObj) = lookup i h
        Just (_, metodos, _)         = lookup classeNome ac
        Just (Metodo _ params corpo) = buscaMetodo nome metodos
        escopo                       = zip params args
        thisEnv                      = ("this", Ref i)

-- Chamada de função global:
chamaFuncao :: Id -> [Valor] -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse)
chamaFuncao nome (arg:_) e h ac = f arg e
  where Just (Fun f) = lookup nome ambiente

-- search :: Eq a => a -> [(a, Valor)] -> Valor

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

-- wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]

wr (i,v) [] = [(i,v)]VarVar
