module Project where
import Data.List (find)

type Id = String
type Numero = Double

data Termo = Var Id
           | LitB Bool
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

instance Show Termo where
    show (Var i)              = i
    show (LitB b)             = show b
    show (LitN n)             = show n
    show (LitS s)             = show s
    show (Atr t1 t2)          = show t1 ++ " = " ++ show t2
    show (Mul t1 t2)          = show t1 ++ " * " ++ show t2
    show (If c t1 t2)         = "if (" ++ show c ++ ") then " ++ show t1 ++ " else " ++ show t2
    show (While c t)          = "while (" ++ show c ++ ") { " ++ show t ++ " }"
    show (For ini c upd t)    = "for (" ++ show ini ++ "; " ++ show c ++ "; " ++ show upd ++ ") { " ++ show t ++ " }"
    show (Fun nome ps corpo)  = "fun " ++ nome ++ "(" ++ unwords ps ++ ") { " ++ show corpo ++ " }"
    show (Call obj m args)    = show obj ++ "." ++ m ++ "(" ++ unwords (map show args) ++ ")"
    show (Lam p corpo)        = "\\" ++ p ++ " -> " ++ show corpo
    show (Apl t1 t2)          = show t1 ++ " " ++ show t2
    show (Seq t1 t2)          = show t1 ++ " ; " ++ show t2
    show (Class nome _ cs)    = "class " ++ nome ++ " { ... campos: " ++ unwords cs ++ " }"
    show (New nome)           = "new " ++ nome ++ "()"
    show (InstanceOf t cn)    = show t ++ " instanceof " ++ cn
    show This                 = "this"
    show (FieldAccess t c)    = show t ++ "." ++ c
    show (Ref t)              = "ref(" ++ show t ++ ")"

data Comparacao = Eq Termo Termo
               | Ne Termo Termo
               | Lt Termo Termo
               | Le Termo Termo
               | Gt Termo Termo
               | Ge Termo Termo

instance Show Comparacao where
    show (Eq t1 t2) = show t1 ++ " == " ++ show t2
    show (Ne t1 t2) = show t1 ++ " != " ++ show t2
    show (Lt t1 t2) = show t1 ++ " < "  ++ show t2
    show (Le t1 t2) = show t1 ++ " <= " ++ show t2
    show (Gt t1 t2) = show t1 ++ " > "  ++ show t2
    show (Ge t1 t2) = show t1 ++ " >= " ++ show t2

-- Declaração de classes
type ClasseId = String
type CampoId = String
type MetodoId = String
type Classe = (ClasseId, [Metodo], [CampoId])

data Metodo = Metodo MetodoId [Id] Termo

instance Show Metodo where
    show (Metodo nome ps corpo) = "method " ++ nome ++ "(" ++ unwords ps ++ ") { " ++ show corpo ++ " }"

data Valor = B Bool
           | N Numero
           | S String
           | Class ([Metodo], [Id])
           | FunValue (Valor -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse))
           | VVoid 
           | Erro

instance Show Valor where
    show (B b)         = show b
    show (N n)         = show n
    show (S s)         = show s
    show (Class _)     = "Class definition cannot be printed!"
    show (FunValue _)       = "Function definition cannot be printed!"
    show VVoid         = "void"
    show Erro          = "Erro durante a execução do interpretador"


type Estado = [(String,Valor)]
type Ambiente = [(String,Termo)] -- Ambiente de variáveis e funções
type AmbienteClasse = [(ClasseId, Classe)] -- Ambiente de classes
type Heap = [(Integer, (String,Estado))]

int :: [(Integer, (String,Estado))] -> [(String, Termo)] -> [(ClasseId, Classe)] -> Termo -> [(String, Valor)] -> (Valor, [(String,Valor)], [(Integer, (String,Estado))], [(ClasseId, Classe)])
int :: Heap -> Ambiente -> AmbienteClasse -> Termo -> Estado -> (Valor, Estado, Heap, AmbienteClasse)

ambiente = [("*",Fun (\x -> (Fun (\y -> mulValor x y))))]
mulValor (N x) (N y) = N (x*y)
mulValor _ _ = Erro

int h a ac (Var x) e = (search x (a ++ e), e, h, ac)

int h a ac (LitB b) e = (B b, e, h, ac)
int h a ac (LitS s) e = (S s, e, h, ac)
int h a ac (LitN n) e = (N n, e, h, ac)

int h a ac (Atr x t) e = (v1, wr (x,v1) e1, h1, ac1)
                    where (v1,e1,h1, ac1) = int h a ac t e

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

int h a ac (Lam x t) e = (FunValue (\v -> int ((x,v):a) t), e, h, ac)

int h a ac (Apl t u) e = app v1 v2 e2 h2 ac2
                    where (v1,e1, h1, ac1) = int h a ac t e
                          (v2,e2, h2, ac2) = int h a ac1 u e1


int h a ac (Seq t u) e = int h a ac u e1 h1 ac1 
                    where (_,e1, h1, ac1) = int h a ac t e

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


-- Implementação da função int para While - jalr
int h a ac (While cond body) e = 
    case vcond of
        B True -> 
            -- Executa o corpo e chama While recursivamente com novo estado
            let (_, e', h', ac') = int h a ac body e1
            in int h' a ac' (While cond body) e'
        B False -> 
            -- Condição falsa, termina o loop retornando estado atual
            (VVoid, e1, h1, ac1)
        _ -> 
            -- Erro se condição não for booleana
            (Erro, e1, h1, ac1)
    where
        -- Avalia a condição inicial
        (vcond, e1, h1, ac1) = evalCond h a ac cond e


-- Implementação da função int para New - jalr
int h a ac (New className) e = 
    case lookup className ac of
        Just (_, _, campos) -> 
            -- Cria novo ID único para o objeto
            let newId = fromIntegral (length h) + 1
                -- Cria estado inicial com campos padrão
                estadoInicial = map (\campo -> (campo, valorPadrao)) campos
                -- Valor padrão (poderia ser N 0, B False, S "", etc.)
                valorPadrao = N 0  
                -- Adiciona ao heap
                novoHeap = (newId, (className, estadoInicial)) : h
            in (Ref newId, e, novoHeap, ac)
        Nothing -> 
            -- Classe não encontrada
            (Erro, e, h, ac)

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
chamaFuncao nome args e h ac = aplicaArgs funcao args e h ac
  where funcao = case lookup nome ambiente of
        Just (Fun f) -> f
        _            -> \_ e' h' ac' -> (Erro, e', h', ac')

aplicaArgs :: (Valor -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse)) -> [Valor] -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse)
aplicaArgs f []     e h ac = f VVoid e h ac
aplicaArgs f (v:vs) e h ac =
  let (v', e', h', ac') = f v e h ac
  in aplicaArgs (\_ e'' h'' ac'' -> (v', e'', h'', ac'')) vs e' h' ac'



-- search :: Eq a => a -> [(a, Valor)] -> Valor

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

-- wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]

wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]
wr (i,v) [] = [(i,v)]
wr (i,v) ((j,w):l)
  | i == j    = (i,v) : l
  | otherwise = (j,w) : wr (i,v) l

app :: Valor -> Valor -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse)
app (FunValue f) v e h ac = f v e h ac
app _ _ e h ac = (Erro, e, h, ac)