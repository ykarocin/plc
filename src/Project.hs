---------------------------------------------------------------

type Id = String
type Numero = Double
data Boolean = True | False

data Termo = Var Id
           | LitB Boolean
           | LitN Numero
           | LitS String
           --
           | Atr Termo Termo
           | Som Termo Termo
           | Sub Termo Termo
           | Mul Termo Termo
           | Div Termo Termo
           --
           | If Comparacao Termo Termo
           | While Comparacao Termo
           | For Atr Comparacao Termo Termo
           --
           | Fun Id [Id] Termo
           | Call Termo Id [Termo]
           | Lam Id Termo
           | Apl Termo Termo 
           | Seq Termo Termo
           --
           | Class Id [Fun] [Id]
           | New Id
           | InstanceOf Termo Id
           
data Comparacao = Eq Termo Termo
               | Ne Termo Termo
               | Lt Termo Termo
               | Le Termo Termo
               | Gt Termo Termo
               | Ge Termo Termo
 

type Estado = [(Var,Termo)]
type Ambiente = [(Var,Termo)]
type Heap = [(Var, (Var,[Estado]))]

