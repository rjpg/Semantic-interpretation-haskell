-- Definicao de variavel
type Variable = Char

-- Definicao de estado ou localizacao de variaveis
type Loc      = [ ( Variable , Aexp ) ]

------------------------------
-- Sintaxe da linguagem IMP --
------------------------------

-- Expressoes aritmeticas
data Aexp     = Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp | Number Int | Var Variable
	        deriving( Show ,  Ord , Eq )
-- Expressoes Booleanas
data Bexp     = Eq Aexp Aexp | Leq Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp | Bval Bool
		deriving( Show , Eq )
-- Comandos
data Comm     = Skip | Attr Variable Aexp | Seq Comm Comm | Ite Bexp Comm Comm | While Bexp Comm
		deriving( Show )

-- Funcoes de acesso aos estados
look_up :: Variable -> Loc -> Aexp
look_up _ [] = error "A variavel nao se encontra definida"
look_up x ( ( y , z ) : t )
	| x == y    = z
	| otherwise = look_up x t

-- Verifica a existencia de uma variavel num estado
exists_var :: Variable -> Loc -> Bool
exists_var _ [] = False
exists_var x y  = let k = [ p | ( p , s ) <- y ] in elem x k

-- Altera o valor de uma variavel num estado
alter_var :: Variable -> Aexp -> Loc -> Loc
alter_var x y ( ( z , s ) : t )
    | x == z    = ( ( x , y ) : t )
    | otherwise = alter_var x y t 

-- Funcoes de avaliacao de expressoes aritmeticas
ariteval :: Aexp -> Loc -> Aexp
ariteval x y = ( Number ( aritevalaux x y ) )
	where
	aritevalaux :: Aexp -> Loc -> Int
	aritevalaux ( Number x ) _ = x
	aritevalaux ( Var c    ) l = aritevalaux ( look_up c l ) l
	aritevalaux ( Add x y  ) l = ( aritevalaux x l ) + ( aritevalaux  y l )
	aritevalaux ( Sub x y  ) l = ( aritevalaux x l ) - ( aritevalaux  y l )
	aritevalaux ( Mul x y  ) l = ( aritevalaux x l ) * ( aritevalaux  y l )

-- Funcoes de avaliacao de expressoes booleanas
beval :: Bexp -> Loc -> Bexp
beval x y = ( Bval ( bevalaux x y ) )
	where
	bevalaux :: Bexp -> Loc -> Bool
	bevalaux ( Bval x    ) _ = x
	bevalaux ( And  x y  ) l = ( bevalaux x l ) && ( bevalaux y l )
	bevalaux ( Or   x y  ) l = ( bevalaux x l ) || ( bevalaux y l )
  	bevalaux ( Not  x    ) l = not ( bevalaux x l )
	bevalaux ( Eq   x y  ) l = ( ariteval x l ) == ( ariteval y l )
	bevalaux ( Leq  x y  ) l = ( ariteval x l ) <= ( ariteval y l )

-- Funcoes de avaliacao de um programa em IMP
commeval :: Comm -> Loc -> Loc
commeval x l = constroi x l
    where
    constroi :: Comm -> Loc -> Loc
    constroi ( Skip )        l = l
    constroi ( Attr x v )    l = attr_eval x ( ariteval v l ) l
    constroi ( Ite b c1 c2 ) l = ite_eval b c1 c2 l
    constroi ( Seq c1 c2 )   l = seq_eval c1 c2 l
    constroi ( While b c )   l = while_eval b c l

    attr_eval :: Variable -> Aexp -> Loc -> Loc
    attr_eval x y l 
	| exists_var x l == False = ( ( x , y ) : l )
	| otherwise               = alter_var x y l
				    
    ite_eval :: Bexp -> Comm -> Comm -> Loc -> Loc
    ite_eval b c1 c2 l
	| beval b l == ( Bval True ) = constroi c1 l
	| otherwise                  = constroi c2 l
				       
    seq_eval :: Comm -> Comm -> Loc -> Loc
    seq_eval c1 c2  l = constroi c2 ( constroi c1 l )

    while_eval :: Bexp -> Comm -> Loc -> Loc
    while_eval x y l
	| beval x l == ( Bval True ) = let k = constroi y l in while_eval x y k
	| otherwise                  = l

main :: Comm -> IO()
main x = putStr "ola mundo"

commevalIter :: IO ()
commevalIter = do c <- getChar
		  putChar c



-- -- -- -- -- --
-- Para Fazer  --
-- -- -- -- -- --
-- Funcoes para impressao dos dados

-- Expressao a ser avaliada

-- Resto do codigo para avaliar

-- Conteudo do estado actual e dos estados precedentes

-- Funcao de parsing

teste2 = main Skip 
 
teste = commeval (  
	Seq ( Attr 'X' ( Number 1 ) ) 
	(While ( Not (  Eq ( Var 'X' ) ( Number (-5) )) )
		(Attr 'X' ( Sub ( Var 'X' ) ( Number 1 ) ) ) ) )
	[]