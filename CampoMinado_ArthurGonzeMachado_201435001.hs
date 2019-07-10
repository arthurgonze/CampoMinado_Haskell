-- Aluno: Arthur Gonze Machado
-- Matricula: 201435001

{-# LANGUAGE FlexibleInstances #-}
-- Observacao: Eh necessario o pragma acima para declaracao do tipo nao-atomico: 
-- instance AddTres [Int] where ...

-- O tabuleiro eh representado por uma lista de listas.
-- Assume-se que o tabuleiro eh retangular, ou seja, todas as 'sublistas' possuem o mesmo tamanho
-- Assume-se tambem que o tabuleiro nao esta vazio.

module CampoMinado where
import System.Random -- Para gerar numeros aleatorios
import System.IO.Unsafe ( unsafePerformIO ) -- Para pegar o tempo do sistema
import System.Time ( getClockTime , ClockTime(..) ) -- Para pegar o tempo do sistema
import Data.List ( insert, nub, (\\), zipWith4) -- funcoes sobre lista

-- ######################## CONSTRUCAO DE UM TABULEIRO ALEATORIO ########################

-- Escolhendo uma configuracao inicial para o tabuleiro de campo minado.
-- Fazendo escolhas dinamicas: gera uma nova semente para funcao de numeros aleatorios a cada chamada.
-- Gera uma combinacao aleatoria de m elementos a partir de n. ex.: jogada de 0, 1, ..., n-1.
-- O algoritmo usado cria repetidamente escolhas aleatorias ate m valores diferentes sejam encontrados.
-- Assume-se que m<=n.
-- Pos-condicao: o resultado esta em ordem crescente; sem duplicatas.
escolhas :: Int -> Int -> Int -> [Int]
escolhas semente n m
  = fst (escolhasAux ([],aleatorios))
    where
    escolhasAux :: ([Int],[Int]) -> ([Int],[Int])
    escolhasAux (cs,(r:rs))
      | length cs >= m 	= (cs,[])
      | otherwise	= escolhasAux (nub (insert r cs) , rs)
    aleatorios :: [Int]
    aleatorios = randomRs (0::Int,n-1) (mkStdGen semente)
    
-- Uma configuracao inicial aleatoria
-- A semente para gerar numeros aleatorios eh dada pelo tempo do sistema em segundos.
-- Um valor eh escolhido uma vez por sessao: o valor escolhido persiste durante um jogo inteiro.
sementeSessao :: Int
sementeSessao = fromIntegral (case (unsafePerformIO getClockTime) of (TOD s m) -> s) 

-- Uma lista de n escolhas de uma matriz m*p
-- 	m = tamanho da linha
--	p = tamanho da coluna
criaTabuleiroAleatorio :: Int -> Int -> Int -> Int -> [[Bool]]
criaTabuleiroAleatorio semente n m p = pad
    where
    criaMatriz :: Int -> [Int] -> [[Bool]]
    criaMatriz i cs
      | cs==[] = []
      | otherwise = converte primeiro : criaMatriz (i+1) resto
	  where
	  (primeiro,resto) = span ((==i).(flip div m)) cs
	  converte ns = map check [0 .. m-1]
	       where
	       check n = elem n [ x `mod` m | x<-ns ]

    linhas = criaMatriz 0 (escolhas semente (m*p) n)
    pad = linhas ++ replicate (p - length linhas) (replicate m False)

-- Random tabuleiro with a per-session semente.
tabuleiroAleatorio :: Int -> Int -> Int -> [[Bool]]
tabuleiroAleatorio = criaTabuleiroAleatorio sementeSessao 


-- ######################## FIM DA CONSTRUCAO DO TABULEIRO ALEATORIO ########################


-- ######################## LOOP DE JOGO ########################
type Config = [[Bool]]
type Contador  = [[Int]]

class AdcTres a where
  adc3 :: a -> a -> a -> a
  zero :: a
  adcOffset :: [a] -> [a]
  adcOffset = zipOffset3 adc3 zero
  
instance AdcTres Int where
  adc3 n m p = n+m+p
  zero       = 0

instance AdcTres [Int] where
  adc3 = zipWith3 adc3
  zero = repeat zero

-- Combina elemento a elemento(ex.: zipWith3) as tres listas:
--
--	z,a0,a1,a2,...
--	a0,a1,a2,...,an
--  a1,a2,...,an,z
--
-- usando a funcao ternaria f
-- Exp.: f eh a adicao de tres numeros, z eh zero.
zipOffset3 :: (a -> a -> a -> a) -> a -> [a] -> [a]
zipOffset3 f z xs = zipWith3 f (z:xs) xs (tail xs ++ [z])

-- Do tabuleiro ocupado (Booleano) calcula o numero de quadrados adjacentes ocupados.
-- Note que o proprio quadrado entra para conta.
contaConfig :: [[Bool]] -> [[Int]]
contaConfig = adcOffset . map adcOffset . boolPNumero

-- Matriz booleana para matriz numerica; 
-- True para 1, False para 0.
boolPNumero :: [[Bool]] -> [[Int]]
boolPNumero = map (map (\b -> if b then 1 else 0))

pad :: Int -> String -> String
pad n st
  | len <= n		= st ++ replicate (n - len) ' ' 
  | otherwise		= take n st
    where
    len = length st

-- atualiza a lista xs no indice n para armazenar o valor f(xs!!n)
-- trata a excessao de indices fora do intervalo
atualiza :: Int -> (a -> a) -> [a] -> [a]
atualiza n f xs = front ++ rear
		where
		(front,resto) = splitAt n xs
		rear = case resto of
			[]	-> []
			(h:t)	-> f h:t
			
-- atualiza um vetor para armazenar o valor x na posicao (n,m)			
atualizaVetor :: Int -> Int -> a -> [[a]] -> [[a]]
atualizaVetor n m x xss = atualiza n (atualiza m (const x)) xss

-- Mostra jogo
-- Assume que os 2 vetores sao da mesma forma
-- O segundo vetor fornece o contador adjacente da celula,
-- enquanto o primeiro indica se está ou não descoberto.
mostrarJogo :: [[Bool]] -> [[Bool]] -> [[Int]] -> String
mostrarJogo ess mss nss = "   " ++ take (length (head nss)) ['a' .. 'z'] ++ "\n" ++ concat (zipWith4 f [0 .. length nss - 1] ess mss nss)
	     where
	     f n es ms ns = pad 3 (show n) ++ concat (zipWith3 mostrarCelula es ms ns) ++ "\n"

-- Mostra o valor em uma celula particular.
mostrarCelula :: Bool -> Bool -> Int -> String
mostrarCelula abertos marcados n 
	    =   if marcados 
                then "B"
	        else if not abertos 
                then "*"
            else if n==0 
                then "."
	        else show n


-- Funcao para jogar o jogo; passa o numero de minas
-- e o tamanho do tabuleiro(ex.:tam 2 -> 2*2 = 4) como parametros iniciais.
jogar :: Int -> Int -> IO ()
jogar minas tam = tabuleiroDeJogo abertos marcados
   where
   --trata o caso do jogador digitar uma quantidade de minas maior que a metade de espacos no tabuleiro
   tabuleiro    = tabuleiroAleatorio (if (minas > (quot (tam*tam) 2)) then (quot (tam*tam) 2) else minas) tam tam
   contador     = contaConfig tabuleiro			
   abertos      = map (map (const False)) tabuleiro
   marcados     = map (map (const False)) tabuleiro
   
    --  #### COMANDOS ####
    --  s -> sair
    --  r -> revelar
    --  m -> marcar
    --  d -> desmarcar 
   tabuleiroDeJogo :: [[Bool]] -> [[Bool]] -> IO ()
   tabuleiroDeJogo abertos marcados =
     do { 
            putStr (mostrarJogo abertos marcados contador) ;
            jogada <- getChar ;
	        if jogada == 's' -- sai
	            then do { putStr "\nsair" ; return () }
	        else if not (elem jogada "smur") -- ignora jogada invalida
	            then do { putStr "\n" ; tabuleiroDeJogo abertos marcados } -- jogada
	        else 
	            do {
                inptCol <- getChar ;  -- le a coluna do input do jogador
	            let { col = insereInt tam (fromEnum inptCol - fromEnum 'a') } ;
                inptLin <- getChar ;  -- e a linha
	            let { row = insereInt tam (fromEnum inptLin - fromEnum '0') } ; 
	            putStr "\n" ;
	            case jogada of
	            'm' -> tabuleiroDeJogo abertos (atualizaVetor row col True marcados)
	            'd' -> tabuleiroDeJogo abertos (atualizaVetor row col False marcados)
	            'r' -> if tabuleiro!!!(row,col) 
	                    then (do { putStr "Game Over! Você foi explodido!\n" ; return () })
	                   else
	                    (tabuleiroDeJogo (abrePos contador (row,col) abertos)marcados)
	            }
	}
-- Abre a posicao do input do jogador 
abrePos :: [[Int]] -> (Int,Int) -> [[Bool]] -> [[Bool]]
abrePos contador posicao 
  = foldr (.) id $ 
    map ((flip.uncurry) atualizaVetor True) [posicao]

-- Quais são os vizinhos de uma posicao?
vizinhos :: [[Int]] -> (Int,Int) -> [(Int,Int)]
vizinhos contador (p,q) = filter emTab     [           (p-1,q),
                                           (p,q-1),    (p,q),  (p,q+1),
		                                               (p+1,q)
                                           ]   
    where
    emTab (s,t) = 0<=s && s <= linhas &&  0<=t && t <= cols
    linhas = length contador - 1
    cols = length (head contador) -1

-- Insere um valor inteiro no intervalo 
--	0 .. r-1
insereInt :: Int -> Int -> Int
insereInt r val
  | 0<=val && val<r	= val
  | val<0		= 0
  | val>=r 		= r-1

-- Busca em vetor
(!!!) :: [[a]] -> (Int,Int) -> a
xss!!!(p,q) = xss!!p!!q

