import System.IO

-- Inicialização do tabuleiro com a função replicate
-- vai criar uma lista de 0 onde iremos acessar cada posição da lista de inteiros, e assim fazer o manipulamento da posição
tabuleiro = replicate (6*7) (0::Int)


{-
Definindo labels para ser trabalhadas com tipos diferentes quando formos declarar a composição de determinadas funções
criamos tipos para definição do tabuleiro, opções do menu, comandos para operação do jogo para cada rodada, e uma descrição 
dos valores para o fim do jogo quando apresentar os prints para o usuário.

Utilizamos funções definidas em haskell para realizar as chamadas durantes a criação das funções referentes ao núcleo do jogo, para isso
fazemos o uso da descrição de deriving com o propósito de implementar as funções apresentadas na parametrização da linha.
-}
data NewTabuleiro = Tabuleiro [Int] Int Int Int Int deriving Show
data SelecComandos = Posicao Int | QuemAgora | Desistir deriving (Show, Read, Eq)
data Opcoes = ComecarJogo | TamanhoTabuleiro Int Int | QuantosJogadores Int | ComoJogar | Sair deriving (Show, Read, Eq)
data FimDeJogo = Menu | TenteOutraVez deriving (Show, Read, Eq)

-- simboloTabuleiro Int :: [Char]
{- Essa função descreve uma lista de valores representantes para código de cores no momento que apresentamos os valores no terminal quando realizamos o print.
Contemos o código ansi para demonstrar essas componentes sairem coloridas. As peças serão feitas com os seguintes caracteres ██ sendo uma peça da cor do jogador.
Iremos receber um valor do tipo Int devolvendo como saída uma lista de Char com os caracteres que representam a cor da peça.
-}

simboloTabuleiro :: Int -> [Char]
simboloTabuleiro 0 = "[]"
simboloTabuleiro 1 = "\x1b[36m██\x1b[0m"
simboloTabuleiro 2 = "\x1b[31m██\x1b[0m"
simboloTabuleiro 3 = "\x1b[32m██\x1b[0m"
simboloTabuleiro 4 = "\x1b[33m██\x1b[0m"
simboloTabuleiro 5 = "\x1b[35m██\x1b[0m"
simboloTabuleiro x = "??"


-- getCorTabuleiro :: NewTabuleiro -> Int -> [Char]
{-
Função que recebe dois parâmetros, um tabuleiro, e um Int com o valor da posição que desejamos acessar para acessar os símbolos que descrevem nosso jogo
como é uma lista 6 referências para resultados de caracteres modificados, e o caracter considerado como sendo um posição vazia sendo [], assim retornando o valor contido naquela posição
-}
getCorTabuleiro :: NewTabuleiro -> Int -> [Char]
getCorTabuleiro (Tabuleiro t h w ns nj) x = simboloTabuleiro (t !! x)

-- tabuleiroNucleo :: Int -> Int -> [Char]
{-
Função que apresenta como parâmetros dois tipos Int e um resultado de [Char] gerando as posições de todo o tabuleiro para ser salvo os resultado, garantindo o preenchimento da matriz
com espaços para que os valores não fiquem sobrepostos quando for desenvolvida a função para printar o tabuleiro do jogo. Nessa função definimos o formato do tabuleiro quando estamos 
printando uma posição com determinado conteúdo, ou quando estamos apresentando espaços.
-}
tabuleiroNucleo :: Int -> Int -> [Char]
tabuleiroNucleo p x | x == p-1 = if x < 10 then "   " ++ show x else "  " ++ show x
tabuleiroNucleo p x | x == 0 = (replicate (p*4) '-') ++ "\n" ++ show x ++ tabuleiroNucleo p (x+1)
tabuleiroNucleo p x | x < 11 = "   " ++ show x ++ tabuleiroNucleo p (x + 1)
tabuleiroNucleo p x = "  " ++ show x ++ tabuleiroNucleo p (x+1)


-- formatarTabuleiro :: NewTabuleiro -> Int -> [Char]
{-
Função para formatar o tabuleiro com o formato que desejamos printar utilizando as funções passadas para obter o tamanho das representações de cada posição do tabuleiro que colocamos para
peça que desejamos posicionar, e fazendo os espaçamentos desejados com as operações de quebra de linha utilizando como parâmetro para percorrer as suas posições. Faz a manipulação até o último
valor da coluna da matriz.
-}
formatarTabuleiro :: NewTabuleiro -> Int -> [Char]
formatarTabuleiro (Tabuleiro t h w ns nj) x | x == (w*h-1) = getCorTabuleiro (Tabuleiro t h w ns nj) x ++ "\n" ++ tabuleiroNucleo w 0
formatarTabuleiro (Tabuleiro t h w ns nj) x | ((x `mod` w) == 0 && x/=0) = "\n\n" ++ getCorTabuleiro (Tabuleiro t h w ns nj) x ++ "  " ++ formatarTabuleiro (Tabuleiro t h w ns nj) (x+1)
formatarTabuleiro (Tabuleiro t h w ns nj) x = getCorTabuleiro (Tabuleiro t h w ns nj) x ++ "  " ++ formatarTabuleiro (Tabuleiro t h w ns nj) (x+1)

-- printTabuleiro :: NewTabuleiro -> [Char]
-- Faz o print do tabuleiro com todas as posições informando o seu tamanho de comprimento h, e largura w, para saber o tamanho do tabuleiro que desejmaos imprimir
printTabuleiro :: NewTabuleiro -> [Char]
printTabuleiro (Tabuleiro t h w ns nj) = formatarTabuleiro (Tabuleiro t h w ns nj) 0

-- quantJogadores :: Int -> Int -> [Char]
{-
Essa função recebe a quantidade de jogadores que o jogo irá conter, e assim retorna os valores do número do jogador, com sua respectiva cor acessada na função simboloTabuleiro
-}
quantJogadores :: Int -> Int -> [Char]
quantJogadores x nj
    | (x < nj) = ("\n- Jogador " ++ show(x) ++ " (" ++ simboloTabuleiro x ++ ")" ++ quantJogadores (x+1) nj)
    | otherwise = ("\n- Jogador " ++ show(x) ++ " (" ++ simboloTabuleiro x ++ ")")

-- mostraJogadores :: Int -> [Char]
-- Apresenta a quantidade de jogadores presentes na partida
mostraJogadores :: Int -> [Char]
mostraJogadores nj = ("\nJogadores:\n\n***************" ++ quantJogadores 1 nj ++ "\n***************")

-- getTabPositions :: NewTabuleiro -> Int -> Int
{-
Recebe um tabuleiro como entrada da função e um Int, retornando o conteúdo da posição acessada no tabuleiro que será passado com o operador de index !!
-}
getTabPositions :: NewTabuleiro -> Int -> Int
getTabPositions (Tabuleiro t h w ns nj) a = (t !! a)

-- isEmpty :: NewTabuleiro -> Int -> Bool
{-
Recebe um tabuleiro como entrada, e uma posição que deseja verificar para retornar se está com a posição vazia, ou não
-}
isEmpty :: NewTabuleiro -> Int -> Bool
isEmpty (Tabuleiro t h w ns nj) a
    | (getTabPositions (Tabuleiro t h w ns nj) a /= 0) = True
    | otherwise = False

-- Realizando as condições de vitória de cada jogador
{-
Nessa parte do código fazemos a verificação de cada posição do tabuleiro que em relação a cor do jogador, junto da sua posição para analisar as condições de vitoria
para cada jogada realizada. Temos que verificar as jogadas para fechar o jogo nas seguintes direções: Vertical, Horizontal, Diagonais Direita e Esquerda (Inversa e não Inversa) 

Essas funções recebem os seguintes tipos: NewTabuleiro -> Int -> Int -> Int representando o tabuleiro, jogador, e uma posição que recebemos do tabuleiro. Temos que verificar 
sempre que uma peça nova foi posicionada para emsma cor de jogador quando recebemos do tabuleiro, tentando assim chegar até o valor de 4 para considerar a peça vitoriosa.

usamos a função `div` para pegar a parte inteira do resultado recebido, junto da verificação das posições anteriores para a verificação das posições horizontais, e diagonais
-}
verificaVertical :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaVertical (Tabuleiro t h w ns nj) j p
    | ((t!!p) == j) = if ((p + w) < h*w) then (1 + verificaVertical (Tabuleiro t h w ns nj) j (p + w)) else 1
verificaVertical (Tabuleiro t h w ns nj) j p = 0

verificaHorizontalEsq :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaHorizontalEsq (Tabuleiro t h w ns nj) j p
    |  ((t!!p) == j) = if((p - 1) >= 0 && ((p `div` w) == ((p-1) `div` w))) then (1 + verificaHorizontalEsq (Tabuleiro t h w ns nj) j (p-1)) else 1
verificaHorizontalEsq (Tabuleiro t h w ns nj) j p = 0

verificaHorizontalDir :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaHorizontalDir (Tabuleiro t h w ns nj) j p 
    | ((t!!p) == j) = if((p+1) < h*w && ((p `div` w) == ((p+1) `div` w))) then (1 + verificaHorizontalDir (Tabuleiro t h w ns nj) j (p+1)) else 1
verificaHorizontalDir (Tabuleiro t h w ns nj) j p = 0

verificaDiagonalInvEsq :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaDiagonalInvEsq (Tabuleiro t h w ns nj) j p 
    | ((t!!p) == j) = if((p - w - 1) >=0 && ((p `div` w) == (((p - w - 1) `div` w) +1))) then (1 + verificaDiagonalInvEsq (Tabuleiro t h w ns nj) j (p-w-1)) else 1
verificaDiagonalInvEsq (Tabuleiro t h w ns nj) j p = 0

verificaDiagonalInvDir :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaDiagonalInvDir (Tabuleiro t h w ns nj) j p 
    | ((t!!p) == j) = if((p+w+1) < h*w && ((p `div` w) == (((p+w+1) `div` w) -1))) then (1 + verificaDiagonalInvDir (Tabuleiro t h w ns nj) j (p+w+1)) else 1
verificaDiagonalInvDir (Tabuleiro t h w ns nj) j p = 0

verificaDiagonalEsq :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaDiagonalEsq (Tabuleiro t h w ns nj) j p 
    | ((t!!p) == j) = if((p + w - 1) < h*w && ((p `div` w) == (((p + w - 1) `div` w) -1))) then (1 + verificaDiagonalEsq (Tabuleiro t h w ns nj) j (p+w-1)) else 1
verificaDiagonalEsq (Tabuleiro t h w ns nj) j p = 0

verificaDiagonalDir :: NewTabuleiro -> Int -> Int -> Int -- tabuleiro, jogador, posição
verificaDiagonalDir (Tabuleiro t h w ns nj) j p 
    | ((t!!p) == j) = if((p+w+1) < h*w && ((p `div` w) == (((p+w+1) `div` w) -1))) then (1 + verificaDiagonalDir (Tabuleiro t h w ns nj) j (p+w+1)) else 1
verificaDiagonalDir (Tabuleiro t h w ns nj) j p = 0

-- verificaJogo :: NewTabuleiro -> Int -> Int -> Bool
{-
Função principal para validação de todas as posições para verificar analisando os valores de 4 posições de mesma cor para retornar o valor True, e ficando retornando False quando
ainda não formar 4 peças iguais.
-}
verificaJogo :: NewTabuleiro -> Int -> Int -> Bool -- tabuleiro, jogador, posição
verificaJogo (Tabuleiro t h w ns nj) j p
    | ((verificaVertical (Tabuleiro t h w ns nj) j p) >= 4) = True
    | ( - 1 + (verificaHorizontalEsq (Tabuleiro t h w ns nj) j p) + (verificaHorizontalDir (Tabuleiro t h w ns nj) j p) >= 4) = True
    | ( - 1 + (verificaDiagonalInvEsq (Tabuleiro t h w ns nj) j p) + (verificaDiagonalInvDir (Tabuleiro t h w ns nj) j p) >= 4) = True
    | ( - 1 + (verificaDiagonalEsq (Tabuleiro t h w ns nj) j p) + (verificaDiagonalDir (Tabuleiro t h w ns nj) j p) >= 4) = True
    | otherwise = False

-- alteraTabuleiro :: NewTabuleiro -> Int -> Int -> Int -> [Int]
{-
Realizamos a alteração do tabuleiro para as posições novas da matriz que iremos gerar de mais posições quando entramos. Somos capazes de realocar as posições de acesso a matriz
junto a lista [Int] destino.
-}
alteraTabuleiro :: NewTabuleiro -> Int -> Int -> Int -> [Int] -- tabuleiro, jogador, posição nova, posição atual
alteraTabuleiro (Tabuleiro t h w ns nj) j x y
    | (y > h*w-1) = []
    | y == x = [j] ++ alteraTabuleiro (Tabuleiro t h w ns nj) j x (y+1)
alteraTabuleiro (Tabuleiro t h w ns nj) j x y = [t!!(y)] ++ alteraTabuleiro (Tabuleiro t h w ns nj) j x (y+1)


-- novoTabuleiro :: NewTabuleiro -> Int -> Int -> NewTabuleiro
{-
Nessa função gera o novo tabuleiro para a partida desejada realizando a sua alteração no formato desejado. Temos como saída um novo tabuleiro para ser apresentado ao jogador
-}
novoTabuleiro :: NewTabuleiro -> Int -> Int -> NewTabuleiro
novoTabuleiro (Tabuleiro t h w ns nj) j p = (Tabuleiro (alteraTabuleiro (Tabuleiro t h w ns nj) j p 0) h w ns nj)

-- proxTurno :: Int -> Int -> Int
{-
Controle de passagem de turno entre os jogadores, realizando com os parâmetros Int -> Int -> Int e devolvendo qual número do jogador da vez
-}
proxTurno :: Int -> Int -> Int
proxTurno j nj
    | (j < nj) = j + 1
proxTurno j nj = 1


-- Processamento do jogo

-- turnoJogador :: NewTabuleiro -> Int -> IO()
{-
Função que apresenta para o jogador opções para serem utilizadas do dado definido anteriormente para acesso, definindo qual jogador é e suas respectivas opções para serem feitas
-}
turnoJogador :: NewTabuleiro -> Int -> IO()
turnoJogador (Tabuleiro t h w ns nj) y = do
    putStrLn("Você é o Jogador" ++ (show y) ++ "(" ++ (simboloTabuleiro y) ++ ")")
    putStrLn("\nComandos: \n\t- Posicao x\n\t- QuemAgora\n\t- \x1b[36mDesistir\x1b[0m\n")
    entrada <- getLine
    let comando = (read entrada :: SelecComandos)
    executaComando (Tabuleiro t h w ns nj) y comando

-- posicionamento :: NewTabuleiro -> Int -> Int -> IO()
{-
Como descrição do trabalho queremos representar essas funções empregando resultados de IO() para apresentar ao usuário na tela um valor desejado. Nessa função de posicionamento desenvolve
a manipulação da verifição dos movimentos gerados por cada jogador, cuidando com isso a posição que for aplicar na hora que o jogador realizar o comando, analisar as possibilidades de fechamento do jogo 
como um empate, ou uma vitória, sempre aplicando uma função de realizar um novoTabuleiro para posicionar os novos valores na tela com as cores das peças que forem posicionadas.
-}
posicionamento :: NewTabuleiro -> Int -> Int -> IO() -- tabuleiro, jogador, posição
posicionamento (Tabuleiro t h w ns nj) j p
    | (p < 0) = do
              putStrLn("Movimento Invalido!!")
              turnoJogador(Tabuleiro t h w ns nj) j

posicionamento (Tabuleiro t h w ns nj) j p = if (isEmpty (Tabuleiro t h w ns nj) p == True) then posicionamento (Tabuleiro t h w ns nj) j (p-w) else do
    putStrLn ("\ESC[2J")
    let printNovo = novoTabuleiro (Tabuleiro t h w (ns+1) nj) j p
    putStrLn (printTabuleiro (printNovo))
    if(verificaJogo (printNovo) j p == True) then do
        putStrLn ("Jogador" ++ show j ++ "(" ++ (simboloTabuleiro j) ++ ") GANHOU")
        fimJogo(printNovo)
    else if(ns+1 >= h*w) then do
        putStrLn ("EMPATE")
        fimJogo(printNovo)
    else turnoJogador printNovo (proxTurno j nj)

-- Comandos de operação do jogo

-- executaComando :: NewTabuleiro -> Int -> SelecComandos -> IO()
{-
Essa função é descrita para executar a função de seleção de comandos para cada jogada no tabuleiro
-}
executaComando :: NewTabuleiro -> Int -> SelecComandos -> IO()
executaComando (Tabuleiro t h w ns nj) y (QuemAgora) = do
    turnoJogador(Tabuleiro t h w ns nj) y

executaComando (Tabuleiro t h w ns nj) y (Posicao x) = if (x<0 || x>=w) then turnoJogador (Tabuleiro t h w ns nj) y else
    do
        posicionamento (Tabuleiro t h w ns nj) y (w*(h-1)+x)

executaComando (Tabuleiro t h w ns nj) y (Desistir) = do
    putStrLn("Jogador" ++ (show y) ++ "(" ++ (simboloTabuleiro y) ++ ") terminou o jogo por desistência!!")
    fimJogo(Tabuleiro t h w ns nj)


-- comecaJogo :: NewTabuleiro -> IO()
{-
Função para começar a executar o jogo levando em quanto o turno inicial do jogador 1, printando o primeiro tabuleiro sem nenhuma modificação.
-}
comecaJogo :: NewTabuleiro -> IO()
comecaJogo (Tabuleiro t h w ns nj) = do
    putStrLn (printTabuleiro (Tabuleiro t h w ns nj))
    turnoJogador (Tabuleiro t h w ns nj) 1

-- verificaOpcao :: NewTabuleiro -> Opcoes -> IO()
-- Função para descrever o menu principal do jogo relacionando todos as opções possíveis na minha lista destinada para cuidar dos casos possíveis.
verificaOpcao :: NewTabuleiro -> Opcoes -> IO()
verificaOpcao (Tabuleiro t h w ns nj) ComecarJogo = do
    putStrLn ("\ESC[2J")
    comecaJogo(Tabuleiro t h w ns nj)

verificaOpcao (Tabuleiro t h w ns nj) (TamanhoTabuleiro x y) = do
    mudarTamanhoTabuleiro x y nj

verificaOpcao (Tabuleiro t h w ns nj) (QuantosJogadores x) = do
    mudarQuantJogadores (Tabuleiro t h w ns nj) x

verificaOpcao (Tabuleiro t h w ns nj) ComoJogar = do
    comoJogar (Tabuleiro t h w ns nj)

verificaOpcao (Tabuleiro t h w ns nj) (Sair) = do
    sairJogo

-- initJogo :: NewTabuleiro -> IO()
-- contém a função para inicializar o tabuleiro na tela, exibindo as funções do menu para ser utilizada na função anterior descrita que verifica qual opção escolhemos
initJogo :: NewTabuleiro -> IO()
initJogo (Tabuleiro t h w ns nj) = do
    putStrLn (printTabuleiro (Tabuleiro t h w ns nj))
    putStrLn (mostraJogadores nj)
    putStrLn "\nComandos: \n\n\t- ComecarJogo \n\t- TamanhoTabuleiro #comprimento #largura\n\t- QuantosJogadores #count (2-5) \n\t- ComoJogar\n\t- \x1b[31mSair\x1b[0m"
    entrada <- getLine
    let comando = (read entrada :: Opcoes)
    verificaOpcao (Tabuleiro t h w ns nj) comando 

-- mudarQuantJogadores :: NewTabuleiro -> Int -> IO()
-- Recebe do teclado a quantidade de jogadores que desejamos ter em uma partida para ser iniciada, recebemos no máximo 5 jogadores em relação das cores disponíveis para serem printadas
-- no terminal.
mudarQuantJogadores :: NewTabuleiro -> Int -> IO()
mudarQuantJogadores (Tabuleiro t h w ns nj) x | (x>=2 && x<=5) = initJogo(Tabuleiro t h w 0 x)
mudarQuantJogadores (Tabuleiro t h w ns nj) x = initJogo (Tabuleiro t h w 0 nj)

-- mudarTamanhoTabuleiro :: Int -> Int -> Int -> IO()
-- Essa função tem o propósito de realizar a reformatação para do tamanho que desejamos aplicar para o tabuleiro. Limitamos para um tamanho mínimo de uma matriz 4x4, recebendo os
-- valores de entrada da função de Int
mudarTamanhoTabuleiro :: Int -> Int -> Int -> IO()
mudarTamanhoTabuleiro x y nj
    | x>=4 && y>=4 = do
        let w = y
        let h = x
        let n = replicate (h*w) (0::Int)
        putStr ("\ESC[2J")
        putStr ("\nRedimensionando posições do tabuleiro em [" ++ show h ++ "," ++ show w ++ "].\n\n")
        initJogo(Tabuleiro n h w 0 nj)
        return ()
mudarTamanhoTabuleiro x y nj = mudarTamanhoTabuleiro 6 7 nj

sairJogo :: IO()
sairJogo = putStr ("Saindo...\n")

-- comoJogar :: NewTabuleiro -> IO()
-- Função para apresentar ao jogador como funciona o jogo no terminal
comoJogar :: NewTabuleiro -> IO()
comoJogar (Tabuleiro t h w ns nj)= do
    putStr ("Você está jogando Liga4 pela linguagem Haskell!!\nBasicamente o jogo consiste em 2 jogadores com um tabuleiro para posicionar suas peças\nde forma a construir conexões de 4 peças (em quaisquer direção) da mesma cor a fim de conectar 4 peças\nvence o jogador que formar a primeira ligação primeiro!!\n\nPodemos colocar um tamanho diferente para o tabuleiro com a função TamanhoTabuleiro\npassando os valores da matriz que desejamos\nSelecionamos a quantidade de jogadores desejadas com a função QuantosJogadores\n\nDica.: Não coloque errado os nomes das funções no terminal pois o programa encontra uma exceção e encerra\n\n") 
    initJogo(Tabuleiro t h w ns nj)
    return ()

-- comandoFimPartida :: NewTabuleiro -> FimDeJogo -> IO()
-- função para analisar quando uma partida foi encerrada quando algum determinado jogador perder ou empatar
comandoFimPartida :: NewTabuleiro -> FimDeJogo -> IO()
comandoFimPartida (Tabuleiro t h w ns nj) TenteOutraVez = comecaJogo (Tabuleiro t h w ns nj)
comandoFimPartida (Tabuleiro t h w ns nj) Menu = initJogo (Tabuleiro t h w ns nj)

-- fimJogo :: NewTabuleiro 
-- função para analisar quando algum jogador venceu a partida pegando o valor desejado para o comando recebido do usuário
fimJogo :: NewTabuleiro -> IO()
fimJogo (Tabuleiro t h w ns nj) = do
    putStr("\n\x1b[33mTenteOutraVez?\x1b[0m\n- TenteOutraVez\n- Menu\n>")
    entrada <- getLine
    let comando = (read entrada :: FimDeJogo)
    putStr ("\ESC[2J")
    comandoFimPartida (Tabuleiro (replicate (h*w) (0::Int)) h w 0 nj) comando

main = do
    putStrLn ("\ESC[2J")
    initJogo (Tabuleiro tabuleiro 6 7 0 2)
