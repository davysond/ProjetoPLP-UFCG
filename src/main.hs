{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Char (isDigit, toLower)
import System.Directory (doesFileExist, removeFile)
import System.Random
import System.IO
  ( IO,
    IOMode (ReadMode, ReadWriteMode, WriteMode, AppendMode),
    getLine,
    hClose,
    hFlush,
    hGetContents,
    hGetLine,
    hPutStr,
    hPutStrLn,
    openFile,
    putStrLn,
  )
import System.IO.Error ()
import Prelude hiding (catch)

data Gerente = Gerente
  { nomeGerente :: String,
    senhaGerente :: String,
    telefoneGerente :: String
  }
  deriving (Read, Show)

data Cliente = Cliente
  { nomeCliente :: String,
    cpf :: String,
    senha :: String,
    telefone :: String,
    saldo :: String
  }
  deriving (Read, Show)

data Emprestimo = Emprestimo
  { nomeEmprestimo :: String,
    cpfEmprestimo :: String,
    totalParcelas :: String,
    dataDePagamento :: String,
    valorParcela :: String,
    juros :: String,
    valorTotal :: String,
    status :: String
  }
  deriving (Read, Show)

data Investimento = Investimento {
    nomeInvestimento :: String,
    cpfInvestimento :: String,
    valorInvestimento :: String,
    tipoDeInvestimento :: String,
    valorRetornado :: String,
    statusInvestimento :: String
  } deriving (Read, Show)

printLine :: IO ()
printLine = putStrLn "\n------------------------------------------"

main :: IO ()
main = do
  printLine
  putStr "--- Bem-vindo ao Sistema Bancário SBHaskell ---"
  showMenu

showMenu :: IO ()
showMenu = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"

  putStrLn "1 - Sou Gerente"
  putStrLn "2 - Sou Cliente"
  putStrLn "3 - Sair"
  printLine

  putStr "Opção: "
  opcao <- getLine
  menus opcao

menus :: String -> IO ()
menus x
  | x == "1" = acessoGerente
  | x == "2" = menuCliente
  | x == "3" = encerrarSessao
  | otherwise = invalidOption showMenu

menuGerente :: IO ()
menuGerente = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"
  putStrLn "1 - Ver usuários cadastrados no sistema"
  putStrLn "2 - Remover usuários"
  putStrLn "3 - Atualizar contato do gerente"
  putStrLn "4 - Ver empréstimos"
  putStrLn "5 - Ver Investimentos"
  putStrLn "0 - Voltar"
  printLine
  putStr "Opção: "
  opcao <- getLine
  opcaoGerente opcao

opcaoGerente :: String -> IO ()
opcaoGerente x
  | x == "1" = verClientesCadastrados
  | x == "2" = removerCliente
  | x == "3" = atualizarContatoGerente
  | x == "4" = verEmprestimosCadastrados
  | x == "5" = verInvestimentosCadastrados
  | x == "0" = showMenu
  | otherwise = invalidOption menuGerente

menuCliente :: IO ()
menuCliente = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"
  putStrLn "1 - Criar uma conta"
  putStrLn "2 - Logar no sistema como cliente"
  putStrLn "3 - Ver contato do gerente"
  putStrLn "0 - Voltar ao menu principal"
  printLine
  putStr "Opção: "
  opcao <- getLine
  opcaoCliente opcao

opcaoCliente :: String -> IO ()
opcaoCliente x
  | x == "1" = cadastrarComoCliente
  | x == "2" = logarComoCliente
  | x == "3" = verContatoDoGerente
  | x == "0" = showMenu
  | otherwise = invalidOption menuCliente

segundoMenuCliente :: String -> IO ()
segundoMenuCliente cpf = do
  printLine
  putStrLn "\nSelecione o que deseja como cliente\n"
  putStrLn "1 - Consultar dados da minha conta"
  putStrLn "2 - Realizar Saque"
  putStrLn "3 - Realizar Depósito"
  putStrLn "4 - Realizar Empréstimo"
  putStrLn "5 - Realizar Investimento"
  putStrLn "0 - Retornar para o menu"
  printLine
  putStr "Opção: "
  opcao <- getLine
  segundaTelaCliente opcao cpf

segundaTelaCliente :: String -> String -> IO ()
segundaTelaCliente x cpf
  | x == "1" = consultarDados cpf
  | x == "2" = sacar cpf
  | x == "3" = depositar cpf
  | x == "4" = realizarEmprestimo cpf
  | x == "5" = realizarInvestimento cpf
  | x == "0" = menuCliente
  | otherwise = invalidOption (segundoMenuCliente cpf)

atualizarContatoGerente :: IO ()
atualizarContatoGerente = do
  printLine
  putStrLn "\nTem certeza que deseja atualizar o contato do gerente?"
  putStrLn "\n-- Aperte 1 para continuar... --"
  printLine
  opcao <- getLine
  opcaoContato opcao

opcaoContato :: String -> IO ()
opcaoContato x
  | x == "1" = mudaContato
  | otherwise = invalidOption menuGerente

encerrarSessao :: IO ()
encerrarSessao = do
  printLine
  putStrLn "Saindo... até a próxima!"
  printLine

invalidOption :: IO () -> IO ()
invalidOption function = do
  putStrLn "\n# Por favor, selecione uma alternativa válida."
  function

------- Metodos Admnistrador -------

imprimeClientesCadastrados :: [Cliente] -> Int -> IO ()
imprimeClientesCadastrados [] 0 = putStrLn "\n# Nenhum cliente cadastrado!"
imprimeClientesCadastrados [] _ = putStrLn "\n# Clientes listados com sucesso!"
imprimeClientesCadastrados (x : xs) n = do
  putStrLn (show n ++ " - Nome: " ++ obterNomes x ++ " - CPF: " ++ obterCpf x ++ " - Telefone: " ++ obterTelefone x ++ " - Saldo: R$ " ++ obterSaldo x)
  imprimeClientesCadastrados xs (n + 1)

verClientesCadastrados :: IO ()
verClientesCadastrados = do
  arquivoExiste <- doesFileExist "clientes.txt"

  if arquivoExiste then do
    file <- openFile "clientes.txt" ReadMode
    contents <- hGetContents file
    let clientes = lines contents

    printLine
    imprimeClientesCadastrados [read x :: Cliente | x <- clientes] 0
  else do
    putStrLn "\n# Não há clientes cadastrados!"
  menuGerente


removerCliente :: IO ()
removerCliente = do
  clientesCadastrados <- doesFileExist "clientes.txt"
  if not clientesCadastrados
    then do
      putStrLn "# Não há clientes cadastrados!"
    else do
      putStr "\nInsira o CPF do cliente a ser removido: "
      cpf <- getLine

      file <- openFile "clientes.txt" ReadMode
      clientesContent <- hGetContents file
      let clientes = lines clientesContent
      let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] cpf ""

      if not hasCliente
        then do
          putStrLn ("\n# O cliente com CPF: '" ++ cpf ++ "' não existe!")
        else do
          removeFile "clientes.txt"
          let novaListaDeClientes = [read x :: Cliente | x <- clientes, obterCpf (read x :: Cliente) /= cpf]
          atualizaClientes novaListaDeClientes
          putStrLn "# Cliente removido com sucesso!"

  menuGerente

atualizaClientes :: [Cliente] -> IO ()
atualizaClientes [] = putStrLn "# Cliente atualizado com sucesso!\n"
atualizaClientes (x : xs) = do
  clientesCadastrados <- doesFileExist "clientes.txt"
  if not clientesCadastrados
    then do
      file <- openFile "clientes.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
    else appendFile "clientes.txt" ("\n" ++ show x)
  atualizaClientes xs


acessoGerente :: IO ()
acessoGerente = do
  printLine
  putStrLn "\nFaça acesso como gerente: "
  putStr "Senha de gerência: "
  senha <- getLine

  gerenteDados <- readFile "gerente.txt"
  let gerente = read gerenteDados :: Gerente

  if obterGerente gerente "senha" == senha
    then do
      putStrLn ""
      putStrLn "# Login como gerente realizado com sucesso!"
      menuGerente
    else do
      printLine
      putStrLn "# Senha inválida. Por favor, tente novamente!"
      putStr "Deseja tentar fazer login como gerente novamente! (s/n): "
      opcao <- getChar

      if toLower opcao == 's'
        then do
          acessoGerente
        else showMenu



mudaContato :: IO ()
mudaContato = do
  gerenteContent <- readFile "gerente.txt"
  let gerenteDados = read gerenteContent :: Gerente

  putStr "\n# Insira o seu novo número de contato: "
  novoNumero <- getLine

  removeFile "gerente.txt"
  gerenteFile <- openFile "gerente.txt" WriteMode

  let gerente =
        Gerente
          { nomeGerente = obterGerente gerenteDados "nome",
            senhaGerente = obterGerente gerenteDados "senha",
            telefoneGerente = novoNumero
          }

  hPutStr gerenteFile (show gerente)
  hFlush gerenteFile
  hClose gerenteFile

  putStrLn "\n# Contato atualizado com sucesso!"
  menuGerente
------------------------------------

--------- Metodos Clientes ---------
cadastrarComoCliente :: IO ()
cadastrarComoCliente = do
  putStr "\nInsira o seu nome: "
  nome <- getLine

  putStr "Insira o seu CPF: "
  cpf <- getLine

  putStr "Insira a sua senha: "
  senha <- getLine

  putStr "Insira o seu telefone: "
  telefone <- getLine

  putStr "Insira o seu saldo inicial: "
  saldo <- getLine

  fileExists <- doesFileExist "clientes.txt"
  if fileExists
    then do
      file <- openFile "clientes.txt" ReadMode
      contents <- hGetContents file
      let clientes = lines contents
      let hasThisClient = encontraCliente ([read x :: Cliente | x <- clientes]) cpf ""

      if hasThisClient
        then do
          putStrLn ""
          putStrLn "# Usuário já existente!"
          menuCliente
        else do
          putStrLn ""
          putStrLn "# Cliente cadastrado com sucesso!"
          criarCliente nome cpf senha telefone saldo
      else do
        putStrLn "" 
        putStrLn "# Cliente cadastrado com sucesso!"
        criarCliente nome cpf senha telefone saldo

criarCliente :: String -> String -> String -> String -> String -> IO ()
criarCliente nome cpf senha telefone saldo = do
  let cliente = Cliente {nomeCliente = nome, cpf = cpf, senha = senha, telefone = telefone, saldo = saldo}

  clientesCadastrados <- doesFileExist "clientes.txt"

  if clientesCadastrados
    then do
      file <- appendFile "clientes.txt" ("\n" ++ show cliente)
      menuCliente
    else do
      file <- appendFile "clientes.txt" (show cliente)
      menuCliente

logarComoCliente :: IO ()
logarComoCliente = do
  printLine
  putStr "# Por favor, insira o seu CPF: "
  cpf <- getLine
  fileExists <- doesFileExist "clientes.txt"

  if fileExists
    then do
      putStr "# Por favor, insira a sua senha: "
      senha <- getLine
      file <- openFile "clientes.txt" ReadMode
      contents <- hGetContents file
      let clientes = lines contents
      let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] cpf senha

      if hasCliente
        then do
          putStrLn "\n# Login realizado com sucesso!"
          segundoMenuCliente cpf
        else do
          putStrLn "\n# Nome ou senha incorretos. Por favor, insira-os novamente!"
          menuCliente
      hClose file
    else do
      putStrLn "# Nenhum cliente não cadastrado. Por favor, cadastre-se!"
      cadastrarComoCliente

verContatoDoGerente :: IO ()
verContatoDoGerente= do
  gerenteContent <- readFile "gerente.txt"
  let gerente = read gerenteContent :: Gerente
  putStr "\nNome: "
  putStrLn (obterGerente gerente "nome")
  putStr "\nContato: "
  putStrLn (obterGerente gerente "telefone")

  menuCliente

consultarDados :: String -> IO ()
consultarDados cpf = do
  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents
  let cliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  printLine
  putStrLn (show "Nome: " ++ obterNomes cliente ++ " - CPF: " ++ obterCpf cliente ++ " - Telefone: " ++ obterTelefone cliente ++ " - Saldo: R$ " ++ obterSaldo cliente)

  segundoMenuCliente cpf

sacar :: String -> IO ()
sacar cpf = do
  putStr "# Por favor, insira o valor a ser sacado: "
  valor <- getLine

  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents

  let dadosAntigosDoCliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  let saldoAntigo = obterSaldo dadosAntigosDoCliente
  let novoSaldo = (read saldoAntigo :: Double) - (read valor :: Double) 
  if novoSaldo >= 0
    then do
      removeFile "clientes.txt"
      let novaListaDeClientes = [read x :: Cliente | x <- clientes, not (encontrarClienteASerRemovido (read x :: Cliente) cpf)]
      let clienteEditado = Cliente{ nomeCliente = obterNomes dadosAntigosDoCliente,
            cpf = obterCpf dadosAntigosDoCliente,
            senha = obterSenha dadosAntigosDoCliente,
            telefone = obterTelefone dadosAntigosDoCliente,
            saldo = show novoSaldo
          }
      atualizaClientes (novaListaDeClientes ++ [clienteEditado])
      putStrLn "# Saque realizado com sucesso!"
      segundoMenuCliente cpf
      
  else do
    putStrLn "# Saldo insuficiente!"
    segundoMenuCliente cpf

depositar :: String -> IO ()
depositar cpf = do
  putStr "# Por favor, insira o valor a ser depositado: "
  valor <- getLine

  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents

  let dadosAntigosDoCliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  let saldoAntigo = obterSaldo dadosAntigosDoCliente
  let novoSaldo = (read valor :: Double) + (read saldoAntigo :: Double)
  removeFile "clientes.txt"

  let novaListaDeClientes = [read x :: Cliente | x <- clientes, not (encontrarClienteASerRemovido (read x :: Cliente) cpf)]

  let clienteEditado = Cliente{ nomeCliente = obterNomes dadosAntigosDoCliente,
            cpf = obterCpf dadosAntigosDoCliente,
            senha = obterSenha dadosAntigosDoCliente,
            telefone = obterTelefone dadosAntigosDoCliente,
            saldo = show novoSaldo
          }
  atualizaClientes (novaListaDeClientes ++ [clienteEditado])
  putStrLn "# Depósito realizado com sucesso!"
  segundoMenuCliente cpf

depositarEmprestimo :: String -> String -> IO ()
depositarEmprestimo cpf valor = do
  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents

  let dadosAntigosDoCliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  let saldoAntigo = obterSaldo dadosAntigosDoCliente
  let novoSaldo = (read valor :: Double) + (read saldoAntigo :: Double)
  removeFile "clientes.txt"

  let novaListaDeClientes = [read x :: Cliente | x <- clientes, not (encontrarClienteASerRemovido (read x :: Cliente) cpf)]

  let clienteEditado = Cliente{ nomeCliente = obterNomes dadosAntigosDoCliente,
            cpf = obterCpf dadosAntigosDoCliente,
            senha = obterSenha dadosAntigosDoCliente,
            telefone = obterTelefone dadosAntigosDoCliente,
            saldo = show novoSaldo
          }
  atualizaClientes (novaListaDeClientes ++ [clienteEditado])
  putStrLn "# Depósito do emprestimo realizado com sucesso!"

realizarEmprestimo :: String -> IO ()
realizarEmprestimo cpf = do
  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents

  let dadosDoCliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  let cpf = obterCpf dadosDoCliente
  let nome = obterNomes dadosDoCliente

  putStr "# Por favor, insira o valor a ser pego emprestado: "
  valor <- getLine
  putStr "# Insira o número de parcelas que deseja pagar: "
  numeroDeParcelas <- getLine
  let juros = "1.4"
  let dataDeHojeFormatada = "15/07/2022" -- só parar ter uma base
  let dataVencimentoFormatada = "15/07/2023" -- só parar ter uma base
  let valorTotal = show ((read valor :: Double) + (read valor :: Double) * (read juros :: Double))
  let valorParcela = show ((read valorTotal :: Double) / (read numeroDeParcelas :: Double))
  let totalParcelas = numeroDeParcelas
  putStrLn ("Data do Pagamento: Primeiro dia útil do mês. ")
  putStrLn ("Valor da Parcela: " ++ show valorParcela)
  putStrLn ("Juros: " ++ show juros ++ "\n")
  putStrLn ("Valor total a ser pago: " ++ show valorTotal)
  printLine
  let emprestimo = Emprestimo{
    nomeEmprestimo = nome,
    cpfEmprestimo = cpf,
    totalParcelas = totalParcelas,
    dataDePagamento = "Primero dia util do mês", 
    valorParcela = show valorParcela, 
    juros = show juros, 
    valorTotal = show valorTotal,
    status = "Em andamento..."
    }
  depositarEmprestimo cpf valor
  emprestimosCadastrados <- doesFileExist "emprestimos.txt"

  if emprestimosCadastrados
    then do
      file <- appendFile "emprestimos.txt" ("\n" ++ show emprestimo)
      putStrLn "# Empréstimo realizado com sucesso!"
      segundoMenuCliente cpf

    else do
      file <- appendFile "emprestimos.txt" (show emprestimo) 
      segundoMenuCliente cpf
      
realizarInvestimento :: String -> IO ()
realizarInvestimento cpf = do
  clientesContents <- readFile "clientes.txt"
  let clientes = lines clientesContents

  let dadosDoCliente = acharCliente [read x :: Cliente | x <- clientes] cpf
  let cpf = obterCpf dadosDoCliente
  let nome = obterNomes dadosDoCliente

  putStr "# Por favor, insira o valor a ser investido: "
  valor <- getLine
  putStr ""
  putStrLn "Tipos de Investimento: "
  putStrLn "1 - Poupança: -- 2,99% a.a. "
  putStrLn "2 - CDI: -- 3,95% a.a. "
  putStrLn "3 - Tesouro D.: -- 4,5% a.a. "
  putStrLn "4 - LCI: -- 4,39% a.a. "
  putStrLn "5 - LCA: -- 5,06% a.a. "
  putStr "Selecione uma opção de investimento: "
  tInvest <- getLine
  let tInvestimento = tInvest
  let op1 = read tInvestimento
  let rentabilidade = verificaDigito(op1)
  let valorRetornado = (read valor :: Double) * rentabilidade

  let investimento = Investimento{
    nomeInvestimento = nome,
    cpfInvestimento = cpf,
    valorInvestimento = show valor,
    tipoDeInvestimento = tInvestimento,
    valorRetornado = show valorRetornado,
    statusInvestimento = "Em andamento..."
  }

  investimentosCadastrados <- doesFileExist "investimentos.txt"

  if investimentosCadastrados
    then do
      file <- appendFile "investimentos.txt" ("\n" ++ show investimento)
      putStrLn ""
      putStrLn "# Investimento realizado com sucesso!"
      segundoMenuCliente cpf
    else do
      file <- appendFile "investimentos.txt" ("\n" ++ show investimento)   
      segundoMenuCliente cpf

imprimeEmprestimosCadastrados :: [Emprestimo] -> Int -> IO ()
imprimeEmprestimosCadastrados [] 0 = putStrLn "\n# Nenhum empréstimo cadastrado!"
imprimeEmprestimosCadastrados [] _ = putStrLn "\n# Empréstimos listados com sucesso!"
imprimeEmprestimosCadastrados (x : xs) n = do
  putStrLn ("\nEmpréstimo " ++ show n ++ ":" ++ "\n" )
  putStrLn ("Nome: " ++ (nomeEmprestimo x) ++ "\n")
  putStrLn ("CPF: " ++ (cpfEmprestimo x) ++ "\n")
  putStrLn ("Total de parcelas: " ++ (show (totalParcelas x)) ++ "\n")
  putStrLn ("Data de pagamento: " ++ (dataDePagamento x) ++ "\n")
  putStrLn ("Valor da parcela: " ++ (valorParcela x) ++ "\n")
  putStrLn ("Juros: " ++ (juros x) ++ "\n")
  putStrLn ("Valor total: " ++ (valorTotal x) ++ "\n")
  printLine

  imprimeEmprestimosCadastrados xs (n + 1)

verEmprestimosCadastrados :: IO ()
verEmprestimosCadastrados = do
  arquivoExiste <- doesFileExist "emprestimos.txt"

  if arquivoExiste then do
    file <- openFile "emprestimos.txt" ReadMode
    contents <- hGetContents file
    let emprestimos = lines contents

    printLine
    imprimeEmprestimosCadastrados [read x :: Emprestimo | x <- emprestimos] 0
  else do
    putStrLn "\n# Não há emprestimos cadastrados!"
  menuGerente
    
verInvestimentosCadastrados :: IO ()
verInvestimentosCadastrados = do
  arquivoExiste <- doesFileExist "investimentos.txt"

  if arquivoExiste then do
    file <- openFile "investimentos.txt" ReadMode
    contents <- hGetContents file
    let investimentos = lines contents

    printLine
    imprimeInvestimentosCadastrados [read x :: Investimento | x <- investimentos] 0
  else do
    putStrLn "\n# Não há investimentos cadastrados!"
  menuGerente

imprimeInvestimentosCadastrados :: [Investimento] -> Int -> IO ()
imprimeInvestimentosCadastrados [] 0 = putStrLn "\n# Nenhum investimento cadastrado!"
imprimeInvestimentosCadastrados [] _ = putStrLn "\n# Investimentos listados com sucesso!"
imprimeInvestimentosCadastrados (x : xs) n = do
  putStrLn ("\nInvestimento: " ++ show n ++ ":" ++ "\n" )
  putStrLn ("Nome: " ++ (nomeInvestimento x) ++ "\n")
  putStrLn ("CPF: " ++ (cpfInvestimento x) ++ "\n")
  putStrLn ("Valor a investir: " ++ (show(valorInvestimento x)) ++ "\n")
  putStrLn ("Tipo de investimento: " ++ (tipoDeInvestimento x) ++ "\n")
  putStrLn ("Valor retornado: " ++ (show (valorRetornado x)) ++ "\n")
  printLine
  imprimeInvestimentosCadastrados xs (n + 1)

-------- Metodos auxiliares --------

acharCliente :: [Cliente] -> String -> Cliente
-- Procura Cliente somente verificando o cpf
acharCliente (c : cs) cpf 
  | obterCpf c == cpf = c
  | obterCpf c /= cpf = encontrar
  where
    encontrar = acharCliente cs cpf 

encontrarClienteASerRemovido :: Cliente -> String -> Bool
encontrarClienteASerRemovido cliente cpf = do
  obterCpf cliente == cpf 

obterCliente :: Cliente -> String -> String
obterCliente Cliente {nomeCliente = n, cpf = e, senha = s, telefone = t, saldo = sa} prop
  | prop == "nomeCliente" = n
  | prop == "cpf" = e
  | prop == "senha" = s
  | prop == "telefone" = t
  | prop == "saldo" = sa


obterGerente :: Gerente -> String -> String
obterGerente Gerente {nomeGerente = n, senhaGerente = s, telefoneGerente = t} prop
  | prop == "nome" = n
  | prop == "senha" = s
  | prop == "telefone" = t

indexCliente :: [Cliente] -> String -> Int -> Int
indexCliente (c : cs) cpf i
  | obterCliente c "cpf" == cpf = i
  | obterCliente c "cpf" /= cpf = next
  where
    next = indexCliente cs cpf (i + 1)

toStringListCliente :: [Cliente] -> String
toStringListCliente (x : xs) = show x ++ "\n" ++ toStringListCliente xs
toStringListCliente [] = ""

toCliente :: String -> Cliente
toCliente c = read c :: Cliente

toObjListCliente :: [String] -> [Cliente]
toObjListCliente = map toCliente

obterCpf :: Cliente -> String
obterCpf Cliente {nomeCliente = c, cpf = e, senha = s, telefone = t, saldo = sa} = e

obterSenha :: Cliente -> String
obterSenha (Cliente _ _ senha _ _) = senha

obterNomes :: Cliente -> String
obterNomes (Cliente nomeCliente _ _ _ _) = nomeCliente

obterTelefone :: Cliente -> String
obterTelefone (Cliente _ _ _ telefone _) = telefone

obterSaldo :: Cliente -> String
obterSaldo (Cliente _ _ _ _ saldo) = saldo

obterNomeEmprestimo :: Emprestimo -> String
obterNomeEmprestimo (Emprestimo nomeEmprestimo _ _ _ _ _ _ _) = nomeEmprestimo

obterCpfEmprestimo :: Emprestimo -> String
obterCpfEmprestimo (Emprestimo _ cpfEmprestimo _ _ _ _ _ _) = cpfEmprestimo

obterTotalParcelas :: Emprestimo -> String
obterTotalParcelas (Emprestimo _ _ totalParcelas _ _ _ _ _) = totalParcelas

obterDataDePagamento :: Emprestimo -> String
obterDataDePagamento (Emprestimo _ _ _ dataDePagamento _ _ _ _) = dataDePagamento

obterValorParcela :: Emprestimo -> String
obterValorParcela (Emprestimo _ _ _ _ valorParcela _ _ _) = valorParcela

obterJuros :: Emprestimo -> String
obterJuros (Emprestimo _ _ _ _ _ juros _ _) = juros

obterValorTotal :: Emprestimo -> String
obterValorTotal (Emprestimo _ _ _ _ _ _ valorTotal _) = valorTotal

obterStatus :: Emprestimo -> String
obterStatus (Emprestimo _ _ _ _ _ _ _ status) = status

verificaDigito :: Int -> Double
verificaDigito n | n == 1 = 2.9
                 | n == 2 = 3.95
                 | n == 3 = 4.5
                 | n == 4 = 4.39
                 | n == 5 = 5.06
                 | otherwise = 1

encontraCliente :: [Cliente] -> String -> String -> Bool
encontraCliente [] cpf senha = False
-- Procura Cliente somente verificando o cpf
encontraCliente (c : cs) cpf ""
  | obterCliente c "cpf" == cpf = True
  | obterCliente c "cpf" /= cpf = encontrar
  where
    encontrar = encontraCliente cs cpf ""
-- Procura Cliente verificando o cpf e a senha
encontraCliente (c : cs) cpf senha
  | obterCliente c "cpf" == cpf && obterCliente c "senha" == senha = True
  | obterCliente c "cpf" /= cpf || obterCliente c "senha" /= senha = encontrar
  where
    encontrar = encontraCliente cs cpf senha

