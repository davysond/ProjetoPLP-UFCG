:- include('./cliente.pl').
:- include('./gerente.pl').
                                                          

main :- 
  mostraMenu, nl.

printLine() :- write('-----------------------------------------------'), nl.

mostraMenu :-
  printLine,
  writeln("--- Bem-vindo ao Sistema Bancario SBProlog ---"), nl,
  printLine,
  writeln("Selecione uma das opcoes abaixo:"),
  writeln("1 - Sou Gerente"),
  writeln("2 - Sou Cliente"),
  writeln("3 - Sair"), nl,
  printLine,

  read_line_to_string(user_input, Option),
  (Option == "1" -> login_gerente -> menuGerente;
  Option == "2" -> menuCliente;
  Option == "3" -> sair;
  opcaoInvalida,
  mostraMenu, nl, halt).

menuGerente :-
  writeln("Selecione uma das opcoes abaixo:"), nl,
  writeln("1 - Ver clientes cadastrados no sistema"),
  writeln("2 - Remover clientes"),
  writeln("3 - Atualizar contato do gerente"),
  writeln("4 - Ver Emprestimos"),
  writeln("5 - Ver Investimentos"),
  writeln("0 - Voltar"),
  printLine,
  read_line_to_string(user_input, Option),
  (Option == "1" -> listaClientes, menuGerente;
  Option == "2" -> remove_cliente, menuGerente;
  Option == "3" -> editar_contato_gerente, menuGerente;
  Option == "4" -> listaEmprestimos, menuGerente;
  Option == "5" -> listaInvestimentos, menuGerente;
  Option == "0" -> mostraMenu;
  opcaoInvalida,
  menuGerente).

menuCliente :-
  writeln("Selecione uma das opcoes abaixo:"),
  writeln("1 - Criar uma conta"),
  writeln("2 - Logar no sistema como cliente"),
  writeln("3 - Ver contato do gerente"),
  writeln("0 - Retornar ao menu principal"),
  read_line_to_string(user_input, Option),
  (Option == "1" -> cadastraCliente, menuCliente;
  Option == "2" -> (login_cliente(Cpf) -> segundoMenuCliente(Cpf) ; mostraMenu);
  Option == "3" -> (exibir_contato_gerente, menuCliente);
  Option == "0" -> mostraMenu;
  opcaoInvalida,
  menuCliente).

segundoMenuCliente(Cpf) :-
  writeln("Selecione uma das opcoes abaixo:"), nl,
  writeln("1 - Consultar dados da minha conta"),
  writeln("2 - Realizar Saque"),
  writeln("3 - Realizar Deposito"),
  writeln("4 - Realizar Emprestimo"),
  writeln("5 - Realizar Investimento"),
  writeln("0 - Retornar para o menu principal"),
  printLine,
  read_line_to_string(user_input, Option),
  (Option == "1" -> consultaConta(Cpf), fimMetodo, segundoMenuCliente(Cpf);
  Option == "2" -> saque(Cpf),fimMetodo, segundoMenuCliente(Cpf);
  Option == "3" -> deposito(Cpf),fimMetodo, segundoMenuCliente(Cpf);
  Option == "4" -> fazer_emprestimo(Cpf),fimMetodo, segundoMenuCliente(Cpf);
  Option == "5" -> fazer_investimento(Cpf),fimMetodo, segundoMenuCliente(Cpf);
  Option == "0" -> mostraMenu;
  opcaoInvalida,
  segundoMenuCliente(Cpf)).

exibir_contato_gerente:- nl,
  consult('./data/bd_gerente.pl'),
  gerente("123",_,Contato),
  writeln(Contato),

  writeln("Pressione qualquer tecla para voltar ao menu..."),
  read_line_to_string(user_input, _).

sair :- halt.

opcaoInvalida :-
   writeln("Opcao invalida!"), nl.

