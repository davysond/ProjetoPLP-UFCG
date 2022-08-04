setup_bd :-
  consult('./data/bd_clientes.pl').



arquivo_vazio :-
  \+(predicate_property(cliente(_,_,_,_,_), dynamic)).

adicionaCliente :-
  setup_bd,
  tell('./data/bd_clientes.pl'), nl,
  listing(cliente/5),
  told.

adicionaEmprestimo :-
  setup_bd_emprestimos,
  tell('./data/bd_emprestimos.pl'), nl,
  listing(emprestimo/8),
  told.

adicionaInvestimento :-
  setup_bd_investimentos,
  tell('./data/bd_investimentos.pl'), nl,
  listing(investimento/8),
  told.

cadastraCliente :-
  setup_bd,
  nl, writeln("Insira o seu nome: "),
  read_line_to_string(user_input, Nome),
  nl, writeln("Insira o seu CPF: "),
  read_line_to_string(user_input, Cpf),
  nl, writeln("Insira a sua senha: "),
  read_line_to_string(user_input, Senha),
  nl, writeln("Insira o seu telefone: "),
  read_line_to_string(user_input, Telefone),
  nl, writeln("Insira o saldo inicial: "),
  read(Saldo),
  nl,
  (get_cpf_clientes(Cpfs), member(Cpf, Cpfs) -> nl, writeln("CPF ja cadastrado."), nl, fimMetodo;
  assertz(cliente(Nome, Cpf, Senha, Telefone, Saldo)),
  adicionaCliente,
  writeln("Cliente cadastrado com sucesso!"),nl, fimMetodo),
  fimMetodo.

get_cpf_clientes(Cpfs) :- 
  findall(Cpf, cliente(_,Cpf,_,_,_), Cpfs).

loginCliente(Cpf) :-
  nl,
  writeln("Insira o seu CPF: "),
  read_line_to_string(user_input, Cpf),
  writeln("Insira a sua senha: "),
  read_line_to_string(user_input, Senha),
  (cliente(_, Cpf, Senha, _,_) -> nl, writeln("Login realizado com sucesso!"), nl;
  writeln("Senha incorreta."), nl, fimMetodo, menuCliente).

login_cliente(Cpf) :-
  setup_bd,
  arquivo_vazio -> writeln("Cliente nao cadastrado."), nl, false;
  (cliente(_, _, _, _, _) -> loginCliente(Cpf);
  writeln("Cliente nao cadastrado."), nl, false),
  fimMetodo.

consultaConta(Cpf) :-
  setup_bd_cliente,
  bagof(Nome, cliente(Nome, Cpf, _, _, _), ClienteName),
  atomics_to_string(ClienteName, NomeString),
  bagof(Cpf, cliente(_, Cpf, _, _, _), ClienteCpf),
  atomics_to_string(ClienteCpf, CpfString),
  bagof(Telefone, cliente(_, Cpf, _, Telefone, _), ClienteTelefone),
  atomics_to_string(ClienteTelefone, TelefoneString),
  bagof(Saldo, cliente(_, Cpf, _, _, Saldo), ClienteSaldo),
  atomics_to_string(ClienteSaldo, SaldoString),
  writeln("Dados da conta: "),
  write("Nome: "),
  writeln(NomeString),
  write("CPf: "),
  writeln(CpfString),
  write("Telefone: "),
  writeln(TelefoneString),
  write("Saldo: "),
  writeln(SaldoString),
  told, nl, printLine.


saque(Cpf) :-
  setup_bd_cliente,
  bagof(Nome, cliente(Nome, Cpf, _, _, _), ClienteNome),
  atomics_to_string(ClienteNome, NomeString),
  bagof(Senha, cliente(_, Cpf, Senha, _, _), ClienteSenha),
  atomics_to_string(ClienteSenha, SenhaString),
  bagof(Telefone, cliente(_, Cpf, _, Telefone, _), ClienteTelefone),
  atomics_to_string(ClienteTelefone, TelefoneString),
  bagof(Saldo, cliente(_, Cpf, _, _, Saldo), ClienteSaldo),
  atomics_to_string(ClienteSaldo, SaldoString),
  write("Saldo atual: "),
  writeln(SaldoString),
  nl, writeln("Insira o valor do saque: "),
  read(Valor),
  (Valor > ClienteSaldo -> writeln("Saldo insuficiente."), fimMetodo;
  SaldoNovo is ClienteSaldo - Valor,
  remove_cliente_apos_operacao(Cpf),
  assertz(cliente(NomeString, Cpf, SenhaString, TelefoneString, SaldoNovo)),
  adicionaCliente,
  writeln("Saque realizado com sucesso!"), fimMetodo),
  nl.

deposito(Cpf) :-
  setup_bd_cliente,
  bagof(Nome, cliente(Nome, Cpf, _, _, _), ClienteNome),
  atomics_to_string(ClienteNome, NomeString),
  bagof(Senha, cliente(_, Cpf, Senha, _, _), ClienteSenha),
  atomics_to_string(ClienteSenha, SenhaString),
  bagof(Telefone, cliente(_, Cpf, _, Telefone, _), ClienteTelefone),
  atomics_to_string(ClienteTelefone, TelefoneString),
  bagof(Saldo, cliente(_, Cpf, _, _, Saldo), ClienteSaldo),
  atomics_to_string(ClienteSaldo, SaldoString),
  write("Saldo atual: "),
  writeln(SaldoString),
  nl, writeln("Insira o valor do deposito: "),
  read(Valor),
  SaldoNovo is ClienteSaldo + Valor,
  remove_cliente_apos_operacao(Cpf),
  assertz(cliente(NomeString, Cpf, SenhaString, TelefoneString, SaldoNovo)),
  adicionaCliente,
  writeln("Deposito realizado com sucesso!"), fimMetodo.

fazer_emprestimo(Cpf):-
  setup_bd_cliente,
  bagof(Nome, cliente(Nome, Cpf, _, _, _), ClienteNome),
  atomics_to_string(ClienteNome, NomeString),
  bagof(Senha, cliente(_, Cpf, Senha, _, _), ClienteSenha),
  atomics_to_string(ClienteSenha, SenhaString),
  bagof(Telefone, cliente(_, Cpf, _, Telefone, _), ClienteTelefone),
  atomics_to_string(ClienteTelefone, TelefoneString),
  bagof(Saldo, cliente(_, Cpf, _, _, Saldo), ClienteSaldo),
  atomics_to_string(ClienteSaldo, SaldoString),
  write("Saldo atual: "),
  writeln(SaldoString),
  nl, writeln("Insira o valor do emprestimo: "),
  read(Valor),
  nl, writeln("Insira o numero de parcelas: "),
  read(Parcelas),
  Juros is 0.14,
  ValorTotal is Valor + (Valor * Juros),
  ValorParcela is ValorTotal / Parcelas,
  Status = "Em andamento...",
  SaldoNovo is ClienteSaldo + Valor,
  remove_cliente_apos_operacao(Cpf),
  assertz(cliente(NomeString, Cpf, SenhaString, TelefoneString, SaldoNovo)),
  adicionaCliente,
  adicionar_emprestimo(NomeString, Cpf, Parcelas, ValorParcela, Juros, ValorTotal, Status),
  writeln("Emprestimo realizado com sucesso!"), fimMetodo.

adicionar_emprestimo(Nome, Cpf, Parcelas, ValorParcela, Juros, ValorTotal, Status):-
  setup_bd_emprestimos,
  findall(Id, emprestimo(Id,_,_,_,_,_,_,_), Emprestimos),
  length(Emprestimos, NumeroEmprestimos), 
  IdEmprestimo is NumeroEmprestimos + 1,
  assertz(emprestimo(IdEmprestimo, Nome, Cpf, Parcelas, ValorParcela, Juros, ValorTotal, Status)),
  adicionaEmprestimo.


fazer_investimento(Cpf) :-
  setup_bd_cliente,
  bagof(Nome, cliente(Nome, Cpf, _, _, _), ClienteNome),
  atomics_to_string(ClienteNome, NomeString),
  bagof(Senha, cliente(_, Cpf, Senha, _, _), ClienteSenha),
  atomics_to_string(ClienteSenha, SenhaString),
  bagof(Telefone, cliente(_, Cpf, _, Telefone, _), ClienteTelefone),
  atomics_to_string(ClienteTelefone, TelefoneString),
  bagof(Saldo, cliente(_, Cpf, _, _, Saldo), ClienteSaldo),
  atomics_to_string(ClienteSaldo, SaldoString),
  write("Saldo atual: "),
  writeln(SaldoString),
  nl, writeln("Insira o valor a ser investido: "),
  read(Valor),
  (Valor > ClienteSaldo -> writeln("Saldo insuficiente."), fimMetodo;
  SaldoNovo is ClienteSaldo - Valor,
  remove_cliente_apos_operacao(Cpf),
  assertz(cliente(NomeString, Cpf, SenhaString, TelefoneString, SaldoNovo)),
  adicionaCliente,
  writeln("Tipos de investimentos: "),
  nl, writeln("1 - Poupanca: -- 2,99% a.a."),
  nl, writeln("2 - CDI: -- 3,95% a.a."),
  nl, writeln("3 - Tesouro D.: -- 4,5% a.a."),
  nl, writeln("4 - LCI: -- 4,39% a.a."),
  nl, writeln("5 - LCA: -- 5,06% a.a."),
  nl, writeln("Selecione o tipo de investimento: "),
  read(Tipo),
  (Tipo = 1 -> TipoString = "Poupanca", Taxa = 0.0299, Rendimento = "2.99 % a.a.";
  Tipo = 2 -> TipoString = "CDI", Taxa = 0.0395 , Rendimento = "3.95 % a.a.";
  Tipo = 3 -> TipoString = "Tesouro D.", Taxa = 0.045 , Rendimento = "4.5 % a.a.";
  Tipo = 4 -> TipoString = "LCI", Taxa = 0.0439 , Rendimento = "4.39 % a.a.";
  Tipo = 5 -> TipoString = "LCA", Taxa = 0.0506 , Rendimento = "5.06 % a.a."),
  ValorTotal is Valor + (Valor * Taxa),
  Status = "Em andamento...",
  adicionar_investimento(NomeString, Cpf, TipoString, Valor, Rendimento, ValorTotal, Status),
  writeln("Investimento realizado com sucesso!"), fimMetodo),
  nl.

adicionar_investimento(Nome, Cpf, TipoString, Valor, Rendimento, ValorTotal, Status):-
  setup_bd_investimentos,
  findall(Id, investimento(Id,_,_,_,_,_,_,_), Investimentos),
  length(Investimentos, NumeroInvestimentos),
  IdInvestimento is NumeroInvestimentos + 1,
  assertz(investimento(IdInvestimento, Nome, Cpf, TipoString, Valor, Rendimento, ValorTotal, Status)),
  adicionaInvestimento.

fimMetodo:-
  writeln("Clique em enter para continuar: "),
  read_line_to_string(user_input, _).

  
