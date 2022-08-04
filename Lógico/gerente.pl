:- consult('./data/bd_clientes.pl').



setup_bd_cliente :-
  consult('./data/bd_clientes.pl').

setup_bd_emprestimos :-
  consult('./data/bd_emprestimos.pl').

setup_bd_investimentos :-
  consult('./data/bd_investimentos.pl').

setup_bd_login :-
  consult('./data/bd_gerente.pl').

arquivo_vazio_adm :-
  \+(predicate_property(gerente(_,_,_), dynamic)).

loginGerente :-
  nl,
  writeln("Insira o seu CPF: "),
  read_line_to_string(user_input, Cpf),
  writeln("Insira a sua senha: "),
  read_line_to_string(user_input, Senha),
  (gerente(Cpf, Senha, _) -> nl, writeln("Login realizado com sucesso!"), nl;
  writeln("Senha incorreta."), nl, fimMetodo, mostraMenu).

login_gerente :-
  setup_bd_login,
  arquivo_vazio_adm -> writeln("Gerente nao cadastrado."), nl, false;
  (gerente(_, _, _)) -> loginGerente;
  writeln("Gerente nao cadastrado."), nl, false.

listaClientes :- 
  setup_bd_cliente,
  findall(C, cliente(_, C, _, _,_), ListaClientes),
  printLine,
  writeln("Clientes cadastrados: "),
  printLine,
  exibeClientes(ListaClientes),
  told, nl.

exibeClientes([]) :-
  nl,
  writeln("Nenhum usuario cadastrado.").

exibeClientes([H]) :-
  write("- "),
  consultaConta(H),
  fimMetodoAdm.

exibeClientes([H|T]) :-
  write("- "),
  consultaConta(H),
  exibeClientes(T).


listaEmprestimos :-
  setup_bd_emprestimos,
  findall(Id, emprestimo(Id,_, _, _, _, _, _, _), ListaEmprestimos),
  printLine,
  writeln("Emprestimos cadastrados: "),
  printLine,
  exibeEmprestimos(ListaEmprestimos),
  told, nl.

exibeEmprestimos([]) :-
  nl,
  writeln("Nenhum emprestimo cadastrado.").

exibeEmprestimos([H]) :-
  consultaEmprestimo(H),
  fimMetodoAdm.

exibeEmprestimos([H|T]) :-
  consultaEmprestimo(H),
  exibeEmprestimos(T).

consultaEmprestimo(Id) :-
  setup_bd_emprestimos,
  bagof(Id, emprestimo(Id,_,_,_,_,_,_,_), EmprestimoId),
  atomics_to_string(EmprestimoId, IdString),
  bagof(Nome, emprestimo(Id,Nome,_,_,_,_,_,_), EmprestimoNome),
  atomics_to_string(EmprestimoNome, NomeString),
  bagof(Cpf, emprestimo(Id,_,Cpf,_,_,_,_,_), EmprestimoCpf),
  atomics_to_string(EmprestimoCpf, CpfString),
  bagof(Parcelas, emprestimo(Id,_,_,Parcelas,_,_,_,_), EmprestimoParcelas),
  atomics_to_string(EmprestimoParcelas, ParcelasString),
  bagof(ValorParcela, emprestimo(Id,_,_,_,ValorParcela,_,_,_), EmprestimoValorPArcela),
  atomics_to_string(EmprestimoValorPArcela, ValorParcelaString),
  bagof(Juros, emprestimo(Id,_,_,_,_,Juros,_,_), EmprestimoJuros),
  atomics_to_string(EmprestimoJuros, JurosString),
  bagof(ValorTotal, emprestimo(Id,_,_,_,_,_,ValorTotal,_), EmprestimoValorTotal),
  atomics_to_string(EmprestimoValorTotal, ValorTotalString),
  bagof(Status, emprestimo(Id,_,_,_,_,_,_,Status), EmprestimoStatus),
  atomics_to_string(EmprestimoStatus, StatusString),
  write("Id: "),
  writeln(IdString),
  write("Nome: "),
  writeln(NomeString),
  write("CPF: "),
  writeln(CpfString),
  write("Parcelas: "),
  writeln(ParcelasString),
  write("Valor da parcela: "),
  writeln(ValorParcelaString),
  write("Juros: "),
  writeln(JurosString),
  write("Valor total: "),
  writeln(ValorTotalString),
  write("Status: "),
  writeln(StatusString),
  printLine.

listaInvestimentos :-
  setup_bd_investimentos,
  findall(Id, investimento(Id,_, _, _, _, _, _, _), ListaInvestimentos),
  printLine,
  writeln("Investimentos cadastrados: "),
  printLine,
  exibeInvestimentos(ListaInvestimentos),
  told, nl.

exibeInvestimentos([]) :-
  nl,
  writeln("Nenhum investimento cadastrado.").

exibeInvestimentos([H]) :-
  consultaInvestimento(H),
  fimMetodoAdm.

exibeInvestimentos([H|T]) :-
  consultaInvestimento(H),
  exibeInvestimentos(T).

consultaInvestimento(Id) :-
  setup_bd_investimentos,
  bagof(Id, investimento(Id,_,_,_,_,_,_,_), InvestimentoId),
  atomics_to_string(InvestimentoId, IdString),
  bagof(Nome, investimento(Id,Nome,_,_,_,_,_,_), InvestimentoNome),
  atomics_to_string(InvestimentoNome, NomeString),
  bagof(Cpf, investimento(Id,_,Cpf,_,_,_,_,_), InvestimentoCpf),
  atomics_to_string(InvestimentoCpf, CpfString),
  bagof(Tipo, investimento(Id,_,_,Tipo,_,_,_,_), InvestimentoTipo),
  atomics_to_string(InvestimentoTipo, TipoString),
  bagof(Valor, investimento(Id,_,_,_,Valor,_,_,_), InvestimentoValor),
  atomics_to_string(InvestimentoValor, ValorString),
  bagof(Rendimento, investimento(Id,_,_,_,_,Rendimento,_,_), InvestimentoRendimento),
  atomics_to_string(InvestimentoRendimento, RendimentoString),
  bagof(ValorTotal, investimento(Id,_,_,_,_,_,ValorTotal,_), InvestimentoValorTotal),
  atomics_to_string(InvestimentoValorTotal, ValorTotalString),
  bagof(Status, investimento(Id,_,_,_,_,_,_,Status), InvestimentoStatus),
  atomics_to_string(InvestimentoStatus, StatusString),
  write("ID: "),
  writeln(IdString),
  write("Nome: "),
  writeln(NomeString),
  write("CPF: "),
  writeln(CpfString),
  write("Tipo: "),
  writeln(TipoString),
  write("Valor: "),
  writeln(ValorString),
  write("Rendimento: "),
  writeln(RendimentoString),
  write("Valor total: "),
  writeln(ValorTotalString),
  write("Status: "),
  writeln(StatusString),
  printLine.

add_clientes([]).
add_clientes([[Nome,Cpf,Senha,Telefone, Saldo]|T]) :- 
  add_cliente(Nome,Cpf,Senha,Telefone, Saldo), add_clientes(T).

add_cliente(Nome,Cpf,Senha,Telefone, Saldo) :- 
  assertz(cliente(Nome,Cpf,Senha,Telefone, Saldo)).

list_clientes(C) :- 
  findall([Nome,Cpf,Senha,Telefone, Saldo], cliente(Nome,Cpf,Senha,Telefone, Saldo), C).

remove_cliente :- 
  nl,
  writeln("Insira o CPF da conta a ser excluida: "),
  read_line_to_string(user_input, Cpf),
    list_clientes(C),
    retractall(cliente(_,_,_,_,_)),
    remove_cliente_aux(C, Cpf, C_att),
    add_clientes(C_att),
    tell('./data/bd_clientes.pl'), nl,
    listing(cliente/5),
    told, nl,
    fimMetodoAdm.

remove_cliente_apos_operacao(Cpf) :- 
    list_clientes(C),
    retractall(cliente(_,_,_,_,_)),
    remove_cliente_aux(C, Cpf, C_att),
    add_clientes(C_att),
    tell('./data/bd_clientes.pl'), nl,
    listing(cliente/5),
    told, nl.

remove_cliente_aux([],_,[]) :-
  nl,
  writeln("Usuario inexistente"), nl.
remove_cliente_aux([H|T], Cpf, T) :- member(Cpf, H).
remove_cliente_aux([H|T], Cpf, [H|Out]) :- remove_cliente_aux(T, Cpf, Out).


fimMetodoAdm:-
  writeln("Clique em enter para continuar: "),
  read_line_to_string(user_input, _).
  
editar_contato_gerente :-
  setup_bd_login,
  writeln("Confirme o CPF do gerente: "),
  read_line_to_string(user_input, Cpf),
  writeln("Confirme a senha do gerente: "),
  read_line_to_string(user_input, Senha),

  (gerente(Cpf, Senha, _) -> nl, 
    writeln("Insira o numero do contato a ser atualizado: "),
    read_line_to_string(user_input, Contato), nl, 
    retract(gerente(Cpf, Senha, _)),
    assert(gerente(Cpf,Senha,Contato)), 
    tell('./data/bd_adm.pl'), nl,
    listing(gerente/3),
    told, 
    writeln("Contato atualizado com sucesso."),
    writeln("Pressione qualquer tecla para retornar ao menu..."),
    read_line_to_string(user_input, _),
  writeln("Senha incorreta."), nl, false).
  

