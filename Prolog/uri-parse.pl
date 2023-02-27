%%% -*- Mode: Prolog -*-
%%% Pasta Roberto 869247

uri_parse(URIString,
          uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    string_to_list(URIString, URIList),
    initial_control(URIList,
                    uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)).

initial_control([H | T],
                uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    is_identificatore(H),
    accept([H | T], q0, [],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]).

accept([], S, _,
       [zos, _, _, _, _, _, _]) :-
    statofinalezos(S),
    !,
    fail.

%quando c'è solo Scheme accetta sempre
accept([], S, [],
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    S \= q32,
    S \= q3,
    speciale(Scheme),
    atom(Scheme),
    var(Userinfo),
    var(Host),
    var(Port),
    var(Query),
    var(Fragment),
    var(Path),
    controlla(Userinfo),
    controlla(Host),
    controlla(Port),
    controlla(Path),
    controlla(Query),
    controlla(Fragment),!.
accept([], S, [],
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    S \= q32,
    S \= q3,
    atom(Scheme),
    var(Userinfo),
    var(Host),
    var(Port),
    var(Query),
    var(Fragment),
    var(Path),
    controlla(Userinfo),
    controlla(Host),
    controllaport(Port),
    controlla(Path),
    controlla(Query),
    controlla(Fragment),!.

accept([], q7, _,
       [_, _, _, Port, _, _, _]) :-
    number(Port),
    Port \= 80,
    !,
    fail.

accept([], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], X),
    errore(X),
    !,
    fail.

accept([], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], _),
    final(S),
    speciale(Scheme),
    !,
    controlla(Userinfo),
    controlla(Host),
    controlla(Port),
    controlla(Path),
    controlla(Query),
    controlla(Fragment),
    !.
accept([], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], _),
    final(S),
    !,
    controlla(Userinfo),
    controlla(Host),
    controllaport(Port),
    controlla(Path),
    controlla(Query),
    controlla(Fragment),
    !.

accept([], S, [],
       [_, Userinfo, Host, Port, Path, Query, Fragment]) :-
    final(S),
    !,
    controlla(Userinfo),
    controlla(Host),
    controllaport(Port),
    controlla(Path),
    controlla(Query),
    controlla(Fragment),
    !.
    %ZOS
accept([H | T], q31, _,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(q31, H, S1),
    accept(T, S1, [],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]), !.

accept([H | T], q32, _,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(q32, H, S1),
    accept(T, S1, [],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]), !.

accept([H | T], q32, _,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    is_letter(H),
    contazos([H | T], q32, 0, [],
             [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
    %

accept([H | T], S, Pila,
       [zos, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(S, H, q32),
    nome(S, Pila,
         [zos, Userinfo, Host, Port, Path, Query, Fragment], _),
    !,
    accept(T, q32, [],
           [zos, Userinfo, Host, Port, Path, Query, Fragment]), !.

accept([H | T], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(S, H, S1),
    S = S1,
    accept(T, S1, [H | Pila],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]), !.

 %se HOST c'è, ma PORT non è assegnata
accept(_, q33, _,
       [_, _, _, Port, _, _, _]) :-
    number(Port),
    Port \= 80,
    !,
    fail.
%se HOST c'è, ma PORT non è assegnata
accept([H | T], q33, Pila,
       [zos, Userinfo, Host, 80, Path, Query, Fragment]) :-
    is_letter(H),
    accept([H | T], q32, Pila,
           [zos, Userinfo, Host, 80, Path, Query, Fragment]), !.
 %se HOST c'è, ma PORT non è assegnata
accept([H | T], q33, Pila,
       [Scheme, Userinfo, Host, 80, Path, Query, Fragment]) :-
    is_identificatore(H),
    accept([H | T], q11, Pila,
           [Scheme, Userinfo, Host, 80, Path, Query, Fragment]), !.
%se HOST c'è, ma PORT non è assegnata
accept([H | T], q33, Pila,
       [Scheme, Userinfo, Host, 80, Path, Query, Fragment]) :-
    delta(q33, H, S1),
    accept(T, S1, Pila,
           [Scheme, Userinfo, Host, 80, Path, Query, Fragment]), !.

accept([H | T], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], _),
    speciale(S, Scheme, S2, H),
    !,
    accept(T, S2, [],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]), !.

accept([H | _], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(S, H, S1),
    S \= S1,
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], X),
    errore(X),
    !,
    fail.

accept([H | T], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(S, H, S1),
    S \= S1,
    nome(S, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment], _),
    accept(T, S1, [],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.

accept([H | _], S, _,
       [_, _, _, _, _, _, _]) :-
    delta(S, H, S1),
    S = q7,
    S1 = q11,
    !,
    fail.

accept([H | T], S, Pila,
       [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(S, H, S1),
    S \= S1,
    accept(T, S1, [H | Pila],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]).

errore(X) :- X = f.

is_letter(X) :-
    X =< 122,
    X >= 97,
    !.
is_letter(X) :-
    X =< 90,
    X >= 65.
isdigit(X) :-
    X =< 57,
    X >= 48.
is_identificatore(X) :-
    X \= 47,
    X \= 63,
    X \= 35,
    X \= 64,
    X \= 58.
is_identificatore_host(X) :-
    X \= 47,
    X \= 63,
    X \= 35,
    X \= 64,
    X \= 46,
    X \= 58.

delta(q0, H, q0) :- is_identificatore(H).
delta(q0, H, q1) :- H = 58.    %vai a authority

delta(q1, H, q2) :- H = 47.    %attendi una / vai a authority
delta(q1, H, q11) :- H = 47.    %attendi una / vai a path
delta(q1, H, q14) :- H = 47.

delta(q2, H, q3) :- H = 47.    %vai a host

delta(q3, H, q4) :- is_identificatore(H).
delta(q3, H, q6) :- is_identificatore_host(H).
delta(q3, H, q7) :- is_identificatore_host(H).

delta(q4, H, q4) :- is_identificatore(H).
delta(q4, H, q6) :- H = 64.    %vai a host

delta(q5, H, q6) :- is_identificatore_host(H).
delta(q5, H, q7) :- is_identificatore_host(H).

delta(q6, H, q6) :- is_identificatore_host(H).
delta(q6, H, q5) :- H = 46.
delta(q6, H, q7) :- is_identificatore_host(H).

delta(q7, H, q8) :- H = 58.    %arrivato :
delta(q7, H, q33) :- H = 47.   %porta 80 default

%delta(q33, H, q32) :- is_letter(H).   %vai a ZOS
%delta(q33, H, q11) :- is_identificatore(H).   %vai a path
delta(q33, H, q15) :- H = 63.   %vai a query
delta(q33, H, q17) :- H = 35.   %vai a fragment

delta(q8, H, q9) :- isdigit(H). %controlla se c'è un numero e vai a porta

delta(q9, H, q9) :- isdigit(H).
delta(q9, H, q11) :- H = 47.  %arrivato /
delta(q9, H, q14) :- H = 47.  %arrivato /
delta(q9, H, q32) :- H = 47.   %vai a ZOS

delta(q10, H, q12) :- is_identificatore(H).

delta(q11, H, q12) :- is_identificatore(H).
delta(q11, H, q13) :- is_identificatore(H).

delta(q12, H, q12) :- is_identificatore(H).
delta(q12, H, q11) :- H = 47.
delta(q12, H, q13) :- is_identificatore(H).

delta(q13, H, q15) :- H = 63.  %arrivato ?
delta(q13, H, q17) :- H = 35.  %arrivato #

delta(q14, H, q15) :- H = 63.  %arrivato ?
delta(q14, H, q17) :- H = 35.  %arrivato #

delta(q15, H, q16) :- H \= 35.

delta(q16, H, q16) :- H \= 35.
delta(q16, H, q17) :- H = 35.  %arrivato #

delta(q17, _, q18).

delta(q18, _, q18).

%MAILTO
delta(q19, H, q20) :- is_identificatore(H).

delta(q20, H, q20) :- is_identificatore(H).
delta(q20, H, q21) :- H = 64.

delta(q21, H, q22) :- is_identificatore_host(H).
delta(q21, H, q24) :- is_identificatore_host(H).

delta(q22, H, q22) :- is_identificatore_host(H).
delta(q22, H, q23) :- H = 46.
delta(q22, H, q24) :- is_identificatore_host(H).

delta(q23, H, q22) :- is_identificatore_host(H).
delta(q23, H, q24) :- is_identificatore_host(H).

%NEWS
delta(q25, H, q26) :- is_identificatore_host(H).
delta(q25, H, q28) :- is_identificatore_host(H).

delta(q26, H, q26) :- is_identificatore_host(H).
delta(q26, H, q27) :- H = 46.
delta(q26, H, q28) :- is_identificatore_host(H).

delta(q27, H, q26) :- is_identificatore_host(H).
delta(q27, H, q28) :- is_identificatore_host(H).

%TEL E FAX
delta(q29, H, q30) :- is_identificatore(H).

delta(q30, H, q30) :- is_identificatore(H).

%ZOS
delta(q31, H, q32) :- H = 47.
delta(q32, H, q13) :- H = 35.
delta(q32, H, q13) :- H = 63.
delta(q32, H, q3) :- H = 47.


nome(q0, Pila, [Scheme, _, _, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Scheme).
nome(q0, Pila, [Scheme, _, _, _, _, _, _], f) :-
    atom(Scheme),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Scheme.
nome(q1, _, [_, _, _, _, _, _, _], a).
nome(q2, _, [_, _, _, _, _, _, _], a).
nome(q4, Pila, [_, Userinfo, _, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Userinfo).
nome(q4, Pila, [_, Userinfo, _, _, _, _, _], f) :-
    atom(Userinfo),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Userinfo.
nome(q7, Pila, [_, _, Host, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Host).
nome(q7, Pila, [_, _, Host, _, _, _, _], f) :-
    atom(Host),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Host.
nome(q9, Pila, [_, _, _, Port, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    number_string(Port, String).
nome(q9, Pila, [_, _, _, Port, _, _, _], f) :-
    number(Port),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    number_string(X, String),
    X \= Port.
nome(q13, Pila, [_, _, _, _, Path, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Path).
nome(q13, Pila, [_, _, _, _, Path, _, _], f) :-
    atom(Path),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Path.
nome(q14, _, [_, _, _, _, _, _, _], a).
nome(q16, Pila, [_, _, _, _, _, Query, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Query).
nome(q16, Pila, [_, _, _, _, _, Query, _], f) :-
    atom(Query),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Query.
nome(q18, Pila, [_, _, _, _, _, _, Fragment], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Fragment).
nome(q18, Pila, [_, _, _, _, _, _, Fragment], f) :-
    atom(Fragment),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Fragment.
nome(q20, Pila, [_, Userinfo, _, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Userinfo).
nome(q20, Pila, [_, Userinfo, _, _, _, _, _], f) :-
    atom(Userinfo),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Userinfo.
nome(q24, Pila, [_, _, Host, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Host).
nome(q24, Pila, [_, _, Host, _, _, _, _], f) :-
    atom(Host),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Host.
nome(q28, Pila, [_, _, Host, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Host).
nome(q28, Pila, [_, _, Host, _, _, _, _], f) :-
    atom(Host),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Host.
nome(q30, Pila, [_, Userinfo, _, _, _, _, _], a) :-
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, Userinfo).
nome(q30, Pila, [_, Userinfo, _, _, _, _, _], f) :-
    atom(Userinfo),
    reverse(Pila, NewPila),
    string_to_list(String, NewPila),
    string_to_atom(String, X),
    X \= Userinfo.
nome(q33, _, [_, _, _, _, _, _, _], a).

final(q1).
final(q7).
final(q9).
final(q13).
final(q16).
final(q18).
final(q20).
final(q24).
final(q28).
final(q30).
final(q33).
final(q11).

statofinalezos(q7).
statofinalezos(q9).

speciale(q0, X, q19, H) :-
    H = 58,
    downcase_atom(X, mailto).
speciale(q0, X, q25, H) :-
    H = 58,
    downcase_atom(X, news).
speciale(q0, X, q29, H) :-
    H = 58,
    downcase_atom(X, tel).
speciale(q0, X, q29, H) :-
    H = 58,
    downcase_atom(X, fax).
speciale(q0, X, q31, H) :-
    H = 58,
    downcase_atom(X, zos).
speciale(q7, X, q32, H) :-
    H = 47,
    downcase_atom(X, zos).
speciale(q7, X, q8, H) :-
    H = 58,
    downcase_atom(X, zos).
speciale(q9, X, q32, H) :-
    H = 58,
    downcase_atom(X, zos).

speciale(X) :- downcase_atom(X, mailto), !.
speciale(X) :- downcase_atom(X, news), !.
speciale(X) :- downcase_atom(X, tel), !.
speciale(X) :- downcase_atom(X, fax), !.

contazos([], q32, _, [P1 | Pila],
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    P1 \= 46,
    accept([], q13, [P1 | Pila],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
contazos([H | T], q32, _, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    delta(q32, H, q13),
    accept([H | T], q13, Pila,
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
contazos([H | T], q32, N, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    N1 is N + 1,
    carattere_zos(H),
    N1 < 45,
    !,
    contazos(T, q32, N1, [H | Pila],
             [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
contazos([H | [T1 | T]], q32, N, [P1 | Pila],
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    N < 45,
    H = 40,
    P1 \= 46,
    is_letter(T1),
    contazos([T1 | T], q33, 0, [H, P1 | Pila],
             [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
contazos([], q33, N, [P1 | Pila],
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    P1 = 41,
    N < 9,
    accept([], q13, [P1 | Pila],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.

contazos([H | T], q33, N, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    N1 is N + 1,
    carattere_zos(H),
    N1 < 9,
    !,
    contazos(T, q33, N1, [H | Pila],
             [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.
contazos([H | T], q33, N, Pila,
         [Scheme, Userinfo, Host, Port, Path, Query, Fragment]) :-
    H = 41,
    N < 9,
    !,
    accept(T, q13, [H | Pila],
           [Scheme, Userinfo, Host, Port, Path, Query, Fragment]),
    !.

carattere_zos(H) :- is_letter(H).
carattere_zos(H) :- isdigit(H).
carattere_zos(H) :- H = 46.

uri_display(URIString) :-
    uri_parse(URIString,
              uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)),
    uri_print("Scheme:   ", Scheme),
    uri_print("\nUserinfo: ", Userinfo),
    uri_print("\nHost:     ", Host),
    uri_printport("\nPort:     ", Port),
    uri_print("\nPath:     ", Path),
    uri_print("\nQuery:    ", Query),
    uri_print("\nFragment: ", Fragment).

uri_print(String, Var) :-
    atom(Var),
    !,
    write(String),
    write(Var).
uri_print(String, _) :-
    write(String),
    write("NIL").
uri_printport(String, Var) :-
    number(Var),
    !,
    write(String),
    write(Var).
uri_printport(String, _) :-
    write(String),
    write("NIL").


uri_display(URIString, File) :-
    open(File, write, Out),
    uri_parse(URIString,
              uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)),
    uri_print_out("Scheme:   ", Scheme, Out),
    uri_print_out("\nUserinfo: ", Userinfo, Out),
    uri_print_out("\nHost:     ", Host, Out),
    uri_print_port("\nPort:     ", Port, Out),
    uri_print_out("\nPath:     ", Path, Out),
    uri_print_out("\nQuery:    ", Query, Out),
    uri_print_out("\nFragment: ", Fragment, Out),
    close(Out).

uri_print_out(String, Var, Out) :-
    atom(Var),
    !,
    write(Out, String),
    write(Out, Var).
uri_print_out(String, _, Out) :-
    write(Out, String),
    write(Out, "NIL").
uri_print_port(String, Var, Out) :-
    number(Var),
    !,
    write(Out, String),
    write(Out, Var).
uri_print_port(String, _, Out) :-
    write(Out, String),
    write(Out, "NIL").

controlla(X) :-
    var(X),
    X = [].
controlla(X) :-
    atom(X).
controlla([]).
controllaport(X) :-
    var(X),
    X = 80.
controllaport(X) :-
    number(X).
