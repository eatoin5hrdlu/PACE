:- use_module(library(socket)).


readtoken(10,_,[]) :- !.
readtoken(13,S, T) :- !,
    get0(S,C2),
    readtoken(C2,S,T).

readtoken(C,S,[C|T]) :-
    get0(S,C2),
    writeln(C2),
    flush_output,
    readtoken(C2,S,T).

main :-
    tcp_socket(S),
    tcp_connect(S, '172.16.3.119':23),
    tcp_open_socket(S, SP),
    stream_pair(SP,Read,Write),
    set_stream(Read,buffer(false)),
    set_stream(Write,buffer(false)),
    keepsending(Read,Write),
    close(SP,[force(true)]).


keepsending(R,W) :- send_set(R,W), keepsending(R,W).    

send_set(Read,Write) :-
    member(A,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    member(B,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    atom_codes(Msg,[A,B,0'9,0'0,0'1]),
    write(Write,Msg),
    write(Write,'\r\n'),
    flush_output(Write),
    writeln(sent(Msg)),
    get0(Read, C),
    readtoken(C,Read,Cs),
    atom_codes(Token,Cs),
    writeln(got(Token)),
    sleep(5),
    fail.
send_set(_,_) :- writeln(once).
    
    
