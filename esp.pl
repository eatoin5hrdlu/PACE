:- use_module(library(socket)).
:- dynamic connection/4.

debug.

wifi(milton, staMAC('18:fe:34:f3:00:75'):null, '192.168.254.43').
wifi(milton, staMAC('18:fe:34:f3:00:64'):null, '192.168.254.30').
wifi(milton, staMAC('18:fe:34:f3:44:28'):apMAC('1a:fe:34:f3:44:28'),'192.168.254.25').


:- dynamic devices/1.

reset_devices :-
    retractall(devices(_)),
    assert(devices(0)).

add_dev(N) :-
    retract(devices(D)),
    N is D + 1,
    assert(devices(N)).

    
connect(AP,IP) :-
	wifi(AP,_Mac,IP),
	tcp_socket(S),
	tcp_connect(S, IP:23),
	tcp_open_socket(S, SP),
	stream_pair(SP,Read,Write),
	set_stream(Read,buffer(false)),
	set_stream(Write,buffer(false)),
	( retract(connection(N,IP,R,W)) % Replacement
	 -> close(R),
	    close(W)
	 ;  add_dev(N)                  % New
	),
	assert(connection(N,IP,Read,Write)).


tget0(S,C) :-
    catch( call_with_time_limit(20,get_code(S, C)),
	   E,
	   (writeln(E),fail)),!.
    
random_connection(Random) :-
    findall(connection(N,IP,R,W), connection(N,IP,R,W), Cs),
    random_member(Random,Cs),
    !.

readtoken(-1,R,[]) :- % EndOfFile on a TCP Stream!! Oh No!
    !,
    writeln(attemptReEstablishBrokenTCPConnection(IP)),
    connection(_N,IP,R,W),
    closeConnection(R,W,IP),
    connect(_AP,IP),
    fail.

readtoken(10,_,[]) :- !.

readtoken(13,S, T) :- !,
    tget0(S, C2),
    readtoken(C2,S,T).

readtoken(C,S,[C|T]) :-
    tget0(S, C2),
%    writeln(C2),
%    flush_output,
    readtoken(C2,S,T).

connect :-
    reset_devices,
    connect(milton,IP),
    writeln(connected(IP)),
    fail.
connect.

 main :-
    devices(Num),
    writeln(devices(Num)),
    send_set.

disp(Literal) :- debug, !, write(Literal),write(' '),flush_output.
disp(_).

send_set :-
    repeat,
    member(A,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    member(B,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    atom_codes(Msg,[A,B,0'9,0'0,0'1]),
    random_connection(connection(N,_IP,Read,Write)),
    write(Write,Msg),
    write(Write,'\r\n'),
    flush_output(Write),
    disp(sent(N,Msg)),
    tget0(Read, C),
    readtoken(C,Read,Cs),
    atom_codes(Token,Cs),
    disp(got(N,Token)),
    sleep(1),
    fail.

send_set :- disconnect.

send_set :- halt.

disconnect :-
    reset_devices,
    connection(_N,IP,R,W),
    closeConnection(R,W,IP),
    fail.

closeConnection(R,W,IP) :-
    retract(connection(_N,IP,R,W)),
    close(R,[force(true)]),
    close(W,[force(true)]),
    writeln('done with'(IP)).


    
    
