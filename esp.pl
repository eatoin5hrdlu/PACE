:- use_module(library(socket)).
:- dynamic connection/4, finish/0.
:- [secrets].

debug.     % Remove this to silence output
disp(D) :- (debug -> writeln(D) ; true).

devices(N)  :- findall(x,connection(_,_,_,_),Xs), length(Xs,N).
devNum(N)  :- devices(M), N is M + 1.
inputs(Is) :- findall(R,connection(_,_,R,_),Is).
nip(Nips)  :- findall(N:I,connection(N,I,_,_),Nips).

disconnect :- findall(closed(IP),disconnect(IP),Report),
	      disp(Report).

disconnect(IP,N) :- retract(connection(N,IP,R,W)),
		    write(R,'xx-1\r\n'), % tell remote to close connection
		    sleep(1),
		    close(R,[force(true)]),
		    close(W,[force(true)]).

connect :-  findall(opened(IP),connect(IP),Report),
	    disp(Report).

connect(IP) :-
        ssid(AP),
	ip_ap_mac(IP, AP, _MAC),   % Generator
	trace,
	( disconnect(IP,N) -> true ; devNum(N) ),
	tcp_socket(S),
	catch(tcp_connect(S, IP:23),Ex,(writeln(e(Ex)),fail)),
	tcp_open_socket(S, SP),
	stream_pair(SP,Read,Write),
	set_stream(Read,buffer(false)),
	set_stream(Write,buffer(false)),
	assert(connection(N,IP,Read,Write)),
        disp(connected(N,IP)).

replies(Replies, Timeout) :-
    findall(R,connection(_,_,R,_),Inputs),
    wait_for_input(Inputs,Ready,Timeout),
    maplist(read_token, Ready, Replies).

read_token(S, reply(N,Token) ) :-
    catch(read_line_to_codes(S, Cs),Ex,(writeln(Ex),fail)),
    connection(N,_,S,_),
    atom_codes(Token,Cs).

random_connection(Random) :-
    findall(connection(N,IP,R,W), connection(N,IP,R,W), Cs),
    random_member(Random,Cs),
    !.

% TESTING
start :- start(S,R),writeln(start(S,R)).
start(S,R) :-
    connect,
    thread_create(receivingThread,R,[]),
    writeln('receiving...'),
    thread_create(sendingThread,S,[]),
    writeln('sending...'),
    repeat,
    sleep(10),
    fail.


% Returns input from any of the active connections
receivingThread :-
    repeat, 
    replies(Tokens,20),
    disp('        REPLY'(Tokens)),
    fail.

    
sendingThread :-
    repeat,
    member(A,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    member(B,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    ( finish
     -> disconnect,
	retract(finish)
     ;
     atom_codes(Msg,[A,B,0'9,0'0,0'0,0'0]),
     random_connection(connection(N,_,_R,Write)),
     write(Write,Msg),
     write(Write,'\r\n'),
     flush_output(Write),
     disp(sent(N,Msg)),
     sleep(2),
     fail
    ).
