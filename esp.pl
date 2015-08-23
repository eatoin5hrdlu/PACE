:- use_module(library(socket)).
:- dynamic connection/4, finish/0, devices/1.
:- [secrets].

debug.     % Remove this to silence output
disp(D) :- (debug -> writeln(D) ; true).

reset_devices :-
    retractall(devices(_)),
    assert(devices(0)).

add_dev(N) :-
    retract(devices(D)),
    N is D + 1,
    assert(devices(N)).

disconnect :- reset_devices,
	      findall(closed(IP),disconnect(IP),Report),
	      disp(Report).

disconnect(IP) :- retract(connection(_N,IP,R,W)),
		  write(R,'xx-1\r\n'), % tell remote to close connection
		  sleep(1),
		  close(R,[force(true)]),
		  close(W,[force(true)]).

connect :-  reset_devices,
	    findall(opened(IP),connect(IP),Report),
	    disp(Report).

connect(IP) :-
        ssid(AP),
	ip_ap_mac(IP, AP, _MAC),   % Generator
	( disconnect(IP) -> true ; add_dev(N) ),
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

%( Ready = []  % Nobody's sent anything
%     -> Replies = [timeout(Timeout)]
%     ;  maplist(read_token, Ready, Replies)
%    ).

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
    devices(Num),
    writeln(devices(Num)),
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
