page_title(status, 'Contents of the Request header').

status(Request)    :- 
    message('handling status~n',[]),
    page_title( status, T ),
    phrase(status(Request), Body),
    reply_html_page( biologic, [ title(T) ],
		     h3(table(border(4),Body))).

status([])    --> [].
status([H|T])  -->
        { atomic(H) },
        !,
	[ tr([td(H),td(H)]) ],
	status(T).
status([H|T]) -->
	{ H =.. [Name, Value|_], translate(Value,Display) },
	[ tr([td(Name),td(Display)]) ],
	status(T).

%
% Some of the Value objects do not have html_text
% conversion rules we have to reduce them to atoms.
%

translate(Blob,      stream) :- blob(Blob, stream),!.
translate([_|_],       list).
translate(ip(_,_,_,D),    D).
translate(client(A,_,_,_),A).
translate(T,              T).

