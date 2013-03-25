
evolution(_Request) :-
    controls([collector:1,turbidostat:4], Controls, []),
    Title = 'PACE Apparatus Control Panel',
    reply_html_page( biologic,
		[ title(Title),
		  script([ language(javascript) ],[]),
		  style([],[ 'td  { text-align : center }']) ],
		center([h1([],Title)|Controls])).

evolution(Request) :-
    errorPage(Request, 'Error creating PACE control page').
  
controls(  [] ) --> [ h2('End of Controls') ].
controls([H|T]) --> controls(H), controls(T).
controls( C:N ) --> { N>0 }, controls(C,1,N).
controls( C:1 ) --> !, control(C, 1).

controls(C,N,N) --> !, control(C, N).
controls(C,N,M) -->    control(C,N), { NN is N+1 }, controls(C,NN,M).

control( turbidostat, X) -->
    [ hr([]),h1([],['Turbidostat ',X]),
      table([border(1),cellspacing(4),cellpadding(4)],
	    [tr( [ th('Turbidity'),
	           th('Threshold'),
		   th(['Current Temperature ',sup(&(deg)),'C']),
		   th(['Target Temperature  ',sup(&(deg)),'C'])]),
	     tr( [ td([555,i(' (0-1023) ')]),
	           td(600),
		   td(37),
		   td(37) ] )
	    ]) 
    ].

control( collector, X) -->
    [ hr([]),h1([],['Collector ',X]),hr([]),
      table([border(1),cellspacing(4),cellpadding(4)],
	    [tr( [ th('Samples'),
		   th('Schedule (minutes)'),
		   th(['Temperature ',sup(&(deg)),'C'])]),
	     tr( [ td(16),
	           td(120),
		   td(10) ] )] )
    ].


