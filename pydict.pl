
config( [
	 name(template),                    % e.g.  name(darwin)
	 numLagoons(4),
	 lagoonRegion(290,240,400,550),  % Area containing lagoons
	 lagoonHeight(60),    % divisor for levelScale
	 lagoonWidth(20),
	 levelScale(100),   % 100 gives level as percentage of lagoonHeight
	 camera(indoor),
	 rotate(false),
	 mac('c4:d6:55:34:8d:07'),  % belongs in snapshot
	 defaultIP('172.16.3.136'),  % belongs in snapshot
	 userpwd('&user=scrapsec&pwd=lakewould'),
	 brightness(100), % 0-240 for indoor camera
	 brightnessCmd('/camera_control.cgi?param=1&value='),
	 contrast(4),
	 contrastCmd('/camera_control.cgi?param=2&value='),
	 picCmd('/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'),
	 layout([
		 screen(680, 900, point(50,0)),
		 cellstat(cellstat,below,[od(0.4),temp(37.0), shape(240,60),font(font(times,roman,18))]),
		 % pumps( pumprail, next_row,   [  mac('98:D3:31:70:2B:70')]),
		 pumps( pumprail, next_row,   [  ]),
		 spacer(        x1, next_row, [color(blue)]),
		 snapshot(     cam, next_row, [ shape(650,420),image('mypic1.jpg')]),
		 spacer(        x2, next_row, []),
		 lagoon( lagoon1, next_row, [temp(35.0), LS, LF]),
		 lagoon( lagoon2, right,    [temp(35.0), LS, LF]),
		 lagoon( lagoon3, right,    [temp(35.0), LS, LF]),
		 lagoon( lagoon4, right,    [temp(35.0), LS, LF]),
		 spacer(        x3, next_row, [color(darkgreen)]),
		 sampler(autosampler, next_row, [ shape(400,30),font(font(times,roman,20)) ])])
	 ]) :-
 LS = shape(142,60),
 LF = font(font(times,roman,14)).

main :-
	config(List),
	member(name(Name),List),
	concat_atom([Name,'.settings'],File),
	pl2py(List,PythonDict,[]),
	flatten(PythonDict,Flat,[]),
	concat_atom(Flat,PyAtom),
	tell(File),
trace,
	write('config = {\n     '),
	writeln(PyAtom),
	writeln('}'),
	told.

flatten([])    --> !,  [].
flatten([H|T]) --> !, flatten(H), flatten(T).
flatten(H)     --> [H].

quote_atom([A],Q) :- quote_atom(A,Q).
quote_atom(N,N)   :- number(N).
quote_atom(A,Q)   :- atom(A), concat_atom(['''',A,''''],Q).

quote_atoms([],[], _).
quote_atoms([A],[Q],_) :- quote_atom(A,Q).
quote_atoms([A|T],[Q,Spacer|QT],Spacer) :- quote_atom(A,Q), quote_atoms(T,QT,Spacer).

indent      --> ['     '].

pl2py([])         --> !, [].
pl2py(layout(_))  --> !, [].
pl2py(A)          --> { quote_atom(A,Q)} , !, [Q].
pl2py([H])        --> !, pl2py(H), [' \n'].
pl2py([H|T])      --> !, pl2py(H), indent, pl2py(T).

pl2py(Term) --> { Term =.. [F|[A]],
	          quote_atom(F,QF),
	          quote_atom(A,QA),
		  !
                },
		[QF, ' :  ', QA, ',\n'].
pl2py(Term) --> { Term =.. [F|[A|As]],
	          quote_atom(F,QF),
	          quote_atoms([A|As], Str, ','),
                  !
                },
	        [QF,' : (', Str, '),\n' ].

pl2py(Term) --> { Term =.. [F|Args],
	          quote_atom(F, QF)
	        },
		[QF, ' :  {'],
		pl2py(Args),
		['      }'].
                 

	





