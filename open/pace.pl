:- use_module(library(charsio)).
pace(_Request) :-
        Title = 'PACE Control Panel',
        turbidostats(List),
	length(List, Length),
	size(Length, Font, Cols, Dsp, Dpd, Esp, Epd),
	style(List, Style),
	findall(th([class=N],Print),
	        ( member(N,List), initial_upcase(N,Print)),Header),
	findall(td([class=N], [V,&(nbsp),sup(&(deg)),'C']),
	        ( member(N,List),measurement(N,temperature,V)), Temps),
	findall(td([class=N],V),
	        ( member(N,List),measurement(N,turbidity,V)), Turbs),
	findall(td(textarea([rows=8,cols=Cols,name=Name],'x\ny\nz')),
	        member(Name,List), Edits),
	reply_html_page( biologic,
	        [ title(Title),
		  meta(['http-equiv'(refresh),content(5)],[]),
                  script([ language(javascript) ],[]),
		  Style ],
		  body(id(biologic),
                   center( font([size=Font],
			       [ h1([],Title),
                                 table([border(1),cellspacing(Dsp),cellpadding(Dpd)],
				       [tr(Header),tr(Temps),tr(Turbs)] ),
		                 table([border(1),cellspacing(Esp),cellpadding(Epd)],
					[tr(Edits)])
                               ]
                               )
                         ))).

pace(Request) :-
	errorPage(Request, 'Error creating PACE control page').

% size(Num, Font, Cols, Dsp, Dpd, Esp, Epd)
size(N, '+4', 27, 12, 22, 14, 5) :- N < 5.
size(N, '+3', 17, 18, 11, 9, 2 ) :- N > 4.

% Style 'light-grey' entries which are not online/1.
style(List, style([], [Style])) :-
        append("td  {text-align:center }\n",Rest,StyleCodes),
	style_codes(List,Rest,[]),
	atom_codes(Style, StyleCodes).

style_codes([])     --> [].
style_codes([N|Ns]) --> { online(N), ! },
	                style_codes(Ns).
style_codes([N|Ns]) --> ".", atom_to_chars(N)," {color:#DDDDDD}\n",
	                style_codes(Ns).

initial_upcase(LC, Name) :-
	atom_codes(LC,[C|Cs]),
	to_upper(C, UC),
	atom_codes(Name,[UC|Cs]).

