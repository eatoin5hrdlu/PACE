:- multifile debug/0.
:- dynamic debug/0.

die(_Request) :-
        (debug
	 -> retractall(debug), Value = ' Off '
	 ;  assert(debug), Value = ' On '
	),
	reply_html_page(title('Toggle Debugging'),
			[center([h1('Toggling Debugging'),
				 h2(['Debugging is ',Value])])]).
